#' visdat_
#'
#' Internal function. Prepares data for a choropleth leaflet map.
#'
#' @param dat tibble.
#' @param level either 'us_county' (contiguous us map on the level of counties), or 'world' (on the level of countries).
#'
#' @return a list object that can be passed to leaflet()
#'
#'@export
#' @examples #
visdat_=function(dat, level='us_county', vars='n'){

  require(tidyverse)

  if(level=='us_county'){
    if(is.null(dat) || (nrow(dat)==0)) dat=us_counties%>%mutate(n=0)
    visdat=us_county_foundation_map
  }else if(level=='world'){
    if(is.null(dat) || (nrow(dat)==0)) dat=country_name_alphacode%>%select(COUNTRY=COUNTRY_2, COUNTRY_3, country_name)%>%mutate(n=0)
    visdat=world_foundation_map
  }

  dat=dat%>%mutate_at(vars, function(x) na_if(x,0))

  for(x in names(dat)){
    visdat[[x]]=dat%>%pull(x)
  }

  return(visdat)
}

#' map_us
#'
#'Choropleth map of contiguous US on county level
#'
#' @param dat tibble, containing polyname (in the format of polyname in package "maps", e.g. "new york,new york"), numeric variable(s) to be plotted, and label (to be shown in hoverlabel)
#' @param return_df #Boolean, whether to return table of results in addition to a plot (as a list)
#' @param vars vector of variables to include, possibly named. If more than one, will add layer control, using vector names.
#' @param map_provider provider of maptiles (see leaflet documentation). NULL will use leaflet default.
#'
#' @return leaflet object, or a list of a leaflet object and a tibble, depending on return_df
#' @export
#'
#' @examples
map_us=function(dat, return_df=FALSE, vars='n', map_provider="CartoDB.Positron"){

  require(tidyverse)
  require(leaflet)

  if(is.null(names(vars))) names(vars)=vars

  if(!'label' %in% names(dat)) dat$label=''

  dat=dat%>%right_join(

    us_counties,
    by='polyname'

  )%>%
    arrange(ind)%>%
    select(-ind)%>%
    mutate_at(vars, replace_na, replace=0)%>%
    visdat_(vars=vars)

  fig=dat%>%
    leaflet()

  if(is.null(map_provider)){
    fig=fig%>%addTiles()
  }else{
    fig=fig%>%addProviderTiles(map_provider)
  }

  for(i in seq_along(vars)){
    fig=fig%>%
      addPolygons(
      color=colorRampI(dat[[vars[i]]], na=0),
      stroke=FALSE,
      smoothFactor = 0,
      fillOpacity=0.5,
      label=~lapply(paste0(STATE,', ',COUNTYNAME,'<br>',label),htmltools::HTML),
      group=names(vars)[i]
    )
  }

  if(length(vars)>1){
    fig=fig%>%
      addLayersControl(baseGroups = names(vars),
                       options=layersControlOptions(collapsed=FALSE)
      )
  }

  if(return_df){

    dat=dat[c('STATE','COUNTYNAME',vars)]%>%
      as_tibble()%>%
      distinct()
    return(list(fig=fig, dat=dat))
  }

  return(fig)

}

#' map_world
#'
#'Choropleth map of the world on country level
#'
#' @param dat tibble, containing country (2-letter country codes, or 3-letter codes, depending on country_nchar), numeric variable(s) to be plotted and label (to be shown in hoverlabel)
#' @param return_df #Boolean, whether to return table of results in addition to a plot (as a list)
#' @param vars vector of variables to include, possibly named. If more than one, will add layer control, using vector names.
#' @param country_nchar 2 or 3, how to interpret the country values in dat: as 2- or 3-letter codes
#' @param map_provider provider of maptiles (see leaflet documentation). NULL will use leaflet default.
#'
#' @return leaflet object, or a list of a leaflet object and a tibble, depending on return_df
#' @export
#'
#' @examples
map_world=function(dat, return_df=FALSE, vars='n', country_nchar=2, map_provider="CartoDB.Positron"){

  require(tidyverse)
  require(leaflet)

  if(is.null(names(vars))) names(vars)=vars

  if(!'label' %in% names(dat)) dat$label=''

  if(country_nchar==2){
    dat=dat%>%rename(COUNTRY_2=country)%>%
      right_join(country_name_alphacode, by='COUNTRY_2')
  }else{
    dat=dat%>%rename(COUNTRY_3=country)%>%
      right_join(country_name_alphacode, by='COUNTRY_3')
  }

  dat=dat%>%
    rename(COUNTRY=COUNTRY_2)%>%
    arrange(ind)%>%
    select(-ind)%>%
    mutate_at(vars, replace_na, replace=0)%>%
    visdat_(level='world', vars=vars)

  fig=dat%>%
    leaflet()

  if(is.null(map_provider)){
    fig=fig%>%addTiles()
  }else{
    fig=fig%>%addProviderTiles(map_provider)
  }


  for(i in seq_along(vars)){
  fig=fig%>%
    addPolygons(
      color=colorRampI(dat[[vars[i]]], na=0),
      stroke=FALSE,
      smoothFactor = 0,
      fillOpacity=0.5,
      label=~lapply(paste0(country_name,'<br>',label),htmltools::HTML),
      group=names(vars)[i]
    )
  }

  if(length(vars)>1){
    fig=fig%>%
      addLayersControl(baseGroups = names(vars),
                       options=layersControlOptions(collapsed=FALSE)
      )
  }

  if(return_df){

    dat=dat[c('country_name','COUNTRY_3',vars)]%>%
      as_tibble()%>%
      distinct(country_name, COUNTRY_3, .keep_all = TRUE)%>%
      group_by(country_name)%>%
      mutate(temp=n())%>%
      ungroup()%>%
      mutate(country_name=if_else(temp>1, paste0(country_name, ' (',COUNTRY_3,')'), country_name))%>%
      select(-temp, -COUNTRY_3)
    return(list(fig=fig, dat=dat))
  }

  return(fig)

}

#' colorRampI
#'
#' A wrapper for colorRamp with a few modifications. Returns a vector of color codes
#'
#' @param x a numerical vector to be mapped on colors
#' @param na if numeric - the value to which NAs in x are equivalent. If character - color for NA.
#' @param quantiles logical, whether or not map in quantile space, or directly.
#' @param colors - colors to interpolate, to be passed to colorRamp()
#' @param ... other arguments to be passed to colorRamp()
#'
#' @return vector of color codes
#' @export
#'
#' @examples #
colorRampI=function(x, na='white', quantiles=TRUE, colors=c('white','red'), ...){


  if('data.frame' %in% class(x)) x=x%>%pull(1)


  wherena=is.na(x)

  if(length(x)==0 || all(wherena)) return(rep(colors[1], length(x)))
  if(quantiles){
    cdf=ecdf(x)
    x=cdf(x)
    if(is.numeric(na)) x[wherena]=cdf(na)
  }else if(is.numeric(na)){
    x[wherena]=na
  }

  z=colorRamp(colors=colors, ...)(x/max(x,na.rm=TRUE))
  if(is.character(na)){
    y=rep(na, length(x))
    notna=!wherena
    y[notna]=rgb(z[notna,,drop=FALSE], maxColorValue = 255)
  }else{
    y=rgb(z, maxColorValue = 255)
  }

  return(y)
}


# #Not used. A function to coarse-grain polygons in maps, leaving, every-step-th point of the traced boundaries.
# simplify_polygons=function(dat, step=2){
#
#   if(step==1) return(dat)
#
#   dat$x=lapply(split(dat$x, cumsum(is.na(dat$x))), function(x){
#     c(x[seq(1,length(x)-1,step)],x[length(x)])
#   })%>%unlist()%>%setNames(NULL)
#
#   dat$y=lapply(split(dat$y, cumsum(is.na(dat$y))), function(x){
#     c(x[seq(1,length(x)-1,step)],x[length(x)])
#   })%>%unlist()%>%setNames(NULL)
#
#   return(dat)
# }
