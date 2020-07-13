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
visdat_=function(dat, level='us_county'){

  require(tidyverse)

  if(level=='us_county'){
    if(is.null(dat) || (nrow(dat)==0)) dat=us_counties%>%mutate(n=0)
    visdat=us_county_foundation_map
  }else if(level=='world'){
    if(is.null(dat) || (nrow(dat)==0)) dat=country_name_alphacode%>%select(COUNTRY=COUNTRY_2, COUNTRY_3, country_name)%>%mutate(n=0)
    visdat=world_foundation_map
  }

  dat=dat%>%mutate(n=na_if(n,0))

  for(x in names(dat)){
    visdat[[x]]=dat%>%pull(x)
  }

  return(visdat)
}

#' map_us
#'
#'Choropleth map of contiguous US on county level
#'
#' @param dat tibble, containing polyname (in the format of polyname in package "maps", e.g. "new york,new york"), n (numeric variable to be plotted), and label (to be shown in hoverlabel)
#' @param item_name string, description of the plotted variable, to be shown in the hoverlabel
#' @param suffix string, units to be shown after the value of n in the hoverlabel
#' @param decimals # integer, decimal points in rounding
#' @param return_df #Boolean, whether to return table of results instead of a plot
#' @param include_vars list of variables to include in the returned table. If return_df is FALSE, ignored.
#'
#' @return leaflet object or tibble, depending on return_df
#' @export
#'
#' @examples
map_us=function(dat, return_df=FALSE, include_vars=c('n')){

  require(tidyverse)
  require(leaflet)

  dat=dat%>%right_join(

    us_counties,
    by='polyname'

  )%>%
    arrange(ind)%>%
    select(-ind)%>%
    replace_na(list(n=0))%>%
    visdat_()

  fig=dat%>%
    leaflet()%>%
    addTiles()%>%
    addPolygons(
      color=~colorRampI(n, na=0),
      stroke=FALSE,
      smoothFactor = 0,
      fillOpacity=0.5,
      label=~lapply(paste0(STATE,', ',COUNTYNAME,'<br>',label),htmltools::HTML)
    )

  if(return_df){

    dat=dat[c('STATE','COUNTYNAME',include_vars)]%>%
      as_tibble()%>%
      drop_na(n)%>%
      filter(n>0)%>%
      distinct()%>%
      arrange(desc(n))
    return(dat)
  }

  return(fig)

}

#' map_world
#'
#'Choropleth map of the world on country level
#'
#' @param dat tibble, containing country (2-letter country codes, e.g. "US"), n (numeric variable to be plotted) and label (to be shown in hoverlabel)
#' @param item_name string, description of the plotted variable, to be shown in the hoverlabel
#' @param suffix string, units to be shown after the value of n in the hoverlabel
#' @param decimals # integer, decimal points in rounding
#' @param return_df #Boolean, whether to return table of results instead of a plot
#' @param include_vars list of variables to include in the returned table. If return_df is FALSE, ignored.
#'
#' @return leaflet object or tibble, depending on return_df
#' @export
#'
#' @examples
map_world=function(dat, return_df=FALSE, include_vars=c('n')){


  dat=dat%>%rename(COUNTRY_2=country)%>%
    right_join(country_name_alphacode, by='COUNTRY_2')%>%
    rename(COUNTRY=COUNTRY_2)%>%
    arrange(ind)%>%
    select(-ind)%>%
    replace_na(list(n=0))%>%
    visdat_(level='world')

  fig=dat%>%
    leaflet()%>%
    setView(lng = 0, lat = 25, zoom = 1.4)%>%
    addTiles()%>%
    addPolygons(
      color=~colorRampI(n, na=0),
      stroke=FALSE,
      smoothFactor = 0,
      fillOpacity=0.5,
      label=~lapply(paste0(country_name,'<br>',label),htmltools::HTML)
    )

  if(return_df){

    dat=dat[c('country_name','COUNTRY_3',include_vars)]%>%
      as_tibble()%>%
      drop_na(n)%>%
      filter(n>0)%>%
      distinct(country_name, COUNTRY_3, .keep_all = TRUE)%>%
      group_by(country_name)%>%
      mutate(temp=n())%>%
      ungroup()%>%
      mutate(country_name=if_else(temp>1, paste0(country_name, ' (',COUNTRY_3,')'), country_name))%>%
      select(-temp, -COUNTRY_3)%>%
      arrange(desc(n))
    return(dat)
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
