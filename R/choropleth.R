#' visdat
#' 
#' Prepares data for a choropleth leaflet map.
#'
#' @param dat tibble.
#' @param level either 'us_county' (contiguous us map on the level of counties), or 'world' (on the level of countries).
#'
#' @return a list object that can be passed to leaflet()
#' @export
#'
#' @examples
visdat=function(dat, level='us_county', us_counties=NULL, country_name_alphacode=NULL){
  
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

map_us=function(dat, item_name, suffix='', decimals=0, return_df=FALSE, include_vars=c('n')){
  
  dat=dat%>%visdat()
  
  fig=dat%>%
    leaflet()%>%
    addTiles()%>%
    addPolygons(
      color=~colorRampI(n, na=0),
      stroke=FALSE,
      smoothFactor = 0,
      fillOpacity=0.5,
      label=~lapply(paste0(STATE,', ',COUNTYNAME,'<br>',item_name,': ',ifelse(is.na(n), 0, pN(round(n,decimals))), suffix
      ),htmltools::HTML)
    )
  
  if(return_df){
    if(!'n1' %in% names(dat)) dat$n1=0
    if(!'n2' %in% names(dat)) dat$n2=0
    dat=tibble(STATE=dat$STATE, COUNTYNAME=dat$COUNTYNAME, n=dat$n, n1=dat$n1, n2=dat$n2)%>%
      drop_na(n)%>%filter(n>0)%>%
      distinct()%>%
      arrange(desc(n))
    return(list(dat=dat, fig=fig))
  }
  
  return(fig)
  
}

map_world=function(dat, item_name, suffix='', decimals=0, return_df=FALSE){
  
  dat=dat%>%visdat(level='world')
  
  fig=dat%>%
    leaflet()%>%
    setView(lng = 0, lat = 25, zoom = 1.4)%>%
    addTiles()%>%
    addPolygons(
      color=~colorRampI(n, na=0),
      stroke=FALSE,
      smoothFactor = 0,
      fillOpacity=0.5,
      label=~lapply(paste0(country_name,', ','<br>',item_name,': ',ifelse(is.na(n), 0, pN(round(n,decimals))), suffix
      ),htmltools::HTML)
    )
  
  if(return_df){
    if(!'n1' %in% names(dat)) dat$n1=0
    if(!'n2' %in% names(dat)) dat$n2=0
    dat=tibble(country_name=dat$country_name, COUNTRY_3=dat$COUNTRY_3, n=dat$n, n1=dat$n1, n2=dat$n2)%>%
      drop_na(n)%>%filter(n>0)%>%
      distinct(country_name, COUNTRY_3, .keep_all = TRUE)%>%
      group_by(country_name)%>%
      mutate(temp=n())%>%
      ungroup()%>%
      mutate(country_name=if_else(temp>1, paste0(country_name, ' (',COUNTRY_3,')'), country_name))%>%
      select(-temp, -COUNTRY_3)%>%
      arrange(desc(n))
    return(list(dat=dat, fig=fig))
  }
  
  return(fig)
  
}
