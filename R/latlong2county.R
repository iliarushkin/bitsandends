

#' latlong2county
#'
#' Map coordinates onto US counties
#'
#' @param dat tibble with  columns 'longitude' and 'latitude'
#'
#' @return dat with added columns 'fips','countyname', 'county_state' and 'county' (as polyname for package maps)
#'
#' @export
#'
#' @examples #
latlong2county=function(dat){

  require(tidyverse)

  dat=dat%>%mutate(ind=seq_len(n()))

  df=dat%>%
    select(x=longitude, y=latitude, ind)%>%
    drop_na()

  df=df%>%
    bind_cols(
      df%>%latlong2county_()
    )%>%
    replace_na(list(county=''))

  df_temp=df%>%distinct(county)

  temp=lapply(strsplit(df_temp$county,','), function(x){if(length(x)==0){return(c('',''))}else{return(x)}})
  df_temp$countyname=sapply(temp, function(x){paste0(toupper(substring(x[2],1,1)), substring(x[2],2))})
  df_temp$county_state=sapply(temp, function(x){paste0(toupper(substring(x[1],1,1)), substring(x[1],2))})
  df_temp$countyname=sapply(strsplit(df_temp$countyname, split=' '),function(x) paste(paste0(toupper(substring(x,1,1)), substring(x,2)), collapse=' '))
  df_temp$countyname=sapply(strsplit(df_temp$countyname, split='-'),function(x) paste(paste0(toupper(substring(x,1,1)), substring(x,2)), collapse='-'))
  df_temp$county_state=sapply(strsplit(df_temp$county_state, split=' '),function(x) paste(paste0(toupper(substring(x,1,1)), substring(x,2)), collapse=' '))

  df_temp=df_temp%>%mutate(
                    county=na_if(county,''),
                    countyname=na_if(countyname,''),
                    county_state=na_if(county_state,'')
                    )%>%
    left_join(
      maps::county.fips%>%
        mutate(fips=as.character(fips))%>%
        rename(county=polyname)%>%
        distinct(county, .keep_all = TRUE)%>%
        mutate(fips=ifelse(nchar(fips)==4, paste0('0',fips), fips)),

      by='county'
    )

  df=df%>%left_join(df_temp, by='county')

  dat%>%
    left_join(
      df%>%select(ind, fips, county, countyname, county_state),
      by='ind'
    )%>%
    arrange(ind)%>%
    select(-ind)

}

#' latlong2county_
#'
#'Internal function, mapping coordinates onto US counties
#'
#' @param df tibble
#'
#' @return tibble
#'
#' @examples #
latlong2county_=function(df) {
  require(maps)
  require(maptools)
  require(tidyverse)
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties=maps::map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs=sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp=maptools::map2SpatialPolygons(counties, IDs=IDs,
                                  proj4string=CRS("+proj=longlat +datum=WGS84"))

  # Convert df to a SpatialPoints object
  df=df%>%mutate(temp=seq_len(n()), x=as.numeric(x), y=as.numeric(y))
  df1=df%>%filter(!(is.na(x) | is.na(y)))
  if(nrow(df1)==0){
    return(tibble(county=rep(NA, nrow(df))))
  }

  pointsSP=SpatialPoints(df1, proj4string=CRS("+proj=longlat +datum=WGS84"))

  # Use 'over' to get _indices_ of the Polygons object containing each point
  indices=over(pointsSP, counties_sp)

  # Return the county names of the Polygons object containing each point
  countyNames=sapply(counties_sp@polygons, function(x) x@ID)

  df%>%select(temp)%>%left_join(
    tibble(temp=df1$temp, county=countyNames[indices]
    ),
    by='temp'
  )%>%select(-temp)
}
