#' cron
#'
#' A makeshift "cron job" functionality
#'
#' @param runfile path to the R file to run
#' @param frequency a string consisting of an integer, a space, and then one of "seconds", "minutes", "hours", "days", "weeks", "months", "years". Plural form is optional and "sec" and "min" are understood.
#' @param start_time datetime object
#' @param end_time datetime object or Inf
#' @param dt pulse period in seconds
#' @param verbose logical
#'
#' @return
#' @export
#'
#' @examples ##
cron=function(runfile, frequency='1 day', start_time=Sys.time(), end_time=Inf, dt=5, verbose=FALSE){

  cron__tgoal=start_time
  cron__end_time=end_time
  cron__frequency=frequency
  cron__dt=dt
  cron__verbose=verbose
  cron__runfile=runfile
  cron__has_run=FALSE
  while(Sys.time()< cron__end_time){
    tnow=Sys.time()
    if((tnow>=cron__tgoal) &(!cron__has_run)){
      cat(as.character(Sys.time()),'Cron job running.\n')
      try({source(cron__runfile)})
      cron__has_run=TRUE
    }else{
      cron__tgoal=tnext_(cron__tgoal, tnow=tnow, frequency = cron__frequency)
      cron__has_run=FALSE
      if(cron__verbose){
        cat(as.character(Sys.time()),'passing; next run aiming at ',as.character(cron__tgoal),'\n')
      }
    }
    Sys.sleep(cron__dt)
  }

}



#' tnext_
#'
#' Given a time sequence from the time t with a frequency, return the first sequence element that is >= tnow
#'
#' @param t datetime, start of sequence
#' @param tnow
#' @param frequency a string consisting of an integer, a space, and then one of "seconds", "minutes", "hours", "days", "weeks", "months", "years". Plural form is optional and "sec" and "min" are understood.
#'
#' @return
#'
#' @examples
tnext_=function(t, tnow=Sys.time(), frequency='1 day'){
  require(lubridate)
  if(t>tnow) return(t)

  temp=strsplit(frequency,' ')[[1]]
  n=as.numeric(temp[1])
  u=temp[2]
  u=ifelse(substring(u, nchar(u))=='s', u, paste0(u,'s'))
  if(u=='secs') u='seconds'
  if(u=='mins') u='minutes'

  temp=n*seq_len(2+length(seq(t, tnow, by=frequency)))
  tnext=switch(u,
           'seconds'=t+seconds(temp),
           'minutes'=t+minutes(temp),
           'hours'=t+hours(temp),
           'days'=t+days(temp),
           'weeks'=t+weeks(temp),
           'months'=t+months(temp),
           'years'=t+years(temp)
           )
  tnext=tnext[which(tnext>=tnow)[1]]

  return(tnext)

}


