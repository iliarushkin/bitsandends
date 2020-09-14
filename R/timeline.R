#' timeline
#'
#'A function for creating data for a timeline plot
#'
#' @param dat tibble, must have column t of times. If "summarize" is not "n", needs a column whose name will be passed as "ycol", and if "summarize" is "weighted.mean", also a column of weights "w".
#' @param trange range of times. If NULL, will be inferred from data.
#' @param summarize one of the following strings: "n","n_distinct","sum", "mean", "weighted.mean", "median","max", to apply to ycol.
#' @param ycol name of the column with values to summarize. If "summarize" is "n", may be left NULL.
#' @param step numeric, timestep to use in binning.
#' @param units character, units for step, as in difftime(), e.g. 'mins' or 'days'.
#' @param time_zone time zone.
#'
#' @return a tibble with columns t (bin-end times), n (value)
#' @export
#'
#' @examples #
timeline=function(dat, trange=NULL, summarize='n', ycol=NULL, step=15, units='mins', time_zone='UTC'){

  require(lubridate)
  require(tidyverse)

  if((is.null(dat) || (nrow(dat)==0)) & is.null(trange)) return(tibble(t=Sys.time(), n=0)%>%head(0))

  if(is.null(dat) || (nrow(dat)==0)){
    dat=tibble(t=trange, col_to_count=0)
    ycol='col_to_count'
    summarize='max'
  }

  if(!is.null(ycol)){
    if(('col_to_count' %in% names(dat)) & (ycol!='col_to_count')) dat=dat%>%select(-col_to_count)
    names(dat)[names(dat)==ycol]='col_to_count'
  }

  t=dat%>%pull(t)%>%with_tz(time_zone)

  timestep=paste(step,units)

  if(is.null(trange)){
    # trange=c(floor_date(min(t, na.rm=TRUE)+lubridate::minutes(-1), timestep), ceiling_date(max(t, na.rm=TRUE), timestep))
    trange=c(floor_date(min(t, na.rm=TRUE), timestep), ceiling_date(max(t, na.rm=TRUE), timestep))
  }else{
    trange=as.POSIXct(trange)%>%with_tz(time_zone)
    trange=c(floor_date(trange[1], timestep), ceiling_date(trange[2], timestep))
  }

  ts=seq(trange[1], trange[2], by=timestep)[-1]

  if(length(t)==0){
    return(tibble(t=ts, n=0))
  }

  timebin=ceiling(as.numeric(difftime(t, trange[1], units=units))/step)

  dat=dat%>%
    mutate(t=ts[timebin])%>%
    group_by(t)

  dat=switch(summarize
            ,'n'=dat%>%summarise(n=n())
            ,'n_distinct'=dat%>%summarize(n=n_distinct(col_to_count))
            ,'sum'=dat%>%summarize(n=sum(col_to_count))
            ,'mean'=dat%>%summarize(n=mean(col_to_count))
            ,'weighted.mean'=dat%>%summarize(n=weighted.mean(col_to_count, w))
            ,'median'=dat%>%summarize(n=median(col_to_count))
            ,'max'=dat%>%summarize(n=max(col_to_count))
  )%>%
    ungroup()%>%
    complete(t=ts, fill=list(n=0))%>%
    arrange(t)

  return(dat)

}

#' Timeline_intervals
#'
#' An extension of timeline(summarize='n', crop=FALSE) for intervals rather than single events.
#'
#' @param dat tibble
#' @param trange range of times. If NULL, will be inferred from data.
#' @param step numeric, timestep to use in binning
#' @param units character, units for step, as in difftime(), e.g. 'mins' or 'days'
#' @param time_zone time zone
#'
#' @return a tibble with columns t (bin-end times), n - count of ongoing events in the bin
#' @export
#'
#' @examples #
timeline_intervals=function(dat, trange=NULL, step=15, units='mins', time_zone='UTC'){

  require(tidyverse)
  require(lubridate)

  if(is.null(dat) || (nrow(dat)==0)) return(tibble(t=Sys.time()[0], n=numeric(0)))


  dat=dat%>%
    mutate(t=start_time)%>%
    timeline(trange=trange, step=step, units=units, summarize='n', time_zone=time_zone)%>%
    full_join(
      dat%>%
        mutate(t=end_time)%>%
        timeline(trange=trange, step=step, units=units, summarize='n', time_zone=time_zone)%>%
        mutate(n=lag(n, default=0))%>%rename(n_e=n),
      by='t'
    )%>%
    mutate_if(is.numeric, replace_na, replace=0)%>%
    arrange(t)%>%
    mutate(n=cumsum(n-n_e))%>%
    select(-n_e)
  return(dat)
}


#' timeline_plot
#'
#' Create a timeline plot with one (n(t)) or two (if n_base is present in the data) lines.
#'
#' @param dat tibble with columns t, n and n_base (optional)
#' @param smooth_bw bandwidth for kernel-smoothing the curve, in units of the time step in the data. If 0 - no smoothing.
#' @param smooth_kernel kernel used in smoothing the curve: 'box' or 'normal'.
#' @param ytitle title for the y-axis
#' @param plot_bgcolor plot background color
#' @param paper_bgcolor plot paper color
#' @param linecolor line color
#' @param fillcolor fill color
#' @param hovermode passed to plotly
#' @param ticksuffix tick suffix
#' @param ytitle_fontsize fontsize for the y-axis title
#' @param sliderthickness thickness (relative height) of the range slider
#' @param yrange range for the y-axis. If NULL, will be determined from the data
#' @param linenames name for the line(s), to show in the hoverplates. If n_base is present in the data, should be length 2.
#'
#' @return plotly object
#' @export
#'
#' @examples #
timeline_plot=function(dat,
                       smooth_bw=0,
                       smooth_kernel='box',
                       ytitle='count', plot_bgcolor='#ECF0F5', paper_bgcolor='#FCFCFC', linecolor='darkgreen', fillcolor='steelblue', hovermode='x',
                       ticksuffix='', ytitle_fontsize=14,
                       sliderthickness=0.05,
                       yrange=NULL,

                       linenames=NULL

){

  require(tidyverse)
  require(plotly)
  require(lubridate)

  if(is.null(dat) || nrow(dat)==0) return()

  if(all(dat$n[dat$t==min(dat$t)]==0)) dat=dat%>%filter(t>min(t))
  if(nrow(dat)==0) return()

  trange=range(dat$t)
  if(trange[1]==trange[2]) trange=c(trange[1]-lubridate::hours(1), trange[1] + lubridate::hours(1))

  if(all(dat$n==0)){
    yrange=c(0,1)
  }else if(!is.null(yrange)){

    if(is.null(yrange$outlier_quantile)) yrange$outlier_quantile=1

    z=c(0,1.2*quantile(dat$n, yrange$outlier_quantile))

    if(!is.null(yrange$max)) z[2]=min(z[2], yrange$max)

    yrange=z
  }


  if(nrow(dat)==1){
    fig=dat%>%
      plot_ly(x=~t, y=~n, type='scatter', mode='markers',
              hoverinfo=ifelse(length(linenames), 'x+y+name','x+y'),
              name=linenames[1],
              showlegend=FALSE,
              marker=list(color=linecolor)
      )
  }else{

    if(length(smooth_bw) && (smooth_bw>0)){
      dat=dat%>%mutate(
        x=seq_len(n()), n=ksmooth(x=x, y=n, kernel=smooth_kernel, bandwidth=smooth_bw, n.points=n())$y
      )

      if('n_base' %in% names(dat)){
        dat=dat%>%mutate(
          n_base=ksmooth(x=x, y=n_base, kernel=smooth_kernel, bandwidth=smooth_bw, n.points=n())$y
        )
      }

    }

    fig=dat%>%
      plot_ly(x=~t, y=~n, type='scatter', mode='lines',

              fill=ifelse('n_base' %in% names(.),'', 'tozeroy'),
              hoverinfo=ifelse(length(linenames), 'x+y+name','x+y'),
              name=linenames[1],
              showlegend=FALSE,
              line=list(color=linecolor, width=1),
              fillcolor=col_add_a(fillcolor)
      )

    if('n_base' %in% names(dat)){

      fig=fig%>%
        add_lines(
          y=~n_base,
          fill='tonexty',
          hoverinfo=ifelse(length(linenames)>1, 'x+y+name','x+y'),
          name=linenames[2]
        )

    }


  }

  fig=fig%>%
    layout(
      hovermode=hovermode
      ,legend=list(orientation='h')
      ,xaxis=list(title='', range=trange)
      ,yaxis=list(title=ytitle, rangemode='tozero', ticksuffix=ticksuffix, range=yrange, titlefont=list(size=ytitle_fontsize), fixedrange=FALSE)
      ,paper_bgcolor='white'
      ,plot_bgcolor=plot_bgcolor
      ,paper_bgcolor=paper_bgcolor
      ,margin=list(t=0,b=0)
    )

  if(!isnothing(sliderthickness)) fig=fig%>%rangeslider(thickness=sliderthickness)


  fig%>%
    config(displayModeBar=FALSE)

}
