#' segment_plot
#'
#' Segment plot, showing an arrow from x to xend for each value of y
#'
#' @param dat tibble with columns label (values for the y axis), x and xend (these two have to be numeric)
#' @param n integer. If positive, # of labels with the largest change to plot. If negative, # of labels with the smallest change to plot
#' @param unit char, units to be shown in the hoverplates
#' @param xtitle x-axis title
#' @param line_width line width
#' @param arrow_size arrow size
#' @param plot_bgcolor plot background color
#' @param paper_bgcolor plot paper color
#'
#' @return plotly object
#' @export
#'
#' @examples #
segment_plot=function(dat, n=20, unit='', xtitle='', line_width=3, arrow_size=7,
                      plot_bgcolor='#ECF0F5', paper_bgcolor='#FCFCFC'
){

  dat=dat%>%
    mutate(dx=xend-x)%>%
    arrange(desc(dx))

  if(n>0){
    dat=dat%>%head(n)
  }else{
    dat=dat%>%tail(-n)
  }

  dat=dat%>%
    mutate(
      label=factor(label, levels=label),
      color=ifelse(dx>0,'darkgreen','red')
    )

  fig=plot_ly()

  df2=dat%>%filter(dx>0)%>%
    mutate(hovertext=paste0(label,'<br>increase ',pN(dx,r=0),' ',unit,', ',pN(100*dx/x, r=0, suff='%'),'<br>(from ',pN(x,r=0),' to ',pN(xend, r=0),')'))

  if(nrow(df2)){
    fig=fig%>%
      add_segments(data=df2,
                   x=~x, xend=~xend,
                   y=~label, yend=~label,
                   line=list(color=~color, width=line_width),
                   hoverinfo='text',
                   hovertext=~hovertext,
                   showlegend=FALSE

      )%>%
      add_markers(x=~xend, y=~label, marker=list(symbol='triangle-right', color=~color, size=arrow_size),
                  hoverinfo='text',
                  hovertext=~hovertext,
                  showlegend=FALSE
      )
  }

  df2=dat%>%filter(dx<=0)%>%
    mutate(hovertext=paste0(label,'<br>decrease ',pN(-dx,r=0),' ',unit,', ',pN(100*dx/x, r=0, suff='%'),'<br>(from ',pN(x,r=0),' to ',pN(xend, r=0),')'))
  if(nrow(df2)){
    fig=fig%>%
      add_segments(data=df2,
                   x=~x, xend=~xend,
                   y=~label, yend=~label,
                   line=list(color=~color, width=line_width),
                   hoverinfo='text',
                   hovertext=~hovertext,
                   showlegend=FALSE

      )%>%
      add_markers(data=df2, x=~xend, y=~label, marker=list(symbol='triangle-left', color=~color, size=arrow_size),
                  hoverinfo='text',
                  hovertext=~hovertext,
                  showlegend=FALSE
      )
  }
  fig=fig%>%
    layout(
      yaxis=list(title='', autorange='reversed', tickvals=dat$label),
      xaxis=list(title=xtitle),
      plot_bgcolor=plot_bgcolor,
      paper_bgcolor=paper_bgcolor
    )%>%config(displayModeBar=FALSE)

  return(fig)
}
