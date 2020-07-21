

#' pyramid_plot
#'
#'Create a population pyramid plot. Traditionally, x-axis is gender and y-axis is age, so even though in this plot they both can have other names and values, we use "age" and "gender" for column names.
#'
#' @param dat tibble with columns "age" and "gender". "gender" should be a factor with two levels (e.g. "Female" and "Male"). If not a factor, will be converted into a factor. Age can be numeric or character.
#' @param yticks values to show on the yaxis
#' @param xtitle x-axis title
#' @param ytitle y-axis title
#' @param yname name for the variable on y-axis, to show in the hoverplate.
#' @param palette vector of two colors, for Female and Male (in that order).
#' @param plot_bgcolor plot background color
#' @param paper_bgcolor plot paper color
#'
#' @return plotly object
#' @export
#'
#' @examples #
pyramid_plot=function(dat, yticks=NULL, xtitle='', ytitle='', yname=NULL,
                      palette=c('tomato', 'steelblue'),
                      plot_bgcolor='#ECF0F5', paper_bgcolor='#FCFCFC'
){

  require(tidyverse)
  require(plotly)

  if(is.null(dat) || (nrow(dat)==0)) return()

  if(is.null(yname)) yname=ytitle
  palette=palette[1:2]
  if(is.null(names(palette))) names(palette)=c('f','m')

  dat$gender=factor(dat$gender)
  dat$gender_num=as.numeric(dat$gender)

  df1=dat%>%filter(gender_num==2)%>%
    drop_na(age)%>%
    group_by(age)%>%
    summarize(n=n())%>%
    complete(age=yticks, fill=list(n=0))%>%
    mutate(pct=100*n/nrow(dat))%>%
    mutate(nx=-n)

  df2=dat%>%filter(gender_num==1)%>%
    drop_na(age)%>%
    group_by(age)%>%
    summarize(n=n())%>%
    complete(age=yticks, fill=list(n=0))%>%
    mutate(pct=100*n/nrow(dat))

  if(is.character(yticks)){
    df1=df1%>%mutate(age=factor(age, levels=yticks))
    df2=df2%>%mutate(age=factor(age, levels=yticks))
  }

  amplitude=max(df1$n, df2$n)
  tickvals=pretty(c(0, amplitude), n=1)
  if(length(tickvals)>2) tickvals=tickvals[tickvals<=amplitude]
  amplitude=max(amplitude, tickvals)
  tickvals=c(-rev(tickvals[tickvals>0]), tickvals)

  fig=df1%>%
    plot_ly(x=~nx, y=~age,
            type='bar',
            orientation='h',
            hoverinfo='text+name',
            text=~paste0(yname,': ',age,'<br>count: ',pN(n),' (',pN(signif(pct,2)),'%)'),
            marker=list(color=palette[2]),
            name=levels(dat$gender)[2],
            showlegend=FALSE
    )%>%
    add_bars(
      data=df2,
      x=~n, y=~age,
      marker=list(color=palette[1]),
      name=levels(dat$gender)[1],
      showlegend=FALSE
    )

  fig=fig%>%
    layout(
      barmode='relative',
      xaxis=list(title=xtitle, tickvals=tickvals, ticktext=abs(tickvals), range=1.1*c(-amplitude, amplitude)),
      yaxis=list(title=ytitle),
      plot_bgcolor=plot_bgcolor,
      paper_bgcolor=paper_bgcolor
    )%>%config(displayModeBar=FALSE)
  return(fig)

}
