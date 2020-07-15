#' sunburstdat
#'
#'Prepares data for plotly geographic sunburst plot.
#'
#' @param dat tibble
#' @param pal palette - a vector of colors. If NULL, will use Set1 from RColorBrewer
#'
#' @return a tibble that can be passed to plotly(type='sunburst')
#' @export
#'
#' @examples #
sunburstdat=function(dat, pal=NULL){

  require(tidyverse)
  require(RColorBrewer)

  if(is.null(dat) || (nrow(dat)==0)) return()
  dat=dat%>%filter(n>0)
  if(nrow(dat)==0) return()
  if(is.null(pal)) pal=RColorBrewer::brewer.pal(9,'Set1')

  df=dat%>%
    mutate(country=na_if(country,''), countrylabel=na_if(countrylabel,''), subdivision=na_if(subdivision,''))%>%
    replace_na(list(country='Unknown', countrylabel='Unknown', subdivision='Unknown'))%>%
    mutate(level1=ifelse(country=='US',countrylabel,'other countries'),
           level2=ifelse(country=='US', subdivision, countrylabel),
           level3=ifelse(country=='US','',subdivision)
    )

  n_total=sum(df$n)

  df1=df%>%
    mutate(ids=paste0('lev1_',as.numeric(factor(paste(level1,'/')))),
           parents='lev0_1'
    )%>%
    select(ids, parents, labels=level1, n)

  df2=df%>%
    mutate(ids=paste0('lev2_',as.numeric(factor(paste(level1,'/',level2)))),
           parents=df1$ids
    )%>%
    select(ids, parents, labels=level2, n)

  df3=df%>%
    mutate(ids=paste0('lev3_',as.numeric(factor(paste(level1,'/',level2,'/',level3)))),
           parents=df2$ids
    )%>%
    select(ids, parents, labels=level3, n)


  df=tibble(labels='World-wide', ids='lev0_1', n=n_total, parents='')%>%
    bind_rows(
      df1%>%group_by(ids)%>%summarize(n=sum(n), labels=first(labels), parents=first(parents))%>%
        ungroup()%>%
        arrange(desc(n))%>%
        mutate(color=pal[(seq_len(nrow(.)) %% length(pal))+1])
      # mutate(color=colorRampPalette(pal)(nrow(.)))
    )%>%
    bind_rows(
      df2%>%group_by(ids)%>%summarize(n=sum(n), labels=first(labels), parents=first(parents))%>%
        ungroup()%>%
        arrange(desc(n))%>%
        mutate(color=pal[(seq_len(nrow(.)) %% length(pal))+1])
    )%>%
    bind_rows(df3%>%filter(labels!=''))


  df$values=df$n
  df$values[df$ids %in% df$parents]=0

  #Remove Unknown regions, keeping their percentages
  temp=df%>%filter(labels=='Unknown')%>%
    filter(parents %in% df$ids)%>%
    group_by(ids=parents)%>%
    summarize(n=sum(n))

  i=which(df$ids %in% temp$ids)
  if(length(i)>0){
    df$values[i]=temp$n[match(df$ids[i],temp$ids)]
  }
  df=df%>%filter(labels!='Unknown')


  df$pct=df$n/df$n[match(df$parents,df$ids)]
  return(df)

}



#' sunburst_plot
#'
#' Create sunburst plot
#'
#' @param dat tibble, as prepared by sunburstdat()
#' @param item_name string
#' @param domain as in plotly
#'
#' @return plotly object
#' @export
#'
#' @examples #
sunburst_plot=function(dat, item_name, domain=list(x=c(0,1), y=c(0,1))){
  if(is.null(dat) || (nrow(dat)==0)) return(plotly_empty(type='scatter', mode='markers')%>%config(displayModeBar=FALSE))

  dat$pct_text=paste0('<br>',signif(dat$pct*100,2),'% of ',item_name,' from ', dat$labels[match(dat$parents, dat$ids)])
  temp=(dat$parents=='lev0_1')
  temp1=temp
  temp[is.na(temp1)]=FALSE
  dat$pct_text[temp]=gsub(dat%>%filter(ids=='lev0_1')%>%pull(labels),'all',dat$pct_text[temp])
  temp=!temp
  temp[is.na(temp1)]=FALSE
  dat$pct_text[temp]=paste0(dat$pct_text[temp],'<br>',signif(100*dat$n[temp]/dat$n[dat$ids=='lev0_1'],2),'% of all')
  dat$pct_text[is.na(dat$pct)]=''

  dat%>%
    plot_ly(labels=~labels, ids=~ids, parents=~parents, values=~values, type='sunburst',
            marker=list(lines=list(width=0.5)
                        , colors=~color
            ),
            textinfo='text',
            hoverinfo='text',
            text=~paste0(truncateString(labels, 20),'<br>',item_name,':<br>',pN(round(n,0))),
            hovertext=~paste0(labels,'<br>',item_name,': ',pN(round(n,0)), pct_text),
            insidetextorientation ='auto',
            domain=domain
    )%>%
    layout(
      paper_bgcolor='white',
      margin=list(t=10, l=0, r=0, b=50)
    )%>%config(displayModeBar=FALSE)

}
