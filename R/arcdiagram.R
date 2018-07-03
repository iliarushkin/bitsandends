library(plotly)

#' Interactive arc-diagram
#'
#' @param links a data-frame of links, contains three, sources and targets (populated with numeric node coordinates that serve as node IDs) and values of links. May contain other metadata
#' @param nodes a data-frame of nodes, contains a column of numeric node coordinates (node IDs) that match those in the source and target columns in \code{links} and a column of node values. May contain other metadata
#' @param link.source name of the source column in \code{links}
#' @param link.target name of the target column in \code{links}
#' @param link.side name of the column in \code{links} which is populated with 1 and -1 indicating which side to draw the link on. If NULL, will default to drawing all on one side
#' @param link.color name of the column in \code{links} which is populated with color names for links. If NULL, will assume 'forestgreen' and 'red' for links on side 1 and -1, respectively
#' @param link.value name of the column in \code{links} which is populated with the numeric link value, used to calculate link width
#' @param node.value name of the column in \code{nodes} which is populated with the numeric node value, used to calculate node size
#' @param node.label name of the column in \code{nodes} which is populated with node labels
#' @param node.ID name of the column in \code{nodes} which is populated with numeric node IDs, serving as coordinates
#' @param node.color name of the column in \code{nodes} which is populated with node color names. If NULL, will assume 'steelblue'
#' @param scale controls the height of looping links compared to their length (links are semi-ellipses). Will not have visible effect if plotly is free to set plot scale
#' @param max.node.size maximum node size
#' @param min.node.size minimum node size
#' @param max.link.width maximum link width
#' @param min.link.width minimum link width
#' @param link.min.value minimum link value to retain. Links with values below this will be ignored. The exception is if ALL links have values below \code{link.min.value}, in which case, to avoid creating an empty diagram, \code{link.min.value} will be set to the maximum link value present.
#' @param link.value.prefix prefix to use in link hover labels
#' @param node.value.prefix prefix to use in node hover labels
#' @param remove.jump.size a value or a vector of values. Links with jump size (target minus source) in \code{remove.jump.size} will be ignored. If NULL, no effect.
#' @param orientation if 'h', the diagram will be horizontal, otherwise vertival. Only the first character is checked, case-insensitive.
#'
#' @return a plotly object
#' @export
#'
#' @examples
arcd=function(links,nodes
              ,link.source='source', link.target='target', link.side=NULL,link.color=NULL, link.value='value'
              ,node.value='value',node.label='label',node.ID='reindex',node.color=NULL
              ,scale=0.5,max.node.size=40, min.node.size=2, max.link.width=30, min.link.width=0.5, link.min.value=NULL
              ,link.value.prefix='jumps'
              ,node.value.prefix='landings'
              ,remove.jump.size=NULL
              ,orientation='h'
              ){

  orientation=tolower(substr(orientation,1,1))

  p=plotly::plot_ly()


  link.value.sum=sum(links[,link.value])
  if(!is.null(remove.jump.size)){
    links=subset(links,!((links[,link.target]-links[,link.source]) %in% remove.jump.size))
  }


  if(!is.null(link.min.value)){
    links=subset(links,links[,link.value]>=min(link.min.value,max(links[,link.value])))
  }


  #Plot links:

  if(!is.null(link.side)){
    sides=links[,link.side]
  }else{
    sides=rep(1,nrow(links))
  }

  if(!is.null(link.color)){
    colors=links[,link.color]
  }else{
    colors=rep('forestgreen',nrow(links))
    colors[sides<0]='red'
  }



  sides=sides*scale/(nrow(nodes)-1)
  if(diff(range(links[,link.value]))==0){
    linkwidth=0.5*(min.link.width+max.link.width)
  }else{
    linkwidth=links[,link.value]-min(links[,link.value])
    if(is.infinite(min(links[,link.value]))){print(paste('Problem!',category,k)); return(NULL)}
    linkwidth=linkwidth*(max.link.width-min.link.width)/max(linkwidth)
    linkwidth=linkwidth+min.link.width
  }

  for (i in 1:nrow(links)){

    x1=links[i,link.source]
    x2=links[i,link.target]
    x=c(x1,0.5*(x1+x2),x2)
    y=c(0,abs(x2-x1),0)*sides[i]
    if(!orientation=='h'){
      temp=y
      y=-x
      x=temp
    }

    p=p%>%plotly::add_trace(inherit=FALSE,x=x,y=y,type='scatter',mode='lines',line=list(shape='spline',width=linkwidth[i],color=colors[i])
                    ,showlegend=FALSE
                    ,hoverinfo='text'
                    ,text=paste0(nodes[links[i,link.source],node.label],' >>><br>',nodes[links[i,link.target],node.label]
                                 ,'<br>',link.value.prefix,': ',prettyNum(links[i,link.value],big.mark =','),' (',signif(100*links[i,link.value]/link.value.sum,3),'%)'
                    )
                    ,legendgroup=1
    )


  }


  #Plot nodes
  if(!is.null(node.color)){
    colors=nodes[,node.color]
  }else{
    colors='steelblue'
  }

  if(diff(range(nodes[,node.value]))==0){
    nodesize=0.5*(min.node.size+max.node.size)
  }else{
    nodesize=nodes[,node.value]-min(nodes[,node.value])
    nodesize=nodesize*(max.node.size-min.node.size)/max(nodesize)
    nodesize=nodesize+min.node.size
  }

  if(orientation=='h'){
    x=nodes[,node.ID]
    y=0
  }else{
    y=-nodes[,node.ID]
    x=0
  }

  p=plotly::add_trace(p,inherit=FALSE,x=x,y=y,type='scatter',mode='markers',marker=list(size=nodesize
                                                                                              ,color='white'
                                                                                              ,opacity=1
                                                                                              ,line=list(color=colors,width=3)
                                                                                              )
                  ,showlegend=TRUE
                  ,legendgroup=1
                  ,name='jumps'
                  ,hoverinfo='text'
                  ,text=paste0(nodes[,node.label],'<br>',node.value.prefix,': ',prettyNum(nodes[,node.value],big.mar=','),' (',signif(100*nodes[,node.value]/sum(nodes[,node.value]),3),'%)')
  )


  if(orientation=='h'){
    xaxis=list(showline=FALSE,showgrid=FALSE,zeroline=FALSE,showticklabels=TRUE)
    yaxis=list(showline=FALSE,showgrid=FALSE,zeroline=TRUE,showticklabels=FALSE)
  }else{
    yaxis=list(showline=FALSE,showgrid=FALSE,zeroline=FALSE,showticklabels=TRUE)
    xaxis=list(showline=FALSE,showgrid=FALSE,zeroline=TRUE,showticklabels=FALSE)
  }
  p=plotly::layout(p,title=''
    ,xaxis=xaxis
    ,yaxis=yaxis
    )

  return(p)
}

