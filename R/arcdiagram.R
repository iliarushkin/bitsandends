arcd=function(links,nodes
              ,link.source='source', link.target='target', link.side=NULL,link.color=NULL, link.value='value'
              ,node.value='value',node.label='label',node.ID='reindex',node.color=NULL
              ,scale=0.5,max.node.size=40, min.node.size=2, max.link.width=30, min.link.width=0.5, link.min.value=NULL
              ,link.value.prefix='jumps'
              ,node.value.prefix='landings'
              ,remove.jump.size=NULL
              ,orientation='h'
              ){
  
  library(plotly)
  orientation=tolower(substr(orientation,1,1))
  
  p=plot_ly()
  
  
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
    
    p=p%>%add_trace(inherit=FALSE,x=x,y=y,type='scatter',mode='lines',line=list(shape='spline',width=linkwidth[i],color=colors[i])
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
  
  p=p%>%add_trace(inherit=FALSE,x=x,y=y,type='scatter',mode='markers',marker=list(size=nodesize
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
  p=p%>%layout(title=''
    ,xaxis=xaxis
    ,yaxis=yaxis
    )
  
  return(p)
}

