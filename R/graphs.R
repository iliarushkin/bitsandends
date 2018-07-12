#' Clean Knowledge Graph
#'
#' Removes spurious and duplicated rows and checks for the absence of loops
#'
#' @param kgraph a dataframe or matrix of two columns.
#'
#' @return the cleaned version of  of the input, if no loops are found. If there are loops, will message and return NULL
#' @export
#'
#' @examples
clean_kgraph=function(kgraph){

  if(ncol(kgraph)!=2){
    stop('kgraph must have two columns.\n')
  }

  #Remove empty rows:
  kgraph=kgraph[!(is.na(kgraph[,1])|is.na(kgraph[,2])),]
  kgraph=kgraph[(kgraph[,1]!='')&(kgraph[,2]!=''),]

  #Remove rows connecting a KC to itself
  kgraph=kgraph[kgraph[,1]!=kgraph[,2],]

  #Remove duplicate rows
  kgraph=unique(kgraph)

  cl=igraph::clusters(igraph::graph_from_edgelist(as.matrix(kgraph)),mode='strong')

  loops=which(cl$csize>1)
  if(length(loops)>0){

    cat('Loops found:\n')
    for(loop in loops){
      cat(paste(names(membership)[membership==loop],collapse=' - '),'\n')
    }
   return(NULL)
  }

  return(kgraph)

}
