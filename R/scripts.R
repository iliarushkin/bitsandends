##kmeans for 1D data, where the clusters are ordered by center value.

#' kmeans for 1D data with ordered clusters
#'
#' Uses \code{kmeans} and then renames the clusters so that they are arranged in the increasing order of means
#' @param x vector of 1D data
#' @param centers number of clusters
#' @param ... other arguments passed to \code{kmeans}
#'
#' @return a list returned by \code{kmeans} with clusters arranged in the order of their means in the following list elements: \code{cluster, withinss, size, centers}.
#' Additional elements are also addded to the list: \code{frac} is the cluster size as a fraction of the total, \code{cluster_sd} is the standard deviations of clusters and \code{centers_se} is the standard error of the cluster centers.
#' @export
#'
#' @examples
kmeans1=function(x,centers,...){

  if(length(unique(x))<centers){
    cl=list(cluster=rep(1,length(x))
            ,size=length(x)
            ,frac=1
            ,centers=matrix(mean(x),nrow=1,ncol=1,dimnames=list(c('1')))
            ,cluster_sd=sd(x)
            ,centers_se=sd(x)/sqrt(length(x))
            )

    return(cl)
  }

  cl=kmeans(x=x,centers=centers,...)

  cl$cluster=rank(cl$centers)[cl$cluster]
  cl$withinss=cl$withinss[order(cl$centers)]
  cl$size=cl$size[order(cl$centers)]
  cl$frac=cl$size/sum(cl$size)

  cl$centers[,1]=sort(cl$centers[,1])

  cl$cluster_sd=aggregate(x,by=list(cl$cluster),function(x){sd(x)})[,2]
  cl$centers_se=cl$cluster_sd/sqrt(cl$size)
  return(cl)

}



#' A linear regression y=kx+a
#'
#' @param x x-coordinates of data points, NAs will be ignored
#' @param y y-coordinates of data points, NAs will be ignored
#'
#' @return a list with elements: \item{a}{intercept}\item{k}{slope}\item{r2}{R-squared}\item{r}{correlation coefficient}\item{r.sd}{standard deviation of the correlation coefficient}
#' @export
#'
#' @examples
lin=function(x,y){
  ind=which(!(is.na(x)|is.na(y)))
  if(length(ind)<2){return(list(a=NA,k=NA,r2=NA,r=NA,r.sd=NA))}
  x=x[ind]
  y=y[ind]
  r=cor(x,y)
  r2=r^2
  r.sd=sqrt((1-r2)/(length(ind)-2))
  k=r*sd(y)/sd(x)
  a=mean(y)-k*mean(x)

  return(list(a=a,k=k,r2=r2,r=r,r.sd=r.sd))


}








#' Sampling for k-fold validation
#'
#' @param ndata the number of rows in the data
#' @param k number of folds
#' @param repetitions number of repetitions of the k-fold procedure
#'
#' @return a list of \code{k*repetitions} components, ieach of which is a vector of randomly sampled indices of length close to \code{ndata/k}.
#' @export
#'
#' @examples
kfoldval=function(ndata,k=5,repetitions=3){

  i=list()
  for(r in 1:repetitions){

    ind=sample(1:ndata)

    chunksize=round(ndata/k)

    i=c(i,lapply(1:k,function(j){
      if(j<k){
        ind[seq(chunksize*(j-1)+1,chunksize*j)]
      }else{
        ind[seq(chunksize*(j-1)+1,ndata)]
      }
    })
    )
  }
  return(i)

}


#A function that computes correlation using only positive or only negative shifts. This is controlled by the argument "only".
#If "only" is a positive number, only positive shifts, if negative - only negative shifts. If null - the usual correlation.
cor1=function(x,y,only=NULL){

  ind=which(!(is.na(x)|is.na(y)))
  if(length(ind)<2) return(NA)

  x=x[ind]
  y=y[ind]

  temp=(x-mean(x))*(y-mean(y))

  if(!is.null(only)){

    if(only>0){
      temp[temp<0]=0
    }else if(only<0){
      temp[temp>0]=0
    }else{
      temp=0
    }

  }

  temp=sum(temp)/(length(ind)-1)
  temp=temp/(sd(x)*sd(y))

  return(temp)

}



#Calculate p-value of the t-test for two samples: x1 and x2. Alternatively, if at least one of these is not given (NULL), instead
#assume that m, sdev and n are, respectively, the means, sdevs and sizes of the samples. Each of these is a vector of length 2 (number of samples.)

#' p-value for a 2-sample Welch's t-test.
#'
#' Can be calculated from the provided samples. Alternatively, if either one of the samples is NULL, can be calculated from the provided means, standard deviations and sample sizes
#'
#' @param x1 first data sample, NAs will be ignored
#' @param x2 second data sample, NAs will be ignored
#' @param m a vector of two values: means of the data samples (used only if \code{x1} or \code{x2} is \code{NULL})
#' @param sdev a vector of two values: standard deviations of the data samples (used only if \code{x1} or \code{x2} is \code{NULL})
#' @param n a vector of two values: sizes of the data samples (used only if \code{x1} or \code{x2} is \code{NULL})
#' @param two.tailed boolean to indicate whether the two-tailed or single-tailed p-value should be calculated
#'
#' @return the Welch's t-test p-value
#' @export
#'
#' @examples
pval=function(x1=NULL,x2=NULL,two.tailed=TRUE,m,sdev,n){
  if(is.null(x1)|is.null(x2)){

    m1=m[1]; m2=m[2]
    n1=n[1]; n2=n[2]
    varm1=((sdev[1])^2)/n1
    varm2=((sdev[2])^2)/n2
  }else{
    x1=x1[!is.na(x1)]
    x2=x2[!is.na(x2)]
    n1=length(x1);
    n2=length(x2);
    m1=mean(x1);
    m2=mean(x2);
    varm1=var(x1)/n1;
    varm2=var(x2)/n2;
  }
  t=(abs(m1-m2))/sqrt(varm1+varm2);
  dof=(varm1+varm2)^2/(varm1^2/(n1-1)+varm2^2/(n2-1));
  pval= pt(t,dof,lower.tail=FALSE);
  if(two.tailed){pval=2*pval}
  return(pval);
}


#' Cohen's d of two samples
#'
#' @param x1 first data sample, NAs will be ignored
#' @param x2 second data sample, NAs will be ignored
#' @param m a vector of two values: means of the data samples (used only if \code{x1} or \code{x2} is \code{NULL})
#' @param sdev a vector of two values: standard deviations of the data samples (used only if \code{x1} or \code{x2} is \code{NULL})
#' @param n a vector of two values: sizes of the data samples (used only if \code{x1} or \code{x2} is \code{NULL})
#'
#' @return Cohen's d
#' @export
#'
#' @examples
Cohen.d=function(x1=NULL,x2=NULL,m,sdev,n){

  if(is.null(x1)|is.null(x2)){

    return((m[2]-m[1])/sqrt((((sdev[1])^2)*(n[1]-1)+((sdev[1])^2)*(n[2]-1))/(n[1]+n[2]-2)))

  }else{
    x1=x1[!is.na(x1)];
    x2=x2[!is.na(x2)];
    return((mean(x2)-mean(x1))/sqrt((var(x1)*(length(x1)-1)+var(x2)*(length(x2)-1))/(length(x1)+length(x2)-2)))
  }


}


# agg_mean=function(x,by,name='x',na.se.rm=FALSE){
#
#   if(is.null(names(by))){
#     names(by)=paste0('Group.',1:length(by))
#   }
#
#   y=plyr::rename(aggregate(x,by=by,FUN=mean,na.rm=TRUE),c('x'=name))
#   y_sd=plyr::rename(aggregate(x,by=by,FUN=sd,na.rm=TRUE),c('x'='sd'))
#   y_n=plyr::rename(aggregate(x,by=by,FUN=length),c('x'='n'))
#   y_max=plyr::rename(aggregate(x,by=by,FUN=max,na.rm=TRUE),c('x'='max'))
#   y_min=plyr::rename(aggregate(x,by=by,FUN=min,na.rm=TRUE),c('x'='min'))
#
#   y$comb_id=paste(y[,names(by)[1]],'and')
#   y_sd$comb_id=paste(y_sd[,names(by)[1]],'and')
#   y_n$comb_id=paste(y_n[,names(by)[1]],'and')
#   y_max$comb_id=paste(y_max[,names(by)[1]],'and')
#   y_min$comb_id=paste(y_min[,names(by)[1]],'and')
#   if(length(by)>1){
#     for(i in 1:length(by)){
#       y$comb_id=paste(y$comb_id,'and',y[,names(by)[i]])
#       y_sd$comb_id=paste(y_sd$comb_id,'and',y_sd[,names(by)[i]])
#       y_n$comb_id=paste(y_n$comb_id,'and',y_n[,names(by)[i]])
#       y_max$comb_id=paste(y_max$comb_id,'and',y_max[,names(by)[i]])
#       y_min$comb_id=paste(y_min$comb_id,'and',y_min[,names(by)[i]])
#     }
#
#   }
#
#   y=merge(y,y_sd[,c('comb_id','sd')],by='comb_id')
#   y=merge(y,y_n[,c('comb_id','n')],by='comb_id')
#   y=merge(y,y_max[,c('comb_id','max')],by='comb_id')
#   y=merge(y,y_min[,c('comb_id','min')],by='comb_id')
#   y$comb_id=NULL
#   y$se=y$sd/sqrt(y$n)
#
#   if(na.se.rm){
#     y=subset(y,!is.na(y$se))
#   }
#
#   ##effect size and pvalues
#
#   i2=which(sapply(by,function(x){length(unique(x))})==2)
#   if(length(i2)>0){
#     i2=i2[1]
#
#     u=unique(y[,names(by)[i2]])
#     y$comb_id=''
#     for (i in 1:length(by)){
#       if(i!=i2){
#         y$comb_id=paste(y$comb_id,'and',y[,names(by)[i]])
#       }
#     }
#
#     values=unique(y$comb_id)
#     y$pval=NA
#     y$Cohen.d=NA
#     for(value in values){
#       i=which(y$comb_id==value)
#       t=y[i,]
#       y$pval[i]=pval(m=t[,name],sdev=t$sd,n=t$n,agg.data=TRUE)
#       y$Cohen.d[i]=Cohen.d(m=t[,name],sdev=t$sd,n=t$n,agg.data=TRUE)
#     }
#
#     y$comb_id=NULL
#   }
#
#
#
#
#   return(y)
#
# }


#' Sorts string vector by numeric characters
#'
#' Numeric characters in each vector element are concatenated and treated as integers, and the sorting is done by these intergers. E.g. \code{c('a10','a2','a1')} would be sorted as \code{c('a1','a2','a10')}.
#'
#' @param x vector of strings
#' @param ... other arguments passed to the function \code{sort}
#'
#' @return sorted vector of strings
#' @export
#'
#' @examples
sortnum=function(x,...){
  options(warn=-1)
  y=x
  y=gsub('[[:alpha:]]','',y)
  y=gsub('[[:punct:]]','',y)

    x=x[order(as.integer(y),...)]

  return(x)

}

