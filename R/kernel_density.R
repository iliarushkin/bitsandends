#' Gaussian-kernel density estimation
#'
#'Estimates density of data in arbitrary number of dimensions
#'
#' @param x a matrix or data-frame of data points. Number of columns will be interpreted as dimensionality
#' @param xgrid a matrix or data-frame of point in which density will be computed, the same number of columns as in \code{x}. If NULL, will assume a uniform grid in the data range, 30 points in each dimension.
#' @param b a vector of bandwidths (standard deviations of the Gaussian kernel). If the length is not equal to the number of columns in \code{x}, will use the first element of \code{b} in all dimensions.
#'
#' @return a data-frame that is \code{xgrid} with the additional "density" column
#' @export
#'
gde=function(x,xgrid=NULL,b=1){

  x=as.matrix(x)

  tic=proc.time()[3]
  if(is.null(xgrid)){

    ranges=as.list(as.data.frame(apply(x,2,range)))
    ranges=lapply(ranges,FUN=function(x){seq(x[1],x[2],length.out=30)})
    xgrid=as.matrix(
      expand.grid(ranges)
    )

  }else{
    xgrid=as.matrix(xgrid)
  }

  if(length(b)<ncol(x)) b=rep(b[1],ncol(x))
  invb2=(1/b[1:ncol(x)])^2

  dens=apply(xgrid,1,FUN=function(xxgrid){
    mean(apply(x,1,FUN=function(xx){
      exp(-0.5*sum((xx-xxgrid)^2*invb2))
    }))
  })

  xgrid=as.data.frame(xgrid)
  xgrid$density=dens*sqrt(prod(invb2/(2*pi)))


  return(xgrid)

}
