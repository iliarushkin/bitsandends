#' Grid-search through parameters of a classifier model
#'
#'Given a vector of values of parameters, return classification errors and confusion matrices of a classifier model for each combination.
#'
#' @param data the data frame of data, containing the predictor columns and the outcome column
#' @param model the function that executes the model, such as randomForest::randomForest or e1071::svm. The data frame argument of this function should be named "data", and the formula should be named "formula".
#' @param formula the formula of the classifier, to be passed to the model
#' @param params a list, each element is named as a parameter to be passed to the model and contains a vector of values to try.
#' @param k a positive integer, the number of folds in the k-fold validation
#' @param repetitions a positive integer, the number of repetitions in the k-fold validation
#' @param verbose a logical, whether or not to print out progress
#' @param writeto if not NULL, will rewrite results cumulatively after each combination of parameters is finished to the file with this path.
#' @details All parameters of the classifier model, out of which the grid will be formed, are assumed to be passed to the classifier as single values (not vectors, matrices, etc). If you want to grid-search parameters that your model takes as, say, a vector, or if your model takes the formula and the data as arguments with different names, use a wrapper function for your model.
#' @return a list with components, indexed in the same order: \item{class.err}{matrix of classification errors per class (each class is a column)}
#' \item{total.err}{a vector of total classification errors}
#' \item{confs}{a list of confusion matrices}
#' \item{gridparam}{a data frame of the grid of the parameters. It provides the connection between the index (common for the preceding list components) and the parameter values.}
#' \item{kfold}{a list containing the number of folds and repetitions in the kfold validation}
#' @export
#'
#' @examples #
# #Create fake data set and formula of prediction
# data=data.frame(x=sample(c(0,1),100,replace=TRUE),p1=runif(100),p2=runif(100))
# data$x=factor(data$x)
# formula=as.formula('x~p1+p2')
#
# #Create list of parameter values to try in the classifier
# params=list(cw=c(0.7, 0.8, 0.9), cost=c(100, 500, 1000), gamma=c(1e-6, 0.01, 0.05, 1, 10))
#
#
# #The SVM classifier e1071::svm takes class weights as a vector argument, so create a wrapper function
# mysvm=function(formula, data, cw, ...){
#    return(e1071::svm(formula=formula, data=data, class.weights=c('1'=cw, '0'=1-cw)))
# }
#
# #Find classification errors for each combination of parameters
# ans=gridclassifier(data=data, model=mysvm, formula=formula, params=params)
gridclassifier=function(data,model,formula,params,k=1,repetitions=1,verbose=FALSE, writeto=NULL){

  temp=all.vars(formula)
  outcome_var=temp[1]
  pred_vars=temp[-1]

  kf=bitsandends::kfoldind(nrow(data),k=k,repetitions=repetitions)

  gridparam=expand.grid(params, stringsAsFactors = FALSE)
  tic=proc.time()[3]

  confs=rep(list(NULL),nrow(gridparam))

  for(i in 1:nrow(gridparam)){

    conf.matrix=rep(list(NULL),length(kf))

    for(j in 1:length(kf)){
      parlist=c(list(formula=formula,data=data[kf[[j]]$training,]),gridparam[i,])
      fit=do.call(model,parlist)

      dat.validation=data[kf[[j]]$validation,]
      pred=predict(fit,dat.validation[,pred_vars,drop=FALSE])
      conf.matrix[[j]]=table(pred=pred,true=dat.validation[,outcome_var,drop=TRUE])

    }

    conf.matrix=Reduce('+',conf.matrix)
    conf.matrix=conf.matrix/length(kf)

    if(verbose){
      cat('Done with combination',i,'out of',nrow(gridparam),'\n')
      cat(round(proc.time()[3]-tic),'seconds \n')
    }
    confs[[i]]=conf.matrix
    if(!is.null(writeto)){
      saveRDS(c(class.errors(confs[1:i]),list(gridparam=gridparam[1:i,],kfold=list(k=k,repetitions=repetitions))),file=writeto)
    }

  }

  return(c(class.errors(confs),list(gridparam=gridparam, kfold=list(k=k,repetitions=repetitions))))

}


class.errors=function(confs){
  class.err=do.call(rbind,lapply(confs,function(conf){
    c(conf[2,1]/(conf[1,1]+conf[2,1]),conf[1,2]/(conf[1,2]+conf[2,2]))
  }))
  colnames(class.err)=colnames(confs[[1]])
  total.err=sapply(confs,function(conf){
    1-sum(diag(conf))/sum(conf)
  })

  return(list(class.err=class.err,total.err=total.err,confs=confs))
}
