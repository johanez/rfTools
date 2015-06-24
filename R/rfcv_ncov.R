# modified rfcv that returns the rf witht he optimal ncov
# for both regression & classification
# todo: 
#   parallelize cv instead of rf? -> (no double calc of top rf))

#' Title
#' A modified \code{\link[randomForest]{rfcv}} that returns the rf with he optimal ncov and the selected variables 
#' @inheritParams randomForest::rfcv
#' @param ntree Number of trees for rf models.
#' @param ncore Number of cores to use (set to 1 for sequential computing).
#' @param verbose Should progress indication be printed?
#' @param ... additional arguments to \code{\link[randomForest]{randomForest}}
#'
#' @return A list with 
#' \itemize{
#'  \item The optimized rf object
#'  \item A data.frame with the OOB and CV performance measures for the optimized and rf model 
#'      using all variables (Regression) or the OOB and CV accuracy assesment results (Classification) 
#'  \item A character vector with the names of the selected variables. 
#' }
#' @export
#'
#' @examples
#' #' ## Classification:
#' ##data(iris)
#' set.seed(71)
#' iris_rfncov <- rfcv_ncov(trainx=iris[ ,names(iris) != "Species"], 
#'                          trainy = iris$Species, ncore = 2)
#' print(iris_rfncov)
rfcv_ncov<- function (trainx, trainy, cv.fold = 5, scale = "log", step = 0.5, 
                      mtry = function(p) max(1, floor(sqrt(p))), recursive = FALSE, 
                      ntree=500, ncore=min(parallel::detectCores(),6), verbose=TRUE,
                      ...) {
  classRF <- is.factor(trainy)
  n <- nrow(trainx)
  p <- ncol(trainx)
  if (scale == "log") {
    k <- floor(log(p, base = 1/step))
    n.var <- round(p * step^(0:(k - 1)))
    same <- diff(n.var) == 0
    if (any(same)) 
      n.var <- n.var[-which(same)]
    if (!1 %in% n.var) 
      n.var <- c(n.var, 1)
  }
  else {
    n.var <- seq(from = p, to = 1, by = step)
  }
  k <- length(n.var)
  cv.pred <- vector(k, mode = "list")
  for (i in 1:k) cv.pred[[i]] <- trainy
  if (classRF) {
    f <- trainy
  }
  else {
    f <- factor(rep(1:5, length = length(trainy))[order(order(trainy))])
  }
  nlvl <- table(f)
  idx <- numeric(n)
  for (i in 1:length(nlvl)) {
    idx[which(f == levels(f)[i])] <- sample(rep(1:cv.fold, 
                                                length = nlvl[i]))
  }
  if(verbose) cat("CV folds: ")
  for (i in 1:cv.fold) {
    #  replaced rf by rf_parallel
    all.rf <- rf_parallel(x=trainx[idx != i, , drop = FALSE], 
                          y=trainy[idx != i], 
                          ntree=ntree,
                          ncore=ncore,
                          xtest=trainx[idx == i, , drop = FALSE], 
                          ytest=trainy[idx == i], mtry = mtry(p), importance = TRUE, 
                          keep.forest=FALSE,
                          ...)
    cv.pred[[1]][idx == i] <- all.rf$test$predicted
    impvar <- (1:p)[order(all.rf$importance[, 1], decreasing = TRUE)]
    
    for (j in 2:k) {
      imp.idx <- impvar[1:n.var[j]]
      #  replaced rf by rf_parallel
      sub.rf <- rf_parallel(x= trainx[idx != i, imp.idx, drop=F],
                            y=trainy[idx != i], 
                            ntree=ntree,
                            ncore=ncore,
                            xtest= trainx[idx == i, imp.idx, drop = FALSE], 
                            ytest= trainy[idx == i], 
                            mtry = mtry(n.var[j]), importance = recursive, 
                            keep.forest=FALSE,
                            ...)
      
      cv.pred[[j]][idx == i] <- sub.rf$test$predicted
      if (recursive) {
        impvar <- (1:length(imp.idx))[order(sub.rf$importance[, 
                                                              1], decreasing = TRUE)]
      }
      NULL
    }
    NULL
    if(verbose) cat(i,", ")
  }
  if(foreach::getDoParRegistered()) doParallel::stopImplicitCluster()
  # calculate reported measures
  if (classRF) {
    error.cv <- sapply(cv.pred, function(x) mean(trainy != x))
    # calculate avg. ac measures for all folds 
    ac_mes_cv <- sapply(cv.pred, function(x){
      em <- table(trainy, x)
      ac_mes <- unlist(ac_measures_from_em(em))
      return(ac_mes)
    })
    ac_mes_cv <- rowMeans(ac_mes_cv)
  }
  else {
    error.cv <- sapply(cv.pred, function(x) mean((trainy - x)^2))
  }
  names(error.cv) <- names(cv.pred) <- n.var
  
  
  n_top <- n.var[which.min(error.cv)]
  # rf with the selected variabels and all variables
  # use seq. random forest here, to keep OOB error est.
  rf_top <- randomForest::randomForest(trainx[, impvar[1:n_top], drop = FALSE], trainy, 
                         importance = T, ntree=ntree,
                         ...)
  rf_all <- randomForest::randomForest(trainx, trainy, 
                         importance = T, ntree=ntree,
                         ...)
  if (classRF){
    ac_mes_oob <- unlist(ac_measures_from_em(rf_top$confusion[,-(nlevels(trainy)+1)]))
    res <- data.frame(rbind(ac_mes_oob, ac_mes_cv))
    names(res) <- c("OA", paste(c("PA","UA"), rep(levels(trainy),each=2)))
    if(verbose)  print.data.frame(res)
    return(list(rf=rf_top, res, impvar=names(trainx)[impvar[1:n_top]]))
  } else {
    
    res <- data.frame(row.names=c("All cov.", "Imp. Cov."), 
                      ncov=c(ncol(trainx), n_top),
                      rsqOOB=c(rf_all$rsq[ntree],  rf_top$rsq[ntree]), 
                      mseOOB=c(rf_all$mse[ntree],  rf_top$mse[ntree]),
                      rsqCV=c(1 - (error.cv[1]/var(trainy)), 1 - (error.cv[which.min(error.cv)]/var(trainy))),
                      mseCV=c(error.cv[1], error.cv[which.min(error.cv)])
    )
    if(verbose)  print.data.frame(res)
    return(list(rf=rf_top, res=res, impvar=names(trainx)[impvar[1:n_top]]))
  }
}
