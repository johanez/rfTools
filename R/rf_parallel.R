#' Parallel random forest using foreach
#'
#' @param ncore Number of cores
#' @param ... additional arguments to RandomForest
#' @inheritParams randomForest::randomForest
#' @return A randomForest object. No confusion matrix, err.rate, mse and rsq components are returned!
#' @export
#' @importFrom foreach "%dopar%"
#' @details Parallel random forest using foreach and the original \code{\link[randomForest]{randomForest}}. 
#' The number of trees is devided over the cores and grown parallel. The returned rf objects are combiend 
#' using \code{\link[randomForest]{combine}}
#' @seealso \code{\link[randomForest]{randomForest}} for the original sequential function. \code{\link[foreach]{foreach}}
#' @examples 
#' ## Classification:
#' ##data(iris)
#' set.seed(71)
#' iris_rfp <- rf_parallel(Species ~ ., data=iris)
#' print(iris_rfp)
#' table(iris_rfp$predicted, iris$Species)
rf_parallel <- function(x, y=NULL, ntree=500, ncore=parallel::detectCores(), ...){
  # optional parallel backend
  if (ncore > 1) {
    # check if already registered
    if (foreach::getDoParWorkers()!= ncore) doParallel::registerDoParallel(ncore)
  } else foreach::registerDoSEQ()
  rf <- foreach::foreach(ntree_i=rep(floor(ntree/ncore), ncore), 
                .combine=randomForest::combine, 
                .packages='randomForest') %dopar% {
                  randomForest::randomForest(x, y, ntree=ntree_i, ...)
                }
  return(rf)
}