#' Calculate accuracy measures from error matrix
#'
#' @param em A confusion matrix (n x n)
#' @param em_dimnames Character vector of class names (n)
#'
#' @return A list given with the overall accuracy and per class users's and producers's accuracy
#' @export
#'
#' @examples
#' error_m <- matrix(c(99, 2, 23, 45), ncol = 2)
#' ac_ac_measures_from_em(error_m, c("a", "b"))
ac_measures_from_em <- function(em, em_dimnames = NULL){
  if (is.null(em_dimnames)) em_dimnames <- dimnames(em)[[1]]
  OA <- sum(diag(em)) / sum(em)
  PA <- numeric(ncol(em))
  UA <- numeric(nrow(em))
  names(OA) <- "Overall Accuracy"
  for (i in 1:ncol(em)){
    PA[i] <- em[i,i] / sum(em[, i])
  }
  for (i in 1:nrow(em)){
    UA[i] <- em[i,i] / sum(em[i, ])
  }
  class_ac <- rbind(PA, UA)
  rownames(class_ac) <- c("Prod. Accuracy", "Users Accuracy")
  if(!is.null(em_dimnames)) colnames(class_ac) <- em_dimnames
  res <- list(OA, class_ac)
  return(res)
}