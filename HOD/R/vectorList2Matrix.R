#' Function to convert list of sparse vectors into sparse matrix. 
#'
#' @param vectorList
#' @return sparse matrix
#' @keywords
#' @import Matrix 
#' @export
#' @examples

#num_elements <-sum(unlist(lapply(vectorList, function(x) length(x@i))))
vectorList2Matrix <- function(vectorList) {
  sm_i <- NULL
  sm_j <- NULL
  sm_x <- NULL
  for (k in 1:length(vectorList)) {
    sm_i <- c(sm_i,rep(k,length(vectorList[[k]]@i)))
    sm_j <- c(sm_j,vectorList[[k]]@i)
    sm_x <- c(sm_x,vectorList[[k]]@x)
  }
  return (sparseMatrix(
    i = sm_i,j = sm_j,x = sm_x,dims = c(length(vectorList),vectorList[[1]]@length)
  ))
}
