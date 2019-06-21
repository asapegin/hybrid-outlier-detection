#' Function that finds optimal k (number of clusters) for train samples using "elbow" method
#'
#' @param c("num_clusters","sim_measure") is a tuple containing 2 lists: (1) different number of clusters in the data and (2) corresponding values of similarity measure
#' @return optimal_k
#' @export
#' @keywords
#' @examples

findOptimalK <- function(sims) {
  optimal_k <- sims[length(sims) %/% 2,]$num_clusters
  
  prevprev = 0
  prev = 0
  max2diff = 0
  
  for (i in 1:nrow(sims)) {
    if (i > 2) {
      #find second order central difference
      cdiff = sims[i,]$sim_measure - 2 * prev + prevprev
      if (cdiff < 0) {
        cdiff = -cdiff
      }
      if (max2diff < cdiff) {
        optimal_k = sims[i,]$num_clusters
        max2diff = cdiff
      }
    }
    prevprev = prev
    prev = sims[i,]$sim_measure
  }
  
  return(optimal_k)
  
}
