#' Function to find clusters in sparse matrix and then for each cluster row calculate distance of that row to the concept vector of cluster
#'
#' @param sparseX (sparce matrix with all data),num_clusters
#' @return list with similarities from each row to its cluster center
#' @keywords
#' @import Matrix foreach skmeans
#' @export
#' @examples

calculateClusterSimilarities <- function(sparseX,num_clusters) {
  # find clusters in sparse matrix
  clusters <-
    skmeans(sparseX,num_clusters,method = 'pclust',control = list(nruns =
                                                                    2))
  
  # calculate concept vectors for all clusters
  cluster_similarities <-
    foreach(j = 1:num_clusters, .combine = c) %do% {
      cluster <- sparseX[which(clusters$cluster == j),,drop = FALSE]
      # mean
      xmean <- Matrix::colMeans(cluster,sparseResult = TRUE)
      # concept_vector
      concept_vector <- xmean / sqrt(sum(xmean ^ 2))
      # calculate cosine similarity vector (between each cluster row and mean)
      sqrtcv <- sqrt(Matrix::crossprod(concept_vector))
      foreach (
        k = 1:nrow(cluster), .combine = c, .packages = c("Matrix")
      ) %do% {
        cluster_row <- cluster[k,,drop = FALSE]
        cluster_row <-
          #sparseVector(rep(1,sum(cluster_row)),which(cluster_row[1,] > 0),length(cluster_row))
          sparseVector(cluster_row[which(cluster_row[1,] > 0)],which(cluster_row[1,] > 0),length(cluster_row))
        as.numeric(Matrix::crossprod(concept_vector,cluster_row) / (sqrtcv * sqrt(Matrix::crossprod(cluster_row))))
      }
    }
  
  return(cluster_similarities)
}
