#' Function that evaluates an optimal nubmer of clusters per sample based on samples from both training data (randomly selected sample) and original data (divided into samples)
#'
#' @param discretised_numeric_columns,mapped_symbolic_columns,discretisation,threads,samples,samplesize,min_clusters,max_clusters
#' @return a tuple 2 values: optimal k determined from training data, optimal k determined from original data
#' @keywords
#' @import data.table doSNOW foreach parallel Matrix slam skmeans e1071 iterators bit64
#' @export
#' @examples

#library("foreach")
#library("doParallel")
#library("doSNOW")
#library("parallel")
#library("Matrix")
#library("slam")
#library("skmeans")
#library("e1071")
#library("iterators")
#require(data.table)
#require(bit64)

#source("calculateClusterSimilarities.R")
#source("findOptimalK.R")
#source("convertToSparseMatrix.R")

determineOptimalK <- function(discretised_numeric_columns,mapped_symbolic_columns,discretisation,samples,samplesize,min_clusters,max_clusters){

timer <- proc.time()

#### Analysis ####

m = nrow(discretised_numeric_columns)
# store maximum value for each column
maxxDN <- mclapply(as.data.frame(discretised_numeric_columns),max)
maxxMS <- mclapply(as.data.frame(mapped_symbolic_columns),max)

# create train samples with bootstrapping
sampledRecords <- foreach(i = 1:samples) %dopar% {
  sample(1:m, size = samplesize, replace = FALSE)
}
trainSamplesDiscretisedNumeric <- foreach(i = 1:samples) %dopar% {
  discretised_numeric_columns[sampledRecords[[i]],]
}
trainSamplesMappedSymbolic <- foreach(i = 1:samples) %dopar% {
  mapped_symbolic_columns[sampledRecords[[i]],]
}

proc.time() - timer

print("Finding optimal k on bootstraped data")
train_sims <-
  foreach (
    num_clusters = seq(from = min_clusters,to = max_clusters,by = 2), .combine = rbind, .packages = c("Matrix","e1071","foreach","iterators","skmeans"), .export=c("convertToSparseMatrix","calculateClusterSimilarities")
  ) %dopar% {
    #print(num_clusters)
    message("Calculating for ",num_clusters," clusters")
    similarities <-
      foreach(
        i = 1:samples, .combine = c, .packages = c("Matrix","e1071","foreach","iterators","skmeans"), .export=c("convertToSparseMatrix","calculateClusterSimilarities")
      ) %do% {
        
        #timer <- proc.time()
        #print("conversion to sparse matrix...")

        # convert each train sample to sparse matrix
        convertedSamples <- convertToSparseMatrix(trainSamplesDiscretisedNumeric[[i]],trainSamplesMappedSymbolic[[i]],discretisation,maxxDN,maxxMS)
        
        #print("conversion done")
        
        sparseX <- cBind(convertedSamples$sparseDiscretisedNumeric,convertedSamples$sparseMappedSymbolic)
        
        #print("sparse matrix constructed")
        
        #print(proc.time() - timer)
        
        #PCA?
        
        return(calculateClusterSimilarities(sparseX,num_clusters))
      }
    
    #find average similarity for all clusters
    sim_measure <- mean(similarities)
    
    return(c(num_clusters,sim_measure))
  }

proc.time() - timer
timer <- proc.time()

print("Finding optimal k on original data")
data_sims <-
  foreach (
    num_clusters = seq(from = min_clusters,to = max_clusters,by = 2), .combine = rbind, .packages = c("Matrix","e1071","foreach","iterators","skmeans"), .export=c("convertToSparseMatrix","calculateClusterSimilarities")
  ) %dopar% {
    #print(num_clusters)
    message("Calculating for ",num_clusters," clusters")
    similarities <-
      foreach(
        i = sample.int(((m %/% samplesize) - 1),samples), .combine = c, .packages = c("Matrix","e1071","foreach","skmeans"), .export=c("convertToSparseMatrix","calculateClusterSimilarities")
      ) %do% {
        p <- i * samplesize + 1
        q <- (i + 1) * samplesize
        if (q > m) {
          q <- m
        }
        
        # convert each sample to sparse matrix
        convertedSamples <- convertToSparseMatrix(discretised_numeric_columns[p:q,],mapped_symbolic_columns[p:q,],discretisation,maxxDN,maxxMS)
        
        sparseX <- cBind(convertedSamples$sparseDiscretisedNumeric,convertedSamples$sparseMappedSymbolic)
        
        # PCA dimensionality reduction?
        
        return(calculateClusterSimilarities(sparseX,num_clusters))
      }
    #find average similarity for all clusters
    sim_measure <- mean(similarities)
    
    return(c(num_clusters,sim_measure))
  }

proc.time() - timer

if (max_clusters - min_clusters >= 2){
  train_sims <- as.data.frame(train_sims)
  data_sims <- as.data.frame(data_sims)
} else{
  train_sims <- data.frame(t(unlist(train_sims)))
  data_sims <- data.frame(t(unlist(data_sims)))
}

names(train_sims) <- c("num_clusters","sim_measure")
names(data_sims) <- c("num_clusters","sim_measure")

optimal_k_train <- findOptimalK(train_sims)
optimal_k_data <- findOptimalK(data_sims)

optimal_k_list <- list("optimal_k_train" = optimal_k_train, "optimal_k_data" = optimal_k_data)

return(optimal_k_list)

}
