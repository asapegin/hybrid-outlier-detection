#' Function for reading and normalising mixed data (continuous numeric and symbolic)
#'
#' @param data,column_types_filename,discretisation,threads
#' @return a tuple with 3 matrices: discretised_numeric_columns, symbolic_columns_mapped_to_category, class_columns
#' @keywords
#' @import data.table doSNOW foreach parallel
#' @export
#' @examples

#require(data.table)
#library("doSNOW")
#library("foreach")

#source("findOptimalK.R")

### Normalisation with discretisation of numerical features
# 2 types of feature discretisation are supported
#
ReadAndNormalise <- function(data,column_types,discretisation) {
  # DEBUG:
  #data <- data[1:1000,]
  
  class_columns <- data[,which(column_types[,2]=="class.")]
  
  # find numeric and symbolic columns
  numeric_columns <- data[,which(column_types[,2]=="continuous.")]
  symbolic_columns <- data[,which(column_types[,2]=="symbolic.")]
  
  # delete columns with zeroes only
  csums = (colSums(abs(numeric_columns)) != 0)
  
  if (FALSE %in% csums) {
    numeric_columns <- numeric_columns[,csums]
  }
  
  # map symbolic columns to category
  symbolic_columns <- as.data.frame(apply(symbolic_columns,2, function(x) as.numeric(factor(x))))
  
  # delete columns with 0 variance for symbolic columns
  novar <- foreach (x = iter(symbolic_columns,by = 'col'), .combine = cbind) %dopar% {
    var(x) == 0
  }
  if (TRUE %in% novar) {
    symbolic_columns <- symbolic_columns[,-c(which(novar))]
  }
  
  # delete columns with 0 variance for numeric columns
  novar = foreach (x = iter(numeric_columns,by = 'col'), .combine = cbind) %dopar% {
    var(x) == 0
  }
  if (TRUE %in% novar) {
    numeric_columns <- numeric_columns[-c(which(novar))]
  }
  
  # discretisation of numeric columns with k-means
  discretised_numeric_columns <- foreach (i = 1:ncol(numeric_columns), .combine = cbind, .packages=c("foreach"), .export=c("findOptimalK")) %dopar% {
    timer <- proc.time()
    
    x <- numeric_columns[,i]
    
    message("Starting discretisation of column ",i)
    
    max_k <- length(unique(x))
    if (max_k > 128) {max_k <- 128}
    min_k <- 2
    stepping <- ceiling((max_k - min_k) / 63)
    
    # try k-means with different number of clusters and calculate similarity measure for each run of k-means
    measures <- foreach(k = seq(from = min_k,to = max_k,by = stepping), .combine = rbind) %do% {
      res <- kmeans(x,centers = k,iter.max = 100, nstart = 3)
      return(cbind(k,res$betweenss / res$totss))
    }
    
    measures <- as.data.frame(measures)
    names(measures) <- c("num_clusters","sim_measure")
    # determine optimal k for discretisation with k-means based on similarity measures
    optimal_k <- findOptimalK(measures)
    
    # Debug
    message("optimal k for discretisation: ",optimal_k)
    
    res <- kmeans(x,optimal_k,iter.max = 100, nstart = 3)
    
    if (discretisation == "advanced"){
    # replace each value NOT with cluster number, but with scaled to unit length (from 0 to 1 using min-max scaling) values of points in cluster
    # split one column into k new columns with scaled values
       newcolumns <- foreach (c = 1:optimal_k, .combine= cbind) %do%{
         newcol <- rep(0,length(x))
    
         min_x <- min(x[which(res$cluster == c)])
         max_x <- max(x[which(res$cluster == c)])
         if (max_x == min_x){
           newcol[which(res$cluster == c)] <- 1
         }else{
          newcol[which(res$cluster == c)] <- (x[which(res$cluster == c)] - min_x) / (max_x - min_x) * 0.5 + 0.5
         }
         #newcol[which(res$cluster != c)] <- 0
         
         return(newcol)
       }
       message("discretization of column ", i, " finished")
       print(proc.time() - timer)   
       
       return(newcolumns)
    }
    else {
      message("discretization of column ", i, " finished")
      print(proc.time() - timer)
      
      # replace each value with cluster number
      return(res$cluster)
    }
  }

  if (discretisation != "advanced"){
    discretised_numeric_columns <- as.data.frame(discretised_numeric_columns)
    names(discretised_numeric_columns) <- names(numeric_columns)
  }
  
  # combine everything back in one data frame
  #data <- base::cbind(discretized_numeric_columns,symbolic_columns,class_column)
  
  columnsList <- list("discretised_numeric_columns" = discretised_numeric_columns, "mapped_symbolic_columns" = symbolic_columns, "class_columns" = as.data.frame(class_columns))
  
  return(columnsList)
}
