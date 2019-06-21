#' Function to convert normalised data (discretised continuous numeric columns and mapped symbolic columns) into sparse matrix for later application of spherical k-means
#'
#' @param trainSamplesDiscretisedNumeric,trainSamplesMappedSymbolic,discretisation,maxxDN (maximum value in the data among discretised numeric columns),maxxMS (maximum value in the data among mapped symbolic columns)
#' @return list with 2 values: sparse discretised numeric columns, sparse mapped symbolic columns
#' @keywords
#' @import Matrix foreach 
#' @export
#' @examples

#library("Matrix")
#library("foreach")

#source("vectorList2Matrix.R")

convertToSparseMatrix <- function(trainSamplesDiscretisedNumeric,trainSamplesMappedSymbolic,discretisation,maxxDN,maxxMS){
  # convert each train sample to sparse matrix
  if (discretisation == "simple"){
    sparseDiscretisedNumeric <- foreach (
      j = 1:ncol(trainSamplesDiscretisedNumeric), .combine = cBind, .packages = "Matrix") %do% {
        x <- trainSamplesDiscretisedNumeric[,j]
        sparseMatrix(which(x != 0),x[x != 0],dims = c(length(x),maxxDN[j]))
      }
  }
  else{
    sparseDiscretisedNumeric <- foreach (
      j = 1:ncol(trainSamplesDiscretisedNumeric), .combine = cBind, .packages = "Matrix", .export=c("cevtorList2Matrix")) %do% {
        values <- trainSamplesDiscretisedNumeric[,j]
        #sparseVector(x = values, i = which(values!=0), length = length(values))
#         if(length(which(values != 0)) == 0){
#           NULL
#         }else{
        sparseMatrix(i = which(values != 0), j = rep(1,length(which(values!=0))), x = values[values != 0],dims = c(length(values),1))
#         }
      }
    #sparseDiscretisedNumeric <- vectorList2Matrix(sparseDiscretisedNumeric)
  }
  
  sparseMappedSymbolic <- foreach (
    j = 1:ncol(trainSamplesMappedSymbolic), .combine = cBind, .packages = "Matrix"
  ) %do% {
    x <- trainSamplesMappedSymbolic[,j]
    sparseMatrix(which(x != 0),x[x != 0],dims = c(length(x),maxxMS[j]))
  }
  
  return(list("sparseDiscretisedNumeric" = sparseDiscretisedNumeric, "sparseMappedSymbolic" <- sparseMappedSymbolic))
}
