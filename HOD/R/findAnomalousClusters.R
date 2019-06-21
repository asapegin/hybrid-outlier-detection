#' Function that performs training and application of the trained model on the data
#'
#' @param discretised_numeric_columns,mapped_symbolic_columns,discretisation,threads,samples,samplesize,optimal_k,data
#' @return a list of anomalies sorted by sum of decision values
#' @keywords
#' @import doSNOW Matrix slam skmeans e1071 iterators data.table bit64
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

#source("convertToSparseMatrix.R")
#source("vectorList2Matrix.R")

# minimum of 2 samples should be set as a parameter, otherwise detection will fail on rowSums function, since it expects an array of SVM output, not a single column from single SVM.
findAnomalousClusters <- function(discretised_numeric_columns,mapped_symbolic_columns,discretisation,samples,samplesize,optimal_k,data){
  
  # store maximum value for each column
  maxxDN <- mclapply(as.data.frame(discretised_numeric_columns),max)
  maxxMS <- mclapply(as.data.frame(mapped_symbolic_columns),max)
  
  # create train samples with bootstrapping
  m = nrow(discretised_numeric_columns)
  sampledRecords <- foreach(i = 1:samples) %dopar% {
    sample(1:m, size = samplesize, replace = FALSE)
  }
  trainSamplesDiscretisedNumeric <- foreach(i = 1:samples) %dopar% {
    discretised_numeric_columns[sampledRecords[[i]],]
  }
  trainSamplesMappedSymbolic <- foreach(i = 1:samples) %dopar% {
    mapped_symbolic_columns[sampledRecords[[i]],]
  }
  
  print("Training SVMs...")
  timer <- proc.time()
  modelDataSvm <-
    foreach(
      i = 1:samples, .packages = c("Matrix","e1071","foreach","iterators","skmeans","nsprcomp"), .export=c("convertToSparseMatrix","vectorList2Matrix")
    ) %dopar% {
      # convert each train sample to sparse matrix
      convertedSamples <- convertToSparseMatrix(trainSamplesDiscretisedNumeric[[i]],trainSamplesMappedSymbolic[[i]],discretisation,maxxDN,maxxMS)
      
      sparseX <- cBind(convertedSamples$sparseDiscretisedNumeric,convertedSamples$sparseMappedSymbolic)
      
      # PCA dimensionality reduction
      #sparseX <- nsprcomp(sparseX,scale=FALSE)
      #csums <- cumsum(sparseX$sdev^2 / sum(sparseX$sdev^2))
      #sparseX$x[,which(csums<0.95)]
      
      # find clusters in sparse matrix
      clusters <-
        skmeans(sparseX,optimal_k,method = 'pclust',control = list(nruns=3))
      
      # calculate concept vectors for all clusters
      concept_vectors <-
        foreach(j = 1:optimal_k, .combine = c) %do% {
          xmean <-
            Matrix::colMeans(sparseX[which(clusters$cluster == j),,drop =
                                       FALSE],sparseResult =
                               TRUE)
          # divide by norm (option: norm(as.matrix(xmean),"f"))
          xmean / sqrt(sum(xmean ^ 2))
        }
      
      # convert list of sparce concept vectors to sparse matrix
      # Option 1: extremely slow.
      #concept_vectors <- lapply(concept_vectors, as, "sparseMatrix")
      #do.call(rBind, concept_vectors)
      # Option 2.
      concept_vectors <- vectorList2Matrix(concept_vectors)
      
      # train one class SVM on concept vectors
      svm(
        concept_vectors,kernel = "radial",type = 'one-classification',y =
          NULL,scale = FALSE, decision.values = TRUE
      )
    }
    
  print("Training finished.")
  
  proc.time() - timer
  timer <- proc.time()
  
  print("Applying models on data...")
  anomalies <-
    foreach(
      i = 0:((m %/% samplesize) - 1), .combine = c, .packages = c("Matrix","e1071","foreach","skmeans"), .export=c("convertToSparseMatrix","vectorList2Matrix")
    ) %dopar% {
      p <- i * samplesize + 1
      q <- (i + 1) * samplesize
      if (q > m) {
        q <- m
      }
      
      # convert each sample to sparse matrix
      convertedSamples <- convertToSparseMatrix(discretised_numeric_columns[p:q,],mapped_symbolic_columns[p:q,],discretisation,maxxDN,maxxMS)
      
      sparseX <- cBind(convertedSamples$sparseDiscretisedNumeric,convertedSamples$sparseMappedSymbolic)
      
      # PCA dimensionality reduction
      
      
      # find clusters in sparse matrix
      clusters <-
        skmeans(sparseX,optimal_k,method = 'pclust',control = list(nruns=3))
      
      # calculate concept vectors for all clusters
      concept_vectors <-
        foreach(j = 1:optimal_k, .combine = c) %do% {
          xmean <-
            Matrix::colMeans(sparseX[which(clusters$cluster == j),,drop =
                                       FALSE],sparseResult =
                               TRUE)
          # divide by norm (option: norm(as.matrix(xmean),"f"))
          xmean / sqrt(sum(xmean ^ 2))
        }
      
      concept_vectors <- vectorList2Matrix(concept_vectors)
      
      # get predictions for each concept vector from each SVM model
      predictDataSvm <-
        foreach(i = 1:samples, .combine = cbind, .packages = "e1071") %do% {
          pred <-
            predict(modelDataSvm[[i]], concept_vectors, decision.values = TRUE)
          dec_values <- attr(pred,"decision.values")
          cbind(pred,dec_values)
        }
      
      # index of predictions in the joined matrix
      # 1,3,5,7,...
      preds <- seq(1,ncol(predictDataSvm),2)
      # index of decision values in the joined matrix
      # 2,4,6,8,...
      dec_values <- seq(2,ncol(predictDataSvm),2)
      
      # find cluster numbers, where all SVMs predicted outlier
      predictions <- which(rowSums(predictDataSvm[,preds]) == 0)
      
      # copy these clusters to the LIST of outliers
      if (length(predictions) > 0) {
        outliers <- foreach(j = 1:length(predictions)) %do% {
          novels <-
            data[p:q,][which(clusters$cluster == predictions[[j]]),]$EVENTNUMBER
          # decision values
          dvs <- predictDataSvm[predictions[[j]],dec_values]
          # return outliers together with decision values
          list(novels,dvs)
        }
      } else {
        outliers <- NULL
      }
      
      outliers
    }
  
  # calibrate decision values
  decision_values <- foreach (anomaly = iter(anomalies), .combine=rbind) %do% {
    anomaly[[2]]
  }
  decision_values <- scale(decision_values, center = FALSE, scale = apply(decision_values, 2, sd, na.rm = TRUE))
  
  sumdv <- rowSums(decision_values)
  
  # replace decision values in anomalies with sums of calibrated values
  for (i in 1:length(anomalies)){
    anomalies[[i]][[2]] <- sumdv[i]
  }
  
  # Sort anomalies by sum of decision values
  anomalies <- anomalies[order(sumdv)]
  
  proc.time() - timer
  
  return(anomalies)
}
