require(data.table)
library(optparse)

option_list <- list(
  make_option(c("-r", "--roc_curve"), type="character", default="roc_curve.rds",
              help="File with ROC curve in RDS [default roc_curve.rds]"),
  make_option(c("-e", "--auc"), type="character", default="auc.txt",
              help="File with AUC value [default auc.txt]"),
  make_option(c("-o", "--original"), type="character", default="../../kddcup.data_first400k_0.1attacks_corrected_header",
              help="File with original data [default ../kddcup.data_first400k_0.1attacks_corrected_header]"),
  make_option(c("-a", "--file_with_anomalies"), type="character", default="bootstrap_skmeans_svm_anomalies.csv",
              help="File with ranked clusters of anomalies [default bootstrap_skmeans_svm_anomalies.csv]")
)

#debug
#setwd("~/Desktop/Automatic_threshold/skmeans2/KDDCup99/results_400k/simple_discretization_10_samples")

parser <- OptionParser(usage="%prog [options]", option_list=option_list)
opt <- parse_args(parser)

anomalies <- fread(opt$file_with_anomalies,data.table=FALSE)
anomalies <- anomalies[,1]
original <- as.data.frame(fread(opt$original,header=TRUE))
original$EVENTNUMBER<-seq.int(nrow(original))

# prepare data for ROC / AUC calculation
original_anomalies <- original[which(original$EVENTNUMBER %in% anomalies),c("EVENTNUMBER","class")]
original_anomalies$nstatus <- as.numeric(original_anomalies$class!="normal.")

num_anomalies <- length(which(anomalies >= 0))

predictions <- vector('numeric',num_anomalies)
i <- 0
for (anomaly in anomalies){
  # extract cluster score for the anomaly
  if (anomaly < 0){
    cluster_score <- abs(anomaly)
  }
  else{
    i <- i + 1
    predictions[i] <- cluster_score
  } 
}

anomalies_only <- anomalies[anomalies>=0]
predictions <- predictions[order(anomalies_only)]
labels <- original_anomalies[order(original_anomalies$EVENTNUMBER),c("nstatus")]

labels <- labels[order(predictions,decreasing=TRUE)]
predictions <- predictions[order(predictions,decreasing=TRUE)]

library("AUC")

roc_curve <- roc(predictions,factor(labels))
saveRDS(roc_curve,file=opt$roc_curve)
write.table(auc(roc_curve),file = opt$auc, append=FALSE, row.names = FALSE, col.names = FALSE)
