require(data.table)
library(optparse)
library(doSNOW)
#library(foreach)
#library(Matrix)
#library(doParallel)
library(parallel)
#library(slam)
#library(skmeans)
#library(e1071)
#library(iterators)
#library(bit64)
library(HOD)

setwd(".")

option_list <- list(
  make_option(c("-d", "--discretisation"), type = "character", default="simple",
              help="Discretisation method to use [default=simple]"),
  make_option(c("-e", "--save_discretised"), type = "character", default="y",
              help="Save data directly after discretisation into file [default y]"),
  make_option(c("-f", "--discretised_filename"), type = "character", default="data_discretised.csv",
              help="Filename to save data directly after discretisation [default data_discretised.csv]"),
  make_option(c("-t", "--threads"), type = "integer", default=2,
              help="Number of threads to use [default=2]"),
  make_option(c("-s", "--samples"), type="integer", default=6,
              help="Number of samples to use for training [default 6]"),
  make_option(c("-o", "--optimal_k_samples"), type="integer", default=3,
              help="Number of samples to use for determining optimal k [default 3]"),
  make_option(c("-l", "--sample_size"), type="integer", default=10000,
              help="Number of events in one sample [default 10000]"),
  make_option(c("-m", "--min_clusters"), type="integer", default=2,
              help="Minimal number of clusters to use for k-means [default 2]"),
  make_option(c("-x", "--max_clusters"), type="integer", default=128,
              help="Maximum number of clusters to use for k-means [default 128]"),
  make_option(c("-i", "--input_file"), type="character", default="../kddcup.data_first400k_0.1attacks_corrected_header",
              help="Input file [default ../kddcup.data_first400k_0.1attacks_corrected_header]"),
  make_option(c("-c", "--column_types"), type="character", default="../kddcup.column_types",
              help="Input files with column types [default ../kddcup.column_types]"),
  make_option(c("-r", "--results_file"), type="character", default="bootstrap_skmeans_svm_anomalies.csv",
              help="File to write results [default bootstrap_skmeans_svm_anomalies.csv]"),
  make_option(c("-a", "--results_file_with_data"), type="character", default="bootstrap_skmeans_svm_anomalies_sorted_data.csv",
              help="File to write results with data [default bootstrap_skmeans_svm_anomalies_sorted_data.csv]"),
  make_option(c("-q", "--start_from_anomaly"), type="character", default="n",
              help="If <y> then read data from RDS and start from anomaly detection directly [default n]"),
  make_option(c("-k", "--optimal_k_rds"), type="character", default="optimal_k.rds",
              help="RDS dump with optimal k value, only needed if start_from_anomaly==y [default optimal_k.rds]"),
  make_option(c("-u", "--original_data_rds"), type="character", default="optimal_data.rds",
              help="RDS dump with original_data, only needed if start_from_anomaly==y [default optimal_data.rds]"),
  make_option(c("-p", "--column_list_rds"), type="character", default="columnlist.rds",
              help="RDS dump with normalised data, only needed if start_from_anomaly==y [default columnlist.rds]")
)

parser <- OptionParser(usage="%prog [options]", option_list=option_list)
opt <- parse_args(parser)

### Set parameters
discretisation <- opt$discretisation
# 
threads = opt$threads
# minimum of 2 samples required!!! Otherwise RowSums function in anomaly detection won't work!!!
samples = opt$samples
optimal_k_samples = opt$optimal_k_samples
samplesize = opt$sample_size
min_clusters = opt$min_clusters
max_clusters = opt$max_clusters
# filenames
kddcup_with_header_filename <- opt$input_file
kddcup_columntypes_filename <- opt$column_types
kddcup_anomalies_filename <- opt$results_file
kddcup_anomalies_original_filename <- opt$results_file_with_data
start_from_anomaly <- opt$start_from_anomaly

message("Options to use: ",discretisation,",",threads,",",samples,",",optimal_k_samples,",",samplesize,",",min_clusters,",",max_clusters,",",kddcup_with_header_filename,",",kddcup_columntypes_filename,",",kddcup_anomalies_filename,",",kddcup_anomalies_original_filename)

cl <- makeCluster(threads,outfile = "")
registerDoSNOW(cl)

if (start_from_anomaly == "n") {

data <- fread(kddcup_with_header_filename,header = TRUE)
original_data <- as.data.frame(data)
original_data$EVENTNUMBER<-seq.int(nrow(original_data))  

#drop timestamp columns, since in this particular implementation (for analysis of KDDCup1999 and UNSW NB15 data) a timestamp is not yet supported
column_types <- fread(kddcup_columntypes_filename,header = FALSE)
column_types <- as.data.frame(column_types)
data <- data[,which(column_types[,2]!="timestamp.")]
  
### Analysis
message("Normalising data")
# it should be no problem to forward data with EVENTNUMBER column added as last column, since ReadAndNormalise function should only process preset types of columns (specific values of class column)
columnsList <- ReadAndNormalise(original_data,column_types,discretisation)
message("done")

discretised_numeric_columns <- columnsList$discretised_numeric_columns
mapped_symbolic_columns <- columnsList$mapped_symbolic_columns
class_columns <- columnsList$class_columns

class_column <- class_columns[,ncol(class_columns)]

if (opt$save_discretised == "y") {
# combine everything back in one data frame
data <- base::cbind(discretised_numeric_columns,mapped_symbolic_columns,class_columns)
# write normalised data
write.table(data,file=opt$discretised_filename,row.names=FALSE,append=FALSE)
}

print("Finding optimal k for analysis")
optimal_k_list <- determineOptimalK(discretised_numeric_columns,mapped_symbolic_columns,discretisation,optimal_k_samples,samplesize,min_clusters,max_clusters)

optimal_k <- (optimal_k_list$optimal_k_train + optimal_k_list$optimal_k_data) %/% 2

saveRDS(columnsList,opt$column_list_rds)
saveRDS(optimal_k,opt$optimal_k_rds)
saveRDS(original_data,opt$original_data_rds)

} else {
  optimal_k <- readRDS(opt$optimal_k_rds)
  columnsList <- readRDS(opt$column_list_rds)
  original_data <- readRDS(opt$original_data_rds)
  discretised_numeric_columns <- columnsList$discretised_numeric_columns
  mapped_symbolic_columns <- columnsList$mapped_symbolic_columns
}

message("Optimal k: ",optimal_k)

print("Starting anomaly detection...")
anomalies <- findAnomalousClusters(discretised_numeric_columns,mapped_symbolic_columns,discretisation,samples,samplesize,optimal_k,original_data)
print("anomaly detection finished")

# save anomalies to file
file.remove(kddcup_anomalies_filename)
for (anomaly in anomalies) {
  write.table(
    c(anomaly[[2]],t(anomaly[[1]])),file = kddcup_anomalies_filename,append = TRUE, row.names = FALSE, col.names = FALSE, sep = ","
  )
}

# save data to file
file.remove(kddcup_anomalies_original_filename)
for (anomaly in anomalies) {
  write.table(anomaly[[2]],file = kddcup_anomalies_original_filename,append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
  write.table(original_data[original_data$EVENTNUMBER %in% anomaly[[1]],],file = kddcup_anomalies_original_filename,append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
}

stopCluster(cl)

