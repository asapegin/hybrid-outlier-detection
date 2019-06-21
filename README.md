# hybrid-outlier-detection

## Unsupervised generic outlier detection using ensemble of one-class SVMs trained on concept vetors from sperical k-means executed on bootstrapped data samples after conversion into vector space (into sparse matrix with one-hot encoding).

This is a generic unsupervised outlier detection algorithm which works on any/mixed data (both categorical and numerical). For the information on the algorithm, please read:

* Section 4.3.1 of Andrey Sapegin. "High-Speed Security Log Analytics Using Hybrid Outlier Detection". Doctoral thesis, Universität Potsdam, 2019. [PDF](https://publishup.uni-potsdam.de/frontdoor/index/index/docId/42611)
* Section 4.3.2 of Andrey Sapegin, David Jaeger, Feng Cheng and Christoph Meinel. "Towards a system for complex analysis of security events in large-scale networks". Elsevier Computers&Security, Volume 67, June 2017, pages 16-34. [PDF](https://authors.elsevier.com/a/1Ue8s_3pcoTuNe)

In short, in order to train ocSVMs, several samples (containing in total 5-20% of dataset) should be randomly selected from the whole dataset. Each of this samples will be converted into sparse matrix using one-hot encoding. In order to perform such conversion, categorical values will be mapped to numbers, numerical values will be discretized (see Section 4.3.3 of "Towards a system for complex analysis of security events in large-scale networks" or 4.2.1.1 of "High-Speed Security Log Analytics Using Hybrid Outlier Detection"). After that all samples will be processed in parallel with sperical k-means in order to identify concept vectors of clusters. Ensemble of ocSVM will be trained on this concept vectors. Then the whole process will repeat for the whole dataset (that will be divided into samples of the same size as were used for training phase). If concept vector will be marked by all ocSVMs as outlier, the whole cluster will be marked as anomaly and assigned an outlier score (scaled sum of decision values).

## Usage

This algorithm was tested on Ubuntu 14.04 64-bit using R 3.2.5

### Example

Clone the repository and run ./launcher_test.sh in order to install HOD package into R and try the algorithm on first 40,000 records from KDD Cup 1999 dataset.

Main.R file contains an example on how to use outlier detection functions from the HOD package.
