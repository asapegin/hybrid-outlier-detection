% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convertToSparseMatrix.R
\name{convertToSparseMatrix}
\alias{convertToSparseMatrix}
\title{Function to convert normalised data (discretised continuous numeric columns and mapped symbolic columns) into sparse matrix for later application of spherical k-means}
\usage{
convertToSparseMatrix(trainSamplesDiscretisedNumeric,
  trainSamplesMappedSymbolic, discretisation, maxxDN, maxxMS)
}
\arguments{
\item{trainSamplesDiscretisedNumeric, trainSamplesMappedSymbolic, discretisation, maxxDN}{(maximum value in the data among discretised numeric columns),maxxMS (maximum value in the data among mapped symbolic columns)}
}
\value{
list with 2 values: sparse discretised numeric columns, sparse mapped symbolic columns
}
\description{
Function to convert normalised data (discretised continuous numeric columns and mapped symbolic columns) into sparse matrix for later application of spherical k-means
}
