% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clusterManyPage.R, R/functionInputs.R, R/plotClusterModule.R
\docType{data}
\name{InputModules}
\alias{InputModules}
\alias{RSECInputs}
\alias{clusterFunctionChoices}
\alias{clusterFunctionInputs}
\alias{combineManyInput}
\alias{dimReduceOptions}
\alias{makeDendrogramInput}
\alias{mergeClustersInput}
\alias{plotClustersInput}
\alias{sSBInputs}
\alias{specializedInputs}
\title{Internal modules used by shiny app}
\format{An object of class \code{character} of length 5.}
\usage{
sSBInputs(id, label = "SSB inputs")

dimReduceOptions

clusterFunctionChoices

clusterFunctionInputs(id, label = "inputs", isRSEC = FALSE)

specializedInputs(id, label = "Specializedinputs", isRSEC = FALSE)

RSECInputs(id)

combineManyInput(id, label = "cMInputs")

makeDendrogramInput(id, label = "cMInputs")

mergeClustersInput(id, label = "cMInputs")

plotClustersInput(id, label = "plotCluster inputs")

plotClustersInput(id, label = "plotCluster inputs")
}
\keyword{datasets}

