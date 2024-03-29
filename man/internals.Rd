% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addBanterDetector.R, R/internals.R
\name{.rfFuncDetector}
\alias{.rfFuncDetector}
\alias{.checkValidStrings}
\alias{.checkModelName}
\alias{.stopIfNoDetectors}
\alias{internals}
\alias{.getSampsize}
\alias{.meanVotes}
\title{Detector randomForest function}
\usage{
.rfFuncDetector(params)

.checkValidStrings(x, label)

.checkModelName(x, model)

.stopIfNoDetectors(x)

.getSampsize(x, n, warn.label)

.meanVotes(x)
}
\description{
Detector randomForest function

Checks to make sure 'x' contains syntactically valid and unique characters

Checks to make sure 'model' is a valid model name
x is a banter_model object

Check that detectors are present
x is a banter_model object

Produces a vector of equal sample sizes for balanced Random Forest model.
x is a vector of species or table of species frequencies
n is desired sample size

Returns matrix of mean species vote percentage by event
x is a list of data.frames with species probability assignments
}
\keyword{internal}
