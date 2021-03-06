% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/numCalls.R
\name{numCalls}
\alias{numCalls}
\alias{propCalls}
\title{Number and Proportion of Calls}
\usage{
numCalls(x, by = c("species", "event"))

propCalls(x, by = c("species", "event"))
}
\arguments{
\item{x}{a \code{\link{banter_model}} object.}

\item{by}{return summary by \code{"species"} or \code{"event"}.}
}
\description{
Number and proportion of calls in BANTER detector models
}
\examples{
data(train.data)
# initialize BANTER model with event data
bant.mdl <- initBanterModel(train.data$events)
# add all detector models
bant.mdl <- addBanterDetector(
  bant.mdl, train.data$detectors, 
  ntree = 50, sampsize = 1, num.cores = 1
)
# run BANTER event model
bant.mdl <- runBanterModel(bant.mdl, ntree = 1000, sampsize = 1)

# number of calls by species and event
numCalls(bant.mdl, "species")
numCalls(bant.mdl, "event")

# proportion of calls by species and event
propCalls(bant.mdl, "species")
propCalls(bant.mdl, "event")

}
\author{
Eric Archer \email{eric.archer@noaa.gov}
}
