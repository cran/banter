#' @title Extract Random Forest Model Data
#' @description Extract BANTER event data used for the Random Forest model.
#' 
#' @param x a \code{\link{banter_model}} object.
#' 
#' @return a \code{\link{randomForest}} model object.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' data(train.data)
#' # initialize BANTER model with event data
#' bant.mdl <- initBanterModel(train.data$events)
#' # add all detector models
#' bant.mdl <- addBanterDetector(
#'   bant.mdl, train.data$detectors, 
#'   ntree = 50, sampsize = 1, num.cores = 1
#' )
#' # run BANTER event model
#' bant.mdl <- runBanterModel(bant.mdl, ntree = 1000, sampsize = 1)
#' 
#' event.df <- getBanterModelData(bant.mdl)
#' head(event.df)
#' 
#' @export
#' 
getBanterModelData <- function(x) x@model.data