#' @title Detector Names
#' @description Return names of detectors loaded in BANTER model.
#' 
#' @param x a \code{\link{banter_model}} object.
#' 
#' @return a vector of names.
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
#' getDetectorNames(bant.mdl)
#' 
#' @export
#' 
getDetectorNames <- function(x) names(x@detectors)