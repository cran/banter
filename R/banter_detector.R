#' @title Detector classification model
#' @description Create classification model for a detector
#'
#' @param name detector name
#' @param detector.data measurement data for detections
#' @param event.data data.frame associating event.id with species
#' @param ntree number of trees in model
#' @param sampsize number or fraction of samples to use in each tree
#'
#' @slot name detector name
#' @slot ids data.frame of event.ids and call.ids for calls in detector
#' @slot model classification model
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @importFrom methods setClass setValidity setMethod new
#' @importFrom randomForest randomForest
#' 
#' @keywords internal
#' 
banter_detector <- methods::setClass(
  "banter_detector",
  slots = c(
    name = "character",
    ids = "data.frame",
    model = "classifier",
    sampsize = "numOrNull"
  )
)

methods::setValidity(
  "banter_detector",
  method = function(object) {
    valid <- NULL
    
    # Check @name -----------------------
    if(length(object@name) != 1) {
      valid <- c(valid, "slot '@name' must have length 1")
    }
    
    # Check @ids ------------------------
    if(!all(c("event.id", "call.id") %in% colnames(object@ids))) {
      valid <- c(valid, "slot '@ids' must have 'event.id' and 'call.id' columns")
    }
    
    if(is.null(valid)) TRUE else valid
  }
)