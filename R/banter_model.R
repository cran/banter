#' @title Events classification model
#' @description Create classification model for events
#'
#' @slot data data.frame of event.ids and call.ids for calls in detector
#' @slot detectors list of \code{detector_model} objects
#' @slot model.data data used to create classification model
#' @slot model classification model
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @keywords internal
#' 
banter_model <- methods::setClass(
  "banter_model",
  slots = c(
    data = "data.frame",
    detectors = "listOrNull",
    model.data = "dfOrNull",
    model = "classifier",
    sampsize = "numOrNull",
    timestamp = "dateOrNull"
  )
)

methods::setValidity(
  "banter_model",
  method = function(object) {
    valid <- NULL
    
    # Check @ids ------------------------
    if(!all(c("event.id", "species") %in% colnames(object@data))) {
      valid <- c(valid, "slot '@data' must have 'event.id' and 'species' columns")
    }
    
    # Check @detectors
    if(!is.null(object@detectors)) {
      if(length(obect@detectors) == 0) {
        valid <- c(valid, "slot '@detectors' can't be an empty list")
      } else {
        classes <- unlist(sapply(object@detectors, class))
        if(any(classes != "detector_model")) {
          valid <- c(valid, "all elements in slot '@detectors' must be a 'detector_model' object")
        }
      }
    }
    
    if(is.null(valid)) TRUE else valid
  }
)

methods::setMethod(
  "show",
  "banter_model",
  function(object) {
    df <- numEvents(object) 
    df <- rbind(
      df, 
      data.frame(species = "Overall", num.events = sum(df$num.events))
    )
    
    pct.correct <- modelPctCorrect(object)
    if(!is.null(pct.correct)) {
      df <- dplyr::left_join(df, pct.correct, by = "species")
    }
    
    if(!is.null(object@timestamp)) {
      cat("Event model run completed at", format(object@timestamp))
    }
    cat("\nNumber of events and model classification rate:\n")
    print(df, digits = 4)
  }
)