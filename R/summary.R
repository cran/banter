#' @name summary
#' @title BANTER Classifier Model Summary
#' @description Display summaries for event and detector models
#' 
#' @param object a \code{\link{banter_model}} object.
#' @param model name of model to summarize. Default is \code{"event"} 
#'   to summarize the event-level model. Can also be name of a detector.
#' @param n number of final iterations to summarize OOB error rate for. If 
#'   between 0 and 1 is taken as a proportion of chain.
#' @param bins number of bins in inbag histogram.
#' @param ... ignored.
#' 
#' @return In the plot that is created, the upper panel shows the trace of the 
#'   Random Forest model OOB rate across sequential trees in the forest. The
#'   lower plot shows a frequency histogram of the number of times each sample 
#'   was inbag (used as training data in a tree in the forest). The vertical 
#'   red lines indicate the expected inbag rate for samples of each species.
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
#' summary(bant.mdl)
#' 
#' @exportMethod summary
methods::setGeneric("summary")

#' @name summary
#' @rdname summary
#' @method summary banter_model
#' @export 
#' 
summary.banter_model <- function(object, model = "event", n = 0.1, bins = 20, ...) {  
  print(object)
  rf <- getBanterModel(object, model)
  if(!is.null(rf)) {  
    sampsize <- if(model == "event") {
      object@sampsize
    } else {
      object@detectors[[model]]@sampsize
    } 
    
    cat("\nDistribution of percent correctly classified overall in last 'n' trees:\n")
    print(round(.rfPctCorrectSmry(rf, n) * 100, 2))
    cat("\nSample inbag rate distribution:\n") 
    print(rbind(
      expected = round(summary(as.vector(sampsize / table(rf$y))), 3),
      observed = round(summary(1 - (rf$oob.times / rf$ntree)), 3)
    ))
    cat("\nConfusion matrix:\n")
    print(round(rfPermute::confusionMatrix(rf), 2))
    
    gridExtra::grid.arrange(
      rfPermute::plotRFtrace(rf, plot = FALSE),
      rfPermute::plotInbag(rf, sampsize = sampsize, bins = bins, plot = FALSE),
      nrow = 2
    )
  }
}

#' @name summary
#' @rdname summary
#' @aliases summary,banter_model-method
methods::setMethod("summary", "banter_model", summary.banter_model) 

#' @rdname internals
#' @keywords internal
#' 
.rfPctCorrectSmry <- function(rf, n = 0.1) {
  x <- rf$err.rate
  if(dplyr::between(n, 0, 1)) n <- ceiling(n * nrow(x))
  correct <- 1 - x[(nrow(x) - n + 1):nrow(x), "OOB"]
  c(n = length(correct), round(summary(correct), 3))
}
