#' @title Plot BANTER Detector Traces
#' @description Plot traces of OOB error rates for detector Random Forest models
#' 
#' @param x a \code{\link{banter_model}} object.
#' @param detector names of models to plot. If set to \code{NULL}, traces for all
#'   models will be shown.
#' 
#' @seealso \code{\link[rfPermute]{plotRFtrace}}
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
#' 
#' plotDetectorTrace(bant.mdl)
#' 
#' @export
#' 
plotDetectorTrace <- function(x, detector = NULL) {  
  if(is.null(detector)) detector <- getDetectorNames(x)
  not.found <- setdiff(detector, getDetectorNames(x))
  if(length(not.found) > 0) {
    stop("can't find detectors: ", paste(not.found, collapse = ", "))
  }
  
  traces <- lapply(detector, function(d) {
    tr <- rfPermute::plotRFtrace(getBanterModel(x, d), FALSE)
    tr + 
      ggplot2::ggtitle(d) +
      ggplot2::theme(axis.title = ggplot2::element_blank())
  })
  traces$ncol <- 1
  traces$bottom <- "trees"
  traces$left = "error"
  
  p <- do.call(gridExtra::grid.arrange, traces)
  print(p)
  
  invisible(NULL)
}