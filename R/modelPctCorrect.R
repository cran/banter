#' @title Model Percent Correct
#' @description Extract percent correctly classified by species for detector 
#'   and event models.
#' 
#' @param x a \code{\link{banter_model}} object.
#' 
#' @return a data.frame with the percent correctly classified for each model 
#'   in \code{x}.
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
#' modelPctCorrect(bant.mdl)
#' 
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' 
#' @export
#' 
modelPctCorrect <- function(x) {
  # check that detectors are present
  if(is.null(x@detectors)) return(NULL)
  
  # creat list of data.frames for each detector
  lapply(c(names(x@detectors), "event"), function(model) {
    rf <- getBanterModel(x, model) 
    if(is.null(rf)) return(NULL)
    # get percent correct for each species and overall
    conf.mat <- table(rf$y, rf$predicted)
    correct <- diag(conf.mat) / rowSums(conf.mat)
    correct <- c(correct, Overall = sum(diag(conf.mat)) / sum(conf.mat))
    # create data.frame
    data.frame(
      species = names(correct), 
      pct.correct = correct * 100, 
      model = model,
      stringsAsFactors = FALSE
    )
  }) %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(
      model = factor(.data$model, levels = c(names(x@detectors), "event"))
    ) %>% 
    tidyr::spread("model", "pct.correct")
}