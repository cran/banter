#' @name numCalls
#' @title Number and Proportion of Calls
#' @description Number and proportion of calls in BANTER detector models
#' 
#' @param x a \code{\link{banter_model}} object.
#' @param by return summary by \code{"species"} or \code{"event"}.
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
#' # number of calls by species and event
#' numCalls(bant.mdl, "species")
#' numCalls(bant.mdl, "event")
#' 
#' # proportion of calls by species and event
#' propCalls(bant.mdl, "species")
#' propCalls(bant.mdl, "event")
#' 
#' @importFrom dplyr n
#' @importFrom magrittr %>%
#' @importFrom plyr .
#' @importFrom rlang .data
#' 
#' @export
#' 
numCalls <- function(x, by = c("species", "event")) {
  if(is.null(x@detectors)) stop("no detectors loaded in model.")
  by <- switch(match.arg(by), species = "species", event = "event.id")
  lapply(names(x@detectors), function(d) {
    x@data %>% 
      dplyr::left_join(x@detectors[[d]]@ids, by = "event.id") %>% 
      dplyr::group_by(.dots = by) %>% 
      dplyr::summarize(n = sum(!is.na(.data$call.id))) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(detector = paste0("num.", d))
  }) %>%
    dplyr::bind_rows() %>% 
    tidyr::spread("detector", "n") %>% 
    replace(is.na(.), 0) %>% 
    as.data.frame()
}

#' @rdname numCalls
#' @export
#' 
propCalls <- function(x, by = c("species", "event")) {
  df <- numCalls(x, by)
  by <- colnames(df)[1]
  df %>% 
    tidyr::gather("detector", "n", -by) %>% 
    dplyr::group_by(.dots = by) %>%
    dplyr::mutate(prop = n / sum(n, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-.data$n) %>% 
    dplyr::mutate(detector = gsub("num.", "prop.", .data$detector)) %>% 
    tidyr::spread("detector", "prop") %>% 
    as.data.frame()
}