#' Checks to make sure 'model' is a valid model name
#' x is a banter_model object
#' @rdname internals
#' @keywords internal
#' 
.checkModelName <- function(x, model) {
  if(model != "event" & !model %in% names(x@detectors)) {
    stop(paste0("model '", model, "' not found."))
  }
  invisible(TRUE)
}

#' Check that detectors are present
#' x is a banter_model object
#' @rdname internals
#' @keywords internal
#' 
.stopIfNoDetectors <- function(x) {
  if(is.null(x@detectors)) {
    stop("no detector models found. see '?addBanterDetector'")
  }
  invisible(TRUE)
}

#' Produces a vector of equal sample sizes for balanced Random Forest model.
#' x is a vector of species or table of species frequencies
#' n is desired sample size
#' @name internals
#' @importFrom stats setNames
#' @keywords internal
#' 
.getSampsize <- function(x, n, warn.label) {
  if(n < 0) n <- 1
  freq <- if(is.table(x)) x else table(x)
  
  if(length(freq) < 2) {
    stop(
      warn.label, " has ", length(freq), 
      " species (need at least 2 for model)"
    )
  }
  
  n <- if(n >= 1) n else round(min(freq) * n, 0)
  if(n == 0) n <- 1
  
  bad.freq <- freq[freq <= n]
  good.freq <- freq[freq > n]
  
  if(length(good.freq) < 2) {
    stop(
      warn.label, ":\n",
      "sampsize = ", n,
      " is >= too many species frequencies (need at least 2 for model):\n",
      paste0("  ", names(freq), ": ", freq, "\n", collapse = ""),
      call. = FALSE
    )
  } else if(length(bad.freq) > 0) {
    warning(
      warn.label, ": sampsize = ", n, " is >= species frequencies:\n",
      paste0("  ", names(bad.freq), ": ", bad.freq, "\n", collapse = ""),
      "These species will be used in the model:\n",
      paste0("  ", names(good.freq), ": ", good.freq, "\n", collapse = ""), 
      call. = FALSE, immediate. = TRUE
    ) 
    setNames(rep(n, length(good.freq)), names(good.freq))
  } else {
    setNames(rep(n, length(freq)), names(freq))
  }
}

#' Returns matrix of mean species vote percentage by event
#' x is a list of data.frames with species probability assignments
#' @rdname internals
#' @keywords internal
#' 
.meanVotes <- function(x) {
  sapply(names(x), function(d) {
    x[[d]] %>% 
      tidyr::gather("species", "prob", -.data$event.id) %>% 
      dplyr::mutate(species = paste0(d, ".", .data$species)) %>% 
      dplyr::group_by(.data$event.id, .data$species) %>% 
      dplyr::summarize(prob.mean = mean(.data$prob)) %>% 
      dplyr::ungroup() 
  }, simplify = FALSE) %>% 
    dplyr::bind_rows() %>% 
    tidyr::spread("species", "prob.mean") %>% 
    replace(is.na(.), 0) 
}