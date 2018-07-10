#' @name addBanterDetector
#' @title Add a BANTER Detector Model
#' @description Add a detector model to a BANTER classifier.
#'
#' @param x a \code{\link{banter_model}} object.
#' @param data detector data.frame or named list of detector data.frames. If 
#'   a data.frame, then \code{name} must be provided.
#' @param name detector name.
#' @param ntree number of trees.
#' @param sampsize number or fraction of samples to use in each tree.
#' @param importance retain importance scores in model? Defaults to 
#'   \code{FALSE} and will be ignored if \code{num.cores > 1}.
#' @param num.cores number of cores to use for Random Forest model.
#' 
#' @return a \code{\link{banter_model}} object with the detector model added or 
#'   removed.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' data(train.data)
#' # initialize BANTER model with event data
#' bant.mdl <- initBanterModel(train.data$events)
#' # add the 'bp' (burst pulse) detector model
#' bant.mdl <- addBanterDetector(
#'   x = bant.mdl, 
#'   data = train.data$detectors$bp, 
#'   name = "bp",
#'   ntree = 50, sampsize = 1, num.cores = 1
#' )
#' bant.mdl
#' 
#' # remove the 'bp' detector model
#' bant.mdl <- removeBanterDetector(bant.mdl, "bp")
#' bant.mdl
#' 
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
#' 
addBanterDetector <- function(x, data, name, ntree, sampsize = 1, 
                              importance = FALSE, num.cores = NULL) {
  # Check that detectors is a list
  if(is.null(x@detectors)) x@detectors <- list()
  
  # If data is a list of detectors
  if(methods::is(data, "list")) {
    if(is.null(names(data))) {
      stop("the list of detectors in 'data' must be named.")
    }
    x@detectors[names(data)] <- sapply(names(data), function(detector) {
      .runDetectorModel(
        x = x, data = data[[detector]], name = detector,
        ntree = ntree, sampsize = sampsize, importance = importance,
        num.cores = num.cores
      )
    }, simplify = FALSE)
  
  # If data is a data.frame
  } else if(methods::is(data, "data.frame")) {
    if(missing(name)) {
      stop("'name' must be supplied with the 'data' data.frame")
    }
    x@detectors[[name]] <- .runDetectorModel(
      x = x, data = data, name = name, 
      ntree = ntree, sampsize = sampsize, importance = importance,
      num.cores = num.cores
    )
  } else stop("'data' must be a list or data.frame.")
  
  # Sort detectors and empty event model info
  x@detectors <- x@detectors[order(names(x@detectors))]
  x@model.data <- x@model <- x@sampsize <- x@timestamp <- NULL
  x
}

#' @rdname addBanterDetector
#' @export
#' 
removeBanterDetector <- function(x, name) {
  x@detectors[[name]] <- NULL
  if(length(x@detectors) == 0) x@detectors <- NULL
  x@model.data <- x@model <- x@sampsize <- x@timestamp <- NULL
  x
}

#' @importFrom parallel detectCores makeCluster clusterExport clusterEvalQ parLapply stopCluster mclapply
#' @importFrom randomForest randomForest combine
#' @importFrom utils sessionInfo
#' @keywords internal
#' 
.runDetectorModel <- function(x, data, name, ntree, 
                              sampsize = 1, importance = FALSE, 
                              num.cores = NULL) {
  
  # Check that "event.id" and "call.id" exist
  if(!all(c("event.id", "call.id") %in% colnames(data))) {
    stop("'data' for detector ", name, " must have 'event.id' and 'call.id' columns")
  }
  
  # Combine event data with call ids in detector
  df <- x@data %>% 
    dplyr::select(.data$event.id, .data$species) %>% 
    dplyr::inner_join(data, by = "event.id") %>% 
    dplyr::mutate(species = as.character(.data$species)) %>% 
    as.data.frame
  
  # Get and check requested sample size
  sampsize <- .getSampsize(
    df$species,
    sampsize,
    paste0("Detector model (", name, ")")
  )

  # Remove missing species and format columns
  df <- df %>%
    dplyr::filter(.data$species %in% names(sampsize)) %>%
    dplyr::mutate(species = factor(.data$species)) %>%
    dplyr::mutate(id = paste0(.data$event.id, ".", .data$call.id)) %>%
    tibble::column_to_rownames("id") %>%
    as.data.frame() %>%
    droplevels()

  # Setup number of cores
  if(is.null(num.cores)) num.cores <- parallel::detectCores() - 1
  num.cores <- min(parallel::detectCores() - 1, num.cores)
  
  # Create random forest parameter list
  params <- list(
    predictors = df %>% 
      dplyr::select(-.data$event.id, -.data$call.id, -.data$species),
    response = df$species,
    sampsize = sampsize,
    importance = importance & num.cores == 1
  )
  
  # Don't use parallelizing if num.cores == 1
  rf <- if(num.cores == 1) {
    params$ntree <- ntree
    .rfFuncDetector(params) 
    
  # Parallel random forest
  } else {
    # Create multicore rf function
    .clRF <- function(i, params) .rfFuncDetector(params)
    params$ntree <- ceiling(ntree / num.cores)
    
    # Run random forest on Linux or Macs using mclapply
    rf.list <- if(Sys.info()[["sysname"]] %in% c("Linux", "Darwin")) {
      parallel::mclapply(
        1:num.cores, .clRF, params = params, mc.cores = num.cores
      )
      
    # Run random forest using parLapply
    } else {
      tryCatch({
        cl <- parallel::makeCluster(num.cores)
        parallel::clusterEvalQ(cl, require(randomForest))
        parallel::clusterExport(cl, "params", environment())
        parallel::parLapply(cl, 1:num.cores, .clRF, params = params)
      }, finally = parallel::stopCluster(cl))
    }
    do.call(randomForest::combine, rf.list)
  }
  
  # Load banter_detector object
  banter_detector(
    name = name,
    ids = df[, c("event.id", "call.id")], 
    model = rf,
    sampsize = sampsize
  ) 
}

#' Detector randomForest function
#' @rdname internals
#' @importFrom randomForest randomForest
#' @keywords internal
#' 
.rfFuncDetector <- function(params) {
  randomForest::randomForest(
    x = params$predictors, 
    y = params$response, 
    ntree = params$ntree, 
    sampsize = params$sampsize, 
    replace = FALSE, 
    importance = params$importance, 
    proximity = FALSE, 
    norm.votes = FALSE
  )
}