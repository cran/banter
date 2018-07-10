#' banter
#' 
#' @docType package
#' @name banter-package
#' @aliases banter
#' @title BioAcoustic EveNT ClassifiER
#' @keywords package
NULL

setOldClass(c("randomForest", "randomForest.formula", "rfPermute", "ranger"))
setClassUnion("classifier", c("randomForest", "randomForest.formula", "rfPermute", "ranger", "NULL"))
setClassUnion("listOrNull", c("list", "NULL"))
setClassUnion("dfOrNull", c("data.frame", "NULL"))
setClassUnion("dateOrNull", c("POSIXct", "NULL"))
setClassUnion("numOrNull", c("numeric", "NULL"))

#' @docType data
#' @name train.data
#' @title Training events and detectors
#' @description A list of events and call data from detectors for training 
#'   BANTER model
#' @usage data(train.data)
#' @format list
#' @keywords datasets
NULL

#' @docType data
#' @name test.data
#' @title Testing events and detectors
#' @description A list of events and call data from detectors for testing 
#'   BANTER model
#' @usage data(test.data)
#' @format list
#' @keywords datasets
NULL