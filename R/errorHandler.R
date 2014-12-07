setOldClass("response")
#' CLASS errorHandler
#'
#' errorHandler Class uses the 
#' 
#' errorHandler is a generator object. 
#'
#' @import jsonlite
#' @import whisker 
#' @import httr
#' @export errorHandler
#' @exportClass errorHandler
#' @aliases errorHandler
#' @examples
#' \dontrun{
#' }

errorHandler <- setRefClass("errorHandler",
                            fields = list(
                              response = "response"),
                            methods = list(
                              initialize = function(response = NULL, ...){
                                if(is.null(response)){
                                  response <<- `class<-`(list(), "response")
                                }
                                callSuper(...)
                              },
                              
                              checkResponse = function(response, warnings = c(), errors = c()){
                                response <<- response
                              })
                            )