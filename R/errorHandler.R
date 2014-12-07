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
                              
                              checkResponse = function(appresponse, pass = c(), warnings = data.frame(), errors = c()){
                                response <<- appresponse
                                if(!appresponse$status_code %in% pass){
                                  if(!appresponse$status_code %in% errors$status_code){
                                    base::stop(capture.output(cat(content(appresponse))), call. = FALSE)                                    
                                  }else{
                                    base::stop(errors$message[errors$status_code == appresponse$status_code], call. = FALSE)
                                  }
                                }
                              })
                            )