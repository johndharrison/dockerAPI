#' docker Class uses the 
#' 
#' dockerContainer is a generator object. 
#'
#' @import jsonlite
#' @import whisker 
#' @import httr
#' @export dockerContainer
#' @exportClass dockerContainer
#' @aliases dockerAPI
#' @include docker.R
#' @examples
#' \dontrun{
#' }

dockerContainer <- setRefClass("dockerContainer",
                      fields = list(id = "character"
                                    , created = "POSIXct"
                                    , image = "character"
                                    , names = "list"
                                    , ports = "list"
                                    , status = "character"
                                    , command = "character"),
                      contains = "docker",
                      methods = list(
                        initialize = function(id = "", created = NULL, image = NULL, names = list(), ports = list(), status = NULL, command = NULL, ...){
                          #ports <- if(is.data.frame(ports)){ports}else{ports[[1]]}
                          if(ncol(ports[[1]]) == 0){ports[[1]] <- data.frame(IP = character(0), PrivatePort = numeric(0), PublicPort = numeric(0), Type = character(0))}
                          id <<- id
                          created <<- created
                          image <<- image
                          names <<- names
                          ports <<- ports
                          status <<- status
                          command <<- command
                          callSuper(...)
                        }
                          )
)