#' docker Class uses the 
#' 
#' dockerContainer is a generator object. 
#'
#' @import jsonlite
#' @import whisker 
#' @import httr
#' @export dockerContainer
#' @exportClass dockerContainer
#' @aliases dockerContainer
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
                        },
                        
                        inspect = function(){
                          'Return low-level information on the container
                          '
                          dUrl <- list(scheme = "http", hostname = ip, port = port
                                       , path = "containers/{{id}}/json", params = NULL
                                       , fragment = NULL, query = NULL
                                       , username = NULL, password = NULL)
                          class(dUrl) <- "url"
                          id <- .self$id
                          checkResponse(GET(whisker.render(build_url(dUrl))))
                          content(response, simplifyDataFrame = TRUE)
                        },
                        
                        listProcesses = function(ps_args = NULL){
                          'List processes running inside the container
                             \\describe{
                             \\item{\\code{ps_args}:}{ps arguments to use (e.g., aux). docker os dependent see \\url{https://github.com/docker/docker/issues/8075}}
                             }'
                          dUrl <- list(scheme = "http", hostname = ip, port = port
                                       , path = "containers/{{id}}/top", params = NULL
                                       , fragment = NULL, query = list("ps_args" = ps_args)
                                       , username = NULL, password = NULL)
                          class(dUrl) <- "url"
                          id <- .self$id
                          checkResponse(GET(whisker.render(build_url(dUrl))))
                          res <- content(reponse)
                          setNames(do.call(rbind.data.frame, res[["Processes"]])
                                   , unlist(res$Titles))
                        }
                          )
)