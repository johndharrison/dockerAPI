#' docker Class uses the 
#' 
#' docker is a generator object. 
#'
#' @import jsonlite
#' @import whisker 
#' @import httr
#' @export docker
#' @exportClass docker
#' @include errorHandler.R
#' @aliases docker
#' @examples
#' \dontrun{
#' docker <- docker(ip = "192.168.59.103", port = 2375L) # windows example
#' docker$get()
#' }

docker <- setRefClass("docker",
                      contains = "errorHandler",
                      fields = list(ip = "character",
                                    port = "integer"),
                      methods = list(
                        initialize = function(ip = "localhost", port = 2375L, ...){
                          ip <<- ip
                          port <<- as.integer(port) # pre-empt user
                          callSuper(...)
                        },
                        getContainers = function(all = TRUE, limit = NULL, since = NULL, before = NULL, size = NULL){
                          'List containers:
                          \\describe{
                          \\item{\\code{all}:}{1/True/true or 0/False/false, Show all containers. Only running containers are shown by default (i.e., this defaults to false)}
                          \\item{\\code{limit}:}{Show limit last created containers, include non-running ones.}
                          \\item{\\code{since}:}{Show only containers created since Id, include non-running ones.}
                          \\item{\\code{before}:}{Show only containers created before Id, include non-running ones.}
                          \\item{\\code{size}:}{1/True/true or 0/False/false, Show the containers sizes}
                          }'
                          dUrl <- list(scheme = "http", hostname = ip, port = port
                                       , path = "containers/json", params = NULL
                                       , fragment = NULL, query = list(all = all, limit = limit, since = since, before = before, size = size)
                                       , username = NULL, password = NULL)
                          class(dUrl) <- "url"
                          checkResponse(GET(build_url(dUrl)), pass = c(200L))
                          res <- content(response, simplifyDataFrame = TRUE)
                          names(res) <- c("command", "created", "id", "image", "names", "ports", "status")
                          res$created <- as.POSIXct(res$created, origin = "1970-01-01")
                          containers <- lapply(seq(nrow(res)), function(x){
                            do.call(dockerContainer, res[x,])$import(.self)
                          })
                          `class<-`(containers, "containerList")
                        },
                        
                        getImages = function(all = FALSE, filters = NULL){
                          'List images:
                          \\describe{
                          \\item{\\code{all}:}{1/True/true or 0/False/false, Show all images. Only running images are shown by default (i.e., this defaults to false)}
                          \\item{\\code{filters}:}{a json encoded value of the filters (a map[string][]string) to process on the images list.}
                           }'
                          dUrl <- list(scheme = "http", hostname = ip, port = port
                                       , path = "images/json", params = NULL
                                       , fragment = NULL, query = list(all = all, filters = toJSON(filters))
                                       , username = NULL, password = NULL)
                          class(dUrl) <- "url"
                          checkResponse(GET(build_url(dUrl)), pass = c(200L))
                          res <- content(response, simplifyDataFrame = TRUE)
                          names(res) <- c("created", "id", "parentId", "repoTags", "size", "virtualSize")
                          res$created <- as.POSIXct(res$created, origin = "1970-01-01")
                          containers <- lapply(seq(nrow(res)), function(x){
                            do.call(dockerImage, res[x,])$import(.self)
                          })
                          `class<-`(containers, "imageList")
                        }
                      )
)