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
                      fields = list(),
                      contains = "errorHandler",
                      fields = list(ip = "character",
                                    port = "integer"),
                      methods = list(
                        initialize = function(ip = "localhost", port = 2375L){
                          ip <<- ip
                          port <<- as.integer(port) # pre-empt user
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
                          content(GET(build_url(dUrl)), simplifyDataFrame = TRUE)
                        },
                      )
)