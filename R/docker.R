setOldClass("url")
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
#' docker <- docker("http://192.168.59.103:2375") # windows example
#' docker$getContainers()
#' 
#' # TLS example:
#' dckr <- docker("https://192.168.59.103:2376")
#' # Note the location of your certs.
#' myConfig = httr::config(sslcert = "../../certs/cert.pem"
#'                    , sslkey = "../../certs/key.pem"
#'                    , sslversion=1L, ssl.verifypeer = FALSE)
#' dckr$getContainers(config = myConfig)
#' dckr$searchImages("rstudio", config = myConfig)
#' }

docker <- setRefClass("docker",
                      contains = "errorHandler",
                      fields = list(dockerUrl = "url"),
                      methods = list(
                        initialize = function(dckUrl = NULL, ...){
                          if(is.null(dckUrl)){
                            dockerUrl <<- `class<-`(list(), "url")
                          }else{
                            if(!"url" %in% class(dckUrl)){
                              dckUrl <- parse_url(dckUrl)
                            }
                            dockerUrl <<- dckUrl
                          }
                          callSuper(...)
                        },
                        getContainers = function(all = TRUE, limit = NULL, since = NULL, before = NULL, size = NULL, ...){
                          'List containers:
                          \\describe{
                          \\item{\\code{all}:}{1/True/true or 0/False/false, Show all containers. Only running containers are shown by default (i.e., this defaults to false)}
                          \\item{\\code{limit}:}{Show limit last created containers, include non-running ones.}
                          \\item{\\code{since}:}{Show only containers created since Id, include non-running ones.}
                          \\item{\\code{before}:}{Show only containers created before Id, include non-running ones.}
                          \\item{\\code{size}:}{1/True/true or 0/False/false, Show the containers sizes}
                          \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                          }'
                          dUrl <- dockerUrl
                          dUrl[c("path", "query")] <- list("containers/json"
                                                           , list(all = all, limit = limit, since = since, before = before, size = size))
                          checkResponse(GET(build_url(dUrl), ...), pass = c(200L))
                          res <- content(response, simplifyDataFrame = TRUE)
                          if(identical(res, list())){return(res)}
                          names(res) <- c("command", "created", "id", "image", "names", "ports", "status")
                          res$created <- as.POSIXct(res$created, origin = "1970-01-01")
                          containers <- lapply(seq(nrow(res)), function(x){
                            do.call(dockerContainer, res[x,])$import(.self)
                          })
                          `class<-`(containers, "containerList")
                        },
                        
                        getImages = function(all = FALSE, filters = NULL, ...){
                          'List images:
                          \\describe{
                          \\item{\\code{all}:}{1/True/true or 0/False/false, Show all images. Only running images are shown by default (i.e., this defaults to false)}
                          \\item{\\code{filters}:}{a json encoded value of the filters (a map[string][]string) to process on the images list.}
                          \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                           }'
                          dUrl <- dockerUrl
                          dUrl[c("path", "query")] <- list("images/json"
                                                           , list(all = all, filters = toJSON(filters)))
                          checkResponse(GET(build_url(dUrl), ...), pass = c(200L))
                          res <- content(response, simplifyDataFrame = TRUE)
                          if(identical(res, list())){return(res)}
                          names(res) <- c("created", "id", "parentId", "repoTags", "size", "virtualSize")
                          res$created <- as.POSIXct(res$created, origin = "1970-01-01")
                          containers <- lapply(seq(nrow(res)), function(x){
                            do.call(dockerImage, res[x,])$import(.self)
                          })
                          `class<-`(containers, "imageList")
                        },
                        
                        createImage = function(fromImage, fromSrc = NULL, repo = NULL, tag = NULL
                                               , registry = NULL, XRegistryAuth = NULL, ...){
                          'Create an image, either by pulling it from the registry or by importing it.
                               \\describe{
                                \\item{\\code{fromImage}:}{Name of the image to pull.}
                                \\item{\\code{fromSrc}:}{source to import, means stdin}
                                \\item{\\code{repo}:}{Repository}
                                \\item{\\code{tag}:}{Tag}
                                \\item{\\code{registry}:}{Registry}
                                \\item{\\code{XRegistryAuth}:}{Base64-encoded AuthConfig object.}
                                \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                               }
                          '
                          dUrl <- dockerUrl
                          dUrl[c("path", "query")] <- list("images/create"
                                                           , list(fromImage = fromImage, fromSrc = fromSrc, repo = repo, tag = tag
                                                                  , registry = registry, "X-Registry-Auth" = XRegistryAuth))
                          checkResponse(POST(build_url(dUrl), ...), pass = c(200L))
                          cat(content(response, "text"))
                        },
                        
                        searchImages = function(term, ...){
                          'List images:
                          \\describe{
                          \\item{\\code{term}:}{Term to search.}
                          \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                          }'
                          if(missing(term)){stop("Please provide a search term", call. = FALSE)}
                          dUrl <- dockerUrl
                          dUrl[c("path", "query")] <- list("images/search"
                                                           , list(term = term))
                          checkResponse(GET(build_url(dUrl), ...), pass = c(200L))
                          content(response, simplifyDataFrame = TRUE)
                        },
                        
                        checkAuth = function(...){
                          'Get the default username and email:
                          \\describe{
                          \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                        }'
                          dUrl <- dockerUrl
                          dUrl["path"] <- list("auth")
                          checkResponse(POST(build_url(dUrl), ...), pass = c(200L))
                          content(response, simplifyDataFrame = TRUE)
                          
                        },
                        
                        info = function(...){
                          'Display system-wide information:
                          \\describe{
                          \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                          }'
                          dUrl <- dockerUrl
                          dUrl["path"] <- list("info")
                          checkResponse(GET(build_url(dUrl), ...), pass = c(200L))
                          content(response, simplifyDataFrame = TRUE)
                        },
                        
                        version = function(...){
                          'Show the docker version information:
                          \\describe{
                          \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                          }'
                          dUrl <- dockerUrl
                          dUrl["path"] <- list("version")
                          checkResponse(GET(build_url(dUrl), ...), pass = c(200L))
                          content(response, simplifyDataFrame = TRUE)
                        },
                        
                        ping = function(...){
                          'Ping the docker server:
                          \\describe{
                          \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                          }'
                          dUrl <- dockerUrl
                          dUrl["path"] <- list("_ping")
                          checkResponse(GET(build_url(dUrl), ...), pass = c(200L))
                          content(response)
                        }
                      )
)