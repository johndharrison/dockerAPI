setOldClass("url")
setOldClass("request")
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
#' @field dockerUrl a string of the docker host url or a httr object of class url
#' @field dockerConf Persistent configuration settings for curl  
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
                      fields = list(dockerUrl = "url", dockerConf = "request"),
                      methods = list(
                        initialize = function(dckUrl = NULL, dckrConf = config(), ...){
                          if(is.null(dckUrl)){
                            dockerUrl <<- `class<-`(list(), "url")
                          }else{
                            if(!"url" %in% class(dckUrl)){
                              dckUrl <- parse_url(dckUrl)
                            }
                            dockerUrl <<- dckUrl
                          }
                          dockerConf <<- dckrConf
                          callSuper(...)
                        },
                        
                        buildREST = function(dUrl = dockerUrl, urlComp, httpMethod
                                             , renderDF = data.frame(), pass = c(200L)
                                             , errors = c(), ...){
                          'Utility function to build RESTful requests.'
                          curlOpts <- list(...)
                          curlOpts$config = c(dockerConf, curlOpts$config)
                          dUrl[names(urlComp)] <- urlComp
                          appUrl <- whisker.render(build_url(dUrl), renderDF)
                          checkResponse(do.call(httpMethod, c(appUrl, curlOpts)), pass = pass, errors = errors)
                        },
                        
                        getContainers = function(all = TRUE, limit = NULL, since = NULL
                                                 , before = NULL, size = NULL, ...){
                          'List containers:
                          \\describe{
                          \\item{\\code{all}:}{1/True/true or 0/False/false, Show all containers. Only running containers are shown by default (i.e., this defaults to false)}
                          \\item{\\code{limit}:}{Show limit last created containers, include non-running ones.}
                          \\item{\\code{since}:}{Show only containers created since Id, include non-running ones.}
                          \\item{\\code{before}:}{Show only containers created before Id, include non-running ones.}
                          \\item{\\code{size}:}{1/True/true or 0/False/false, Show the containers sizes}
                          \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                          }'
                          buildREST(dockerUrl, list(path = "containers/json", query = list(all = all, limit = limit, since = since, before = before, size = size))
                                    , GET, ...)
                          res <- content(response, simplifyDataFrame = TRUE)
                          if(identical(res, list())){return(res)}
                          appNames <- c("command", "created", "id", "image", "names", "ports", "status")
                          appMatches <- match(tolower(appNames), tolower(names(res)))
                          names(res)[appMatches] <- appNames 
                          res$created <- as.POSIXct(res$created, origin = "1970-01-01")
                          res <- res[, appNames]
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
                          buildREST(dockerUrl, list(path = "images/json", query = list(all = all, filters = toJSON(filters)))
                                    , GET, ...)
                          res <- content(response, simplifyDataFrame = TRUE)
                          if(identical(res, list())){return(res)}
                          appNames <-  c("created", "id", "parentId", "repoTags", "size", "virtualSize")
                          appMatches <- match(tolower(appNames), tolower(names(res)))
                          names(res)[appMatches] <- appNames 
                          res$created <- as.POSIXct(res$created, origin = "1970-01-01")
                          res <- res[, appNames]
                          images <- lapply(seq(nrow(res)), function(x){
                            do.call(dockerImage, res[x,])$import(.self)
                          })
                          `class<-`(images, "imageList")
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
                          buildREST(dockerUrl, list(path = "images/create", query = list(fromImage = fromImage, fromSrc = fromSrc, repo = repo, tag = tag
                                                                                         , registry = registry, "X-Registry-Auth" = XRegistryAuth))
                                    , POST, ...)
                          cat(content(response, "text"))
                        },
                        
                        searchImages = function(term, ...){
                          'List images:
                          \\describe{
                          \\item{\\code{term}:}{Term to search.}
                          \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                          
                          }'
                          if(missing(term)){stop("Please provide a search term", call. = FALSE)}
                          buildREST(dockerUrl, list(path = "images/search", query = list(term = term))
                                    , GET)
                          content(response, simplifyDataFrame = TRUE)
                        },
                        
                        createContainer = function(contOpt, ...){
                          'Create a container
                          \\describe{
                          \\item{\\code{contOpt}:}{A object of class "containerOpt". See \\code{\\link{containerOpt}}}
                          \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                          }'
                          if(!"containerOpts" %in% class(contOpt)){
                            stop("contOpt must be of class \"containerOpts\"")
                          }
                          jsonContent <- toJSON(prepareJSON(contOpt), force = TRUE, auto_unbox = TRUE)
                          curlOpts <- list(...)
                          curlOpts$config = c(add_headers("Content-Type" = "application/json"), curlOpts$config)
                          curlOpts$body = c(jsonContent, curlOpts$body)
                          do.call(.self$buildREST, c(list(urlComp = list(path = "containers/create"), dUrl = dockerUrl
                                                     , httpMethod = POST, renderDF = data.frame(), pass = c(201L)
                                                     , errors = c()), curlOpts))
                          content(response)
                        },
                        
                        checkAuth = function(...){
                          'Get the default username and email:
                          \\describe{
                          \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                        }'
                          buildREST(dockerUrl, list(path = "auth"), POST, ...)
                          content(response, simplifyDataFrame = TRUE)                          
                        },
                        
                        info = function(...){
                          'Display system-wide information:
                          \\describe{
                          \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                          }'
                          buildREST(dockerUrl, list(path = "info"), GET, ...)
                          content(response, simplifyDataFrame = TRUE)                          
                        },
                        
                        version = function(...){
                          'Show the docker version information:
                          \\describe{
                          \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                          }'
                          buildREST(dockerUrl, list(path = "version"), GET, ...)
                          content(response, simplifyDataFrame = TRUE)                          
                        },
                        
                        ping = function(...){
                          'Ping the docker server:
                          \\describe{
                          \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                          }'
                          buildREST(dockerUrl, list(path = "_ping"), GET, ...)
                          content(response)                          
                        }
                      )
)