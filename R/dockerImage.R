#' docker Class uses the 
#' 
#' dockerImage is a generator object. 
#'
#' @import jsonlite
#' @import whisker 
#' @import httr
#' @export dockerImage
#' @exportClass dockerImage
#' @include docker.R
#' @aliases dockerImage
#' @examples
#' \dontrun{
#' myConfig <- httr::config(sslcert = "../../certs/cert.pem"
#' , sslkey = "../../certs/key.pem"
#' , sslversion=1L, ssl.verifypeer = FALSE)
#' dckr <- docker("https://192.168.59.103:2376", myConfig)
#' dckr$getContainers()
#' }

dockerImage <- setRefClass("dockerImage",
                           fields = list(
                             created = "POSIXct",
                             id = "character",
                             parentId = "character",
                             repoTags = "list",
                             name = "character",
                             size = "numeric",
                             virtualSize = "numeric"),
                           contains = "docker",
                           methods = list(
                             initialize = function(created = NULL, id = NULL, parentId = NULL
                                                   , repoTags = list(), size = NULL, virtualSize = NULL){
                               created <<- created
                               id <<- id
                               parentId <<- parentId
                               repoTags <<- repoTags
                               size <<- size
                               virtualSize <<- virtualSize
                               name <<- unique(sapply(strsplit(unlist(repoTags), ":"), "[", 1))
                             },
                             
                             inspect = function(...){
                               'Return low-level information on the image.
                               \\describe{
                                \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                               }
                               '
                               buildREST(dockerUrl, list(path = "images/{{appname}}/json"), GET
                                         , data.frame(appname = name), ...)
                               content(response, simplifyDataFrame = TRUE)
                             },
                             
                             history = function(...){
                               'Return the history of the image.
                               \\describe{
                                \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                               }'
                               buildREST(dockerUrl, list(path = "images/{{appname}}/history"), GET
                                         , data.frame(appname = name), ...)
                               content(response, simplifyDataFrame = TRUE)
                             },
                             
                             remove = function(force = FALSE, noprune = FALSE, ...){
                               'Remove an image
                               \\describe{
                                \\item{\\code{force}:}{1/True/true or 0/False/false, default false.}
                                \\item{\\code{noprune}:}{1/True/true or 0/False/false, default false.}
                                \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                                }'
                               
                               buildREST(dockerUrl, list(path = "images/{{appname}}", query = list(force = force, noprune = noprune))
                                         , DELETE, data.frame(appname = name), ...)
                               content(response, simplifyDataFrame = TRUE)
                             }
                             
                           )
)