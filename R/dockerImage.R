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
                             
                             inspect = function(){
                               'Return low-level information on the image.
                          '
                               dUrl <- list(scheme = "http", hostname = ip, port = port
                                            , path = "images/{{appname}}/json", params = NULL
                                            , fragment = NULL, query = NULL
                                            , username = NULL, password = NULL)
                               class(dUrl) <- "url"
                               appid <- id
                               checkResponse(GET(whisker.render(build_url(dUrl))), pass = c(200L))
                               content(response, simplifyDataFrame = TRUE)
                             }
                             
                           )
)