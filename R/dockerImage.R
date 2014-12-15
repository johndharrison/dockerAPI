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
                               dUrl <- dockerUrl
                               dUrl["path"] <- list("images/{{appname}}/json")
                               appname <- name
                               checkResponse(GET(whisker.render(build_url(dUrl))), pass = c(200L))
                               content(response, simplifyDataFrame = TRUE)
                             },
                             
                             history = function(){
                               'Return the history of the image.'
                               dUrl <- dockerUrl
                               dUrl["path"] <- list("images/{{appname}}/history")
                               appname <- name
                               checkResponse(GET(whisker.render(build_url(dUrl))), pass = c(200L))
                               content(response, simplifyDataFrame = TRUE)
                             },
                             
                             remove = function(force = FALSE, noprune = FALSE){
                               'Remove an image
                               \\describe{
                                \\item{\\code{force}:}{1/True/true or 0/False/false, default false.}
                                \\item{\\code{noprune}:}{1/True/true or 0/False/false, default false.}
                                }'
                               
                               dUrl <- dockerUrl
                               dUrl[c("path", "query")] <- list("images/{{appname}}"
                                                                , list(force = force, noprune = noprune))
                               appname <- name
                               checkResponse(DELETE(whisker.render(build_url(dUrl))), pass = c(200L))
                               content(response, simplifyDataFrame = TRUE)
                             }
                             
                           )
)