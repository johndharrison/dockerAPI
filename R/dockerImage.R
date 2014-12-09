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
                             }
                             
                           )
)