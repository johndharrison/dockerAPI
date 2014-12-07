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
                      fields = list(),
                      contains = "docker",
                      methods = list()
)