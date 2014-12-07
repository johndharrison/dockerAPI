#' docker Class uses the 
#' 
#' dockerImage is a generator object. 
#'
#' @import jsonlite
#' @import whisker 
#' @import httr
#' @export dockerImage
#' @exportClass dockerImage
#' @aliases dockerImage
#' @examples
#' \dontrun{
#' }

dockerImage <- setRefClass("dockerImage",
                         fields = list(),
                         contains = "docker",
                         methods = list()
)