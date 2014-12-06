#' @export .DollarNames.dockerAPI
#' @import methods

.DollarNames.dockerAPI <- function(x, pattern){
  superMethods <- getRefClass("envRefClass")$methods()
  dockerMethods <- getRefClass(class(x))$methods()
  grep(pattern, dockerMethods[!dockerMethods%in%superMethods], value=TRUE)
}
