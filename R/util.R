#' @export .DollarNames.docker
#' @export .DollarNames.dockerContainer
#' @export .DollarNames.dockerImage
#' @import methods

.DollarNames.docker <- function(x, pattern){
  superMethods <- getRefClass("envRefClass")$methods()
  errorMethods <- getRefClass("errorHandler")$methods()
  dockerMethods <- getRefClass(class(x))$methods()
  grep(pattern, dockerMethods[!dockerMethods%in%c(superMethods, errorMethods)], value=TRUE)
}

.DollarNames.dockerContainer <- function(x, pattern){
  superMethods <- getRefClass("envRefClass")$methods()
  errorMethods <- getRefClass("errorHandler")$methods()
  dockerMethods <- getRefClass("docker")$methods()
  dockerContainerMethods <- getRefClass(class(x))$methods()
  grep(pattern, dockerContainerMethods[!dockerContainerMethods%in%c(superMethods, dockerMethods, errorMethods)], value=TRUE)
}

.DollarNames.dockerImage <- function(x, pattern){
  superMethods <- getRefClass("envRefClass")$methods()
  errorMethods <- getRefClass("errorHandler")$methods()
  dockerMethods <- getRefClass("docker")$methods()
  dockerImageMethods <- getRefClass(class(x))$methods()
  grep(pattern, dockerImageMethods[!dockerImageMethods%in%c(superMethods, dockerMethods, errorMethods)], value=TRUE)
}
