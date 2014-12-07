#' @export .DollarNames.docker
#' @export .DollarNames.dockerContainer
#' @export .DollarNames.dockerImage
#' @export print.containerList
#' @export rbind.containerList
#' @export [.containerList
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

print.containerList <- function(x){
  print(rbind(x))
}

rbind.containerList <- function(x){
  res <- lapply(x, function(y){
    out <- lapply(c(id = "id", created = "created", image = "image", names = "names"
             ,ports = "ports", status = "status", command = "command"), y$field)
    out$ports <- I(out$ports); out$names <- I(out$names)
    data.frame(out, stringsAsFactors = FALSE)
  }
  )
  do.call(rbind.data.frame, res)
}

`[.containerList` <- function(x, i ,j){
  if(missing(j)){
    return(.Primitive("[")(unclass(x), i))
  }else{
    rbind(x)[i,j]
  }
  
}
