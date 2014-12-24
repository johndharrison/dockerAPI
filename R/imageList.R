#' @export
rbind.imageList <- function(x){
  res <- lapply(x, function(y){
    out <- lapply(c(id = "id", created = "created", parentId = "parentId", name = "name",
                    repoTags = "repoTags", size = "size", virtualSize = "virtualSize"), y$field)
    out$repoTags <- I(out$repoTags)
    data.frame(out, stringsAsFactors = FALSE)
  }
  )
  do.call(rbind.data.frame, res)
}

#' @export
print.imageList <- function(x, ...){
  print(rbind(x), ...)
}

#' @export
`[.imageList` <- function(x, i ,j){
  if(missing(j)){
    res <- .Primitive("[")(unclass(x), i)
    return(`class<-`(res, "imageList"))
  }else{
    rbind(x)[i,j]
  }
  
}

#' @export
`$.imageList` <- function(x, name){
  rbind(x)[[name]]
}

#' @export
#' @importFrom utils .DollarNames
.DollarNames.imageList <- function(x, pattern){
  grep(pattern, names(rbind(x)), value=TRUE)
}
