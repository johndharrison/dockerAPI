#' @export
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

#' @export
print.containerList <- function(x, ...){
  print(rbind(x), ...)
}

#' @export
`[.containerList` <- function(x, i ,j){
  if(missing(j)){
    res <- .Primitive("[")(unclass(x), i)
    return(`class<-`(res, "containerList"))
  }else{
    rbind(x)[i,j]
  }
  
}
