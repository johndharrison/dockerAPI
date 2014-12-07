#' docker Class uses the 
#' 
#' dockerContainer is a generator object. 
#'
#' @import jsonlite
#' @import whisker 
#' @import httr
#' @export dockerContainer
#' @exportClass dockerContainer
#' @aliases dockerContainer
#' @include docker.R
#' @examples
#' \dontrun{
#' }

dockerContainer <- setRefClass("dockerContainer",
                               fields = list(id = "character"
                                             , created = "POSIXct"
                                             , image = "character"
                                             , names = "list"
                                             , ports = "list"
                                             , status = "character"
                                             , command = "character"),
                               contains = "docker",
                               methods = list(
                                 initialize = function(id = "", created = NULL, image = NULL, names = list(), ports = list(), status = NULL, command = NULL, ...){
                                   appports <- ports # fix note
                                   if(ncol(appports[[1]]) == 0){appports[[1]] <- data.frame(IP = character(0), PrivatePort = numeric(0), PublicPort = numeric(0), Type = character(0))}
                                   id <<- id
                                   created <<- created
                                   image <<- image
                                   names <<- names
                                   ports <<- appports
                                   status <<- status
                                   command <<- command
                                   callSuper(...)
                                 },
                                 
                                 inspect = function(){
                                   'Return low-level information on the container.
                          '
                                   dUrl <- list(scheme = "http", hostname = ip, port = port
                                                , path = "containers/{{appid}}/json", params = NULL
                                                , fragment = NULL, query = NULL
                                                , username = NULL, password = NULL)
                                   class(dUrl) <- "url"
                                   appid <- id
                                   checkResponse(GET(whisker.render(build_url(dUrl))), pass = c(200L))
                                   content(response, simplifyDataFrame = TRUE)
                                 },
                                 
                                 listProcesses = function(ps_args = NULL){
                                   'List processes running inside the container.
                             \\describe{
                             \\item{\\code{ps_args}:}{ps arguments to use (e.g., aux). docker os dependent see \\url{https://github.com/docker/docker/issues/8075}}
                             }'
                                   dUrl <- list(scheme = "http", hostname = ip, port = port
                                                , path = "containers/{{appid}}/top", params = NULL
                                                , fragment = NULL, query = list("ps_args" = ps_args)
                                                , username = NULL, password = NULL)
                                   class(dUrl) <- "url"
                                   appid <- id
                                   checkResponse(GET(whisker.render(build_url(dUrl))), pass = c(200L))
                                   res <- content(response)
                                   setNames(do.call(rbind.data.frame, res[["Processes"]])
                                            , unlist(res$Titles))
                                 },
                                 
                                 logs = function(stdout = FALSE, stderr = FALSE, timestamps = FALSE, tail = "all"){
                                   'Get stdout and stderr logs from the container.
                          \\describe{
                          \\item{\\code{stdout}:}{1/True/true or 0/False/false, show stdout log. Default false}
                          \\item{\\code{stderr}:}{1/True/true or 0/False/false, show stderr log. Default false}
                          \\item{\\code{timestamps}:}{1/True/true or 0/False/false, print timestamps for every log line. Default false}
                          \\item{\\code{tail}:}{Output specified number of lines at the end of logs: all or <number>. Default all}
                          }'
                                   dUrl <- list(scheme = "http", hostname = ip, port = port
                                                , path = "containers/{{appid}}/logs", params = NULL
                                                , fragment = NULL, query = list(follow = FALSE, stdout = stdout
                                                                                , stderr = stderr, timestamps = timestamps, tail = tail)
                                                , username = NULL, password = NULL)
                                   appid <- id
                                   class(dUrl) <- "url"
                                   checkResponse(GET(whisker.render(build_url(dUrl))), pass = c(200L))
                                   out <- content(response)
                                   capture.output(cat(rawToChar(out[!out == as.raw(0)])))
                                 },
                                 
                                 fsChanges = function(){
                                   'Inspect changes on containers filesystem.
                                   '
                                   dUrl <- list(scheme = "http", hostname = ip, port = port
                                                , path = "containers/{{appid}}/changes", params = NULL
                                                , fragment = NULL, query = NULL
                                                , username = NULL, password = NULL)
                                   class(dUrl) <- "url"
                                   appid <- id
                                   checkResponse(GET(whisker.render(build_url(dUrl))), pass = c(200L))
                                   content(response, simplifyDataFrame = TRUE)
                                 },
                                 
                                 export = function(filename = tempfile(fileext = ".tar")){
                                   'Export the contents of the container.
                                   \\describe{
                                   \\item{\\code{filename}:}{A filename to export the tar to. If NULL is given the tar is returned in RAW format.}
                                   }
                                   '
                                   dUrl <- list(scheme = "http", hostname = ip, port = port
                                                , path = "containers/{{appid}}/export", params = NULL
                                                , fragment = NULL, query = NULL
                                                , username = NULL, password = NULL)
                                   class(dUrl) <- "url"
                                   appid <- id
                                   if(!is.null(filename)){
                                     checkResponse(GET(whisker.render(build_url(dUrl)), write_disk(filename)), pass = c(200L))
                                   }else{
                                     checkResponse(GET(whisker.render(build_url(dUrl))), pass = c(200L))
                                     content(response)
                                   }
                                 },
                                 
                                 resize = function(height = NULL, width = NULL){
                                   'Resize the TTY of the container.
                                   \\describe{
                                   \\item{\\code{height}:}{Height for the resized container.}
                                   \\item{\\code{width}:}{Width for the resized container.}
                                   }'
                                   if(is.null(height) || is.null(width)){
                                     stop("Please provide a height and width for the resized container.")
                                   }
                                   dUrl <- list(scheme = "http", hostname = ip, port = port
                                                , path = "containers/{{appid}}/resize", params = NULL
                                                , fragment = NULL, query = list(height = height, width = width)
                                                , username = NULL, password = NULL)
                                   class(dUrl) <- "url"
                                   appid <- id
                                   checkResponse(GET(whisker.render(build_url(dUrl))), pass = c(200L))
                                   content(response)
                                 },
                                 
                                 stop = function(t = NULL){
                                   'Stop the container.
                                   \\describe{
                                    \\item{\\code{t}:}{number of seconds to wait before killing the container.}
                                   }
                                   '
                                   dUrl <- list(scheme = "http", hostname = ip, port = port
                                                , path = "containers/{{appid}}/stop", params = NULL
                                                , fragment = NULL, query = list(t = t)
                                                , username = NULL, password = NULL)
                                   class(dUrl) <- "url"
                                   appid <- id
                                   checkResponse(POST(whisker.render(build_url(dUrl))), pass = c(204L)
                                                 , errors = data.frame(status_code = 304, message = "container already stopped", stringsAsFactors = FALSE))
                                 },
                                 
                                 restart = function(t = NULL){
                                   'Restart the container.
                                   \\describe{
                                    \\item{\\code{t}:}{number of seconds to wait before killing the container.}
                                   }
                                   '
                                   dUrl <- list(scheme = "http", hostname = ip, port = port
                                                , path = "containers/{{appid}}/restart", params = NULL
                                                , fragment = NULL, query = list(t = t)
                                                , username = NULL, password = NULL)
                                   class(dUrl) <- "url"
                                   appid <- id
                                   checkResponse(POST(whisker.render(build_url(dUrl))), pass = c(204L))
                                 },
                                 
                                 kill = function(){
                                   'Kill the container.
                                   '
                                   dUrl <- list(scheme = "http", hostname = ip, port = port
                                                , path = "containers/{{appid}}/kill", params = NULL
                                                , fragment = NULL, query = NULL
                                                , username = NULL, password = NULL)
                                   class(dUrl) <- "url"
                                   appid <- id
                                   checkResponse(POST(whisker.render(build_url(dUrl))), pass = c(204L))
                                 }
                               )
)