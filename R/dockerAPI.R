#' CLASS dockerAPI
#'
#' dockerAPI Class uses the 
#' 
#' dockerAPI is a generator object. 
#'
#' @import jsonlite
#' @import whisker 
#' @import httr
#' @export dockerAPI
#' @exportClass dockerAPI
#' @aliases dockerAPI
#' @examples
#' \dontrun{
#' docker <- dockerAPI(ip = "192.168.59.103", port = 2375L) # windows example
#' docker$get()
#' }

dockerAPI <- setRefClass("dockerAPI",
                         fields = list(ip = "character",
                                       port = "integer"),
                         methods = list(
                           initialize = function(ip = "localhost", port = 2375L){
                             ip <<- ip
                             port <<- as.integer(port) # pre-empt user
                           },
                           get = function(all = TRUE, limit = NULL, since = NULL, before = NULL, size = NULL){
                             'List containers:
\\describe{
\\item{\\code{all}:}{1/True/true or 0/False/false, Show all containers. Only running containers are shown by default (i.e., this defaults to false)}
\\item{\\code{limit}:}{Show limit last created containers, include non-running ones.}
\\item{\\code{since}:}{Show only containers created since Id, include non-running ones.}
\\item{\\code{before}:}{Show only containers created before Id, include non-running ones.}
\\item{\\code{size}:}{1/True/true or 0/False/false, Show the containers sizes}
}'
                             dUrl <- list(scheme = "http", hostname = ip, port = port
                                          , path = "containers/json", params = NULL
                                          , fragment = NULL, query = list(all = all, limit = limit, since = since, before = before, size = size)
                                          , username = NULL, password = NULL)
                             class(dUrl) <- "url"
                             content(GET(build_url(dUrl)), simplifyDataFrame = TRUE)
                           },
                           
                           inspectContainer = function(id){
                             'Return low-level information on the container id
\\describe{
\\item{\\code{id}:}{Container id.}
}'
                             dUrl <- list(scheme = "http", hostname = ip, port = port
                                          , path = "containers/{{id}}/json", params = NULL
                                          , fragment = NULL, query = NULL
                                          , username = NULL, password = NULL)
                             class(dUrl) <- "url"
                             content(GET(whisker.render(build_url(dUrl))), simplifyDataFrame = TRUE)
                           },
                           
                           containerProcesses = function(id, ps_args = NULL){
                             'List processes running inside the container id
                             \\describe{
                             \\item{\\code{id}:}{Container id.}
                             \\item{\\code{ps_args}:}{ps arguments to use (e.g., aux). docker os dependent see \\url{https://github.com/docker/docker/issues/8075}}
                             }'
                             dUrl <- list(scheme = "http", hostname = ip, port = port
                                          , path = "containers/{{id}}/top", params = NULL
                                          , fragment = NULL, query = list("ps_args" = ps_args)
                                          , username = NULL, password = NULL)
                             class(dUrl) <- "url"
                             res <- content(GET(whisker.render(build_url(dUrl))))
                             setNames(do.call(rbind.data.frame, res[["Processes"]])
                                      , unlist(res$Titles))
                           },
                           
                           containerLogs = function(id, follow = FALSE, stdout = FALSE, stderr = FALSE, timestamps = FALSE, tail = "all"){
                             'Get stdout and stderr logs from the container id
                             \\describe{
                             \\item{\\code{id}:}{Container id.}
                             \\item{\\code{follow}:}{1/True/true or 0/False/false, return stream. Default false}
                             \\item{\\code{stdout}:}{1/True/true or 0/False/false, show stdout log. Default false}
                             \\item{\\code{stderr}:}{1/True/true or 0/False/false, show stderr log. Default false}
                             \\item{\\code{timestamps}:}{1/True/true or 0/False/false, print timestamps for every log line. Default false}
                             \\item{\\code{tail}:}{Output specified number of lines at the end of logs: all or <number>. Default all}
                             }'
                             dUrl <- list(scheme = "http", hostname = ip, port = port
                                          , path = "containers/{{id}}/logs", params = NULL
                                          , fragment = NULL, query = list(follow = follow, stdout = stdout
                                                                          , stderr = stderr, timestamps = timestamps, tail = tail)
                                          , username = NULL, password = NULL)
                             class(dUrl) <- "url"
                             content(GET(whisker.render(build_url(dUrl))))
                           },
                           
                           containerChanges = function(id){
                             'Inspect changes on container id\'s filesystem
\\describe{
\\item{\\code{id}:}{Container id.}
}'
                             dUrl <- list(scheme = "http", hostname = ip, port = port
                                          , path = "containers/{{id}}/changes", params = NULL
                                          , fragment = NULL, query = NULL
                                          , username = NULL, password = NULL)
                             class(dUrl) <- "url"
                             content(GET(whisker.render(build_url(dUrl))), simplifyDataFrame = TRUE)
                           },
                           
                           exportContainer = function(id, filename = tempfile(fileext = ".tar")){
                             'Export the contents of container id
\\describe{
\\item{\\code{id}:}{Container id.}
\\item{\\code{filename}:}{A filename to export the tar to. If NULL is given the tar is returned in RAW format.}
}'
                             dUrl <- list(scheme = "http", hostname = ip, port = port
                                          , path = "containers/{{id}}/export", params = NULL
                                          , fragment = NULL, query = NULL
                                          , username = NULL, password = NULL)
                             class(dUrl) <- "url"
                             if(!is.null(filename)){
                               GET(whisker.render(build_url(dUrl)), write_disk(filename))
                             }else{
                               content(GET(whisker.render(build_url(dUrl))))
                             }
                           }
                         )
)
