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
                                   if(is.null(ncol(appports[[1]])) || ncol(appports[[1]]) == 0){
                                     appports[[1]] <- data.frame(IP = character(0), PrivatePort = numeric(0), PublicPort = numeric(0), Type = character(0))
                                   }
                                   id <<- id
                                   created <<- created
                                   image <<- image
                                   names <<- names
                                   ports <<- appports
                                   status <<- status
                                   command <<- command
                                   callSuper(...)
                                 },
                                 
                                 inspect = function(...){
                                   'Return low-level information on the container.
                                    \\describe{
                                    \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                                    }
                                   '
                                   dUrl <- dockerUrl
                                   dUrl["path"] <- list("containers/{{appid}}/json")
                                   appid <- id
                                   checkResponse(GET(whisker.render(build_url(dUrl)), ...), pass = c(200L))
                                   content(response, simplifyDataFrame = TRUE)
                                 },
                                 
                                 listProcesses = function(ps_args = NULL, ...){
                                   'List processes running inside the container.
                                    \\describe{
                                    \\item{\\code{ps_args}:}{ps arguments to use (e.g., aux). docker os dependent see \\url{https://github.com/docker/docker/issues/8075}}
                                    \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                                    }'
                                   dUrl <- dockerUrl
                                   dUrl[c("path", "query")] <- list("containers/{{appid}}/top"
                                                                    , list("ps_args" = ps_args))
                                   appid <- id
                                   checkResponse(GET(whisker.render(build_url(dUrl)), ...), pass = c(200L))
                                   res <- content(response)
                                   setNames(do.call(rbind.data.frame, res[["Processes"]])
                                            , unlist(res$Titles))
                                 },
                                 
                                 logs = function(stdout = FALSE, stderr = FALSE, timestamps = FALSE, tail = "all", ...){
                                   'Get stdout and stderr logs from the container.
                          \\describe{
                          \\item{\\code{stdout}:}{1/True/true or 0/False/false, show stdout log. Default false}
                          \\item{\\code{stderr}:}{1/True/true or 0/False/false, show stderr log. Default false}
                          \\item{\\code{timestamps}:}{1/True/true or 0/False/false, print timestamps for every log line. Default false}
                          \\item{\\code{tail}:}{Output specified number of lines at the end of logs: all or <number>. Default all}
                          \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                          }'
                                   dUrl <- dockerUrl
                                   dUrl[c("path", "query")] <- list("containers/{{appid}}/logs"
                                                                    , list(follow = FALSE, stdout = stdout
                                                                           , stderr = stderr, timestamps = timestamps, tail = tail))
                                   appid <- id
                                   checkResponse(GET(whisker.render(build_url(dUrl)), ...), pass = c(200L))
                                   out <- content(response)
                                   capture.output(cat(rawToChar(out[!out == as.raw(0)])))
                                 },
                                 
                                 fsChanges = function(...){
                                   'Inspect changes on containers filesystem.
                                    \\describe{
                                     \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                                    }
                                   '
                                   dUrl <- dockerUrl
                                   dUrl["path"] <- list("containers/{{appid}}/changes")
                                   appid <- id
                                   checkResponse(GET(whisker.render(build_url(dUrl)), ...), pass = c(200L))
                                   content(response, simplifyDataFrame = TRUE)
                                 },
                                 
                                 export = function(filename = tempfile(fileext = ".tar"), ...){
                                   'Export the contents of the container.
                                   \\describe{
                                   \\item{\\code{filename}:}{A filename to export the tar to. If NULL is given the tar is returned in RAW format.}
                                   \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                                   }
                                   '
                                   dUrl <- dockerUrl
                                   dUrl["path"] <- list("containers/{{appid}}/export")
                                   appid <- id
                                   if(!is.null(filename)){
                                     checkResponse(GET(whisker.render(build_url(dUrl)), write_disk(filename), ...), pass = c(200L))
                                   }else{
                                     checkResponse(GET(whisker.render(build_url(dUrl)), ...), pass = c(200L))
                                     content(response)
                                   }
                                 },
                                 
                                 resize = function(height = NULL, width = NULL, ...){
                                   'Resize the TTY of the container.
                                   \\describe{
                                   \\item{\\code{height}:}{Height for the resized container.}
                                   \\item{\\code{width}:}{Width for the resized container.}
                                   \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                                   }'
                                   if(is.null(height) || is.null(width)){
                                     stop("Please provide a height and width for the resized container.")
                                   }
                                   dUrl <- dockerUrl
                                   dUrl[c("path", "query")] <- list("containers/{{appid}}/resize"
                                                                    , list(height = height, width = width))
                                   appid <- id
                                   checkResponse(GET(whisker.render(build_url(dUrl)), ...), pass = c(200L))
                                   content(response)
                                 },
                                 
                                 stop = function(t = NULL, ...){
                                   'Stop the container.
                                   \\describe{
                                    \\item{\\code{t}:}{number of seconds to wait before killing the container.}
                                    \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                                   }
                                   '
                                   dUrl <- dockerUrl
                                   dUrl[c("path", "query")] <- list("containers/{{appid}}/stop"
                                                                    , list(t = t))
                                   appid <- id
                                   checkResponse(POST(whisker.render(build_url(dUrl)), ...), pass = c(204L)
                                                 , errors = data.frame(status_code = 304, message = "container already stopped", stringsAsFactors = FALSE))
                                 },
                                 
                                 restart = function(t = NULL, ...){
                                   'Restart the container.
                                   \\describe{
                                    \\item{\\code{t}:}{number of seconds to wait before killing the container.}
                                    \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                                   }
                                   '
                                   dUrl <- dockerUrl
                                   dUrl[c("path", "query")] <- list("containers/{{appid}}/restart"
                                                                    , list(t = t))
                                   appid <- id
                                   checkResponse(POST(whisker.render(build_url(dUrl)), ...), pass = c(204L))
                                 },
                                 
                                 kill = function(...){
                                   'Kill the container.
                                   \\describe{
                                   \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                                   }
                                   '
                                   dUrl <- dockerUrl
                                   dUrl["path"] <- list("containers/{{appid}}/kill")
                                   appid <- id
                                   checkResponse(POST(whisker.render(build_url(dUrl)), ...), pass = c(204L))
                                 },
                                 
                                 pause = function(...){
                                   'Pause the container
                                   \\describe{
                                   \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                                   }
                                   '
                                   dUrl <- dockerUrl
                                   dUrl["path"] <- list("containers/{{appid}}/pause")
                                   appid <- id
                                   checkResponse(POST(whisker.render(build_url(dUrl)), ...), pass = c(204L))
                                 },
                                 
                                 unpause = function(...){
                                   'Unpause the container.
                                   \\describe{
                                    \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                                   }
                                   '
                                   dUrl <- dockerUrl
                                   dUrl["path"] <- list("containers/{{appid}}/unpause")
                                   appid <- id
                                   checkResponse(POST(whisker.render(build_url(dUrl)), ...), pass = c(204L))
                                 },
                                 
                                 remove = function(force = FALSE, v = FALSE, ...){
                                   'Create an image, either by pulling it from the registry or by importing it.
                                   \\describe{
                                   \\item{\\code{force}:}{1/True/true or 0/False/false, default false.}
                                   \\item{\\code{v}:}{1/True/true or 0/False/false, Remove the volumes associated to the container. Default false}
                                   \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                                   }'
                               
                                   dUrl <- dockerUrl
                                   dUrl[c("path", "query")] <- list("containers/{{appid}}"
                                                                    , list(force = force, v = v))
                                   appid <- id
                                   checkResponse(DELETE(whisker.render(build_url(dUrl)), ...), pass = c(204L))
                                   content(response, simplifyDataFrame = TRUE)
                                 },
                                 
                                 start = function(Binds = NULL, Links = NULL, LxcConf = NULL, PortBindings = NULL
                                                           , PublishAllPorts = FALSE, Privileged = FALSE, Dns = NULL, DnsSearch = NULL
                                                           , VolumesFrom = NULL, CapAdd = NULL, Capdrop = NULL
                                                           , RestartPolicy = "\"Name\": \"on-failure\""
                                                           , NetworkMode = "Bridge"
                                                           , Devices = NULL, ...){
                                   'Start the container id
                                   \\describe{
                                   \\item{\\code{Binds}:}{ A list of volume bindings for this container. Each volume binding is a string of the form container_path (to create a new volume for the container), host_path:container_path (to bind-mount a host path into the container), or host_path:container_path:ro (to make the bind-mount read-only inside the container).}
                                   \\item{\\code{Links}:}{A list of links for the container. Each link entry should be of of the form "container_name:alias"}
                                   \\item{\\code{LxcConf}:}{LXC specific configurations. These configurations will only work when using the lxc execution driver.}
                                   \\item{\\code{PortBindings}:}{ A map of exposed container ports and the host port they should map to. It should be specified in the form { <port>/<protocol>: [{ "HostPort": "<port>" }] } Take note that port is specified as a string and not an integer value.}
                                   \\item{\\code{PublishAllPorts}:}{ Allocates a random host port for all of a container\'s exposed ports. Specified as a boolean value.}
                                   \\item{\\code{Privileged}:}{Gives the container full access to the host. Specified as a boolean value.}
                                   \\item{\\code{Dns}:}{A list of dns servers for the container to use.}
                                   \\item{\\code{DnsSearch}:}{A list of DNS search domains}
                                   \\item{\\code{VolumesFrom}:}{A list of volumes to inherit from another container. Specified in the form <container name>[:<ro|rw>]}
                                   \\item{\\code{CapAdd}:}{A list of kernel capabilties to add to the container.}
                                   \\item{\\code{Capdrop}:}{A list of kernel capabilties to drop from the container.}
                                   \\item{\\code{RestartPolicy}:}{The behavior to apply when the container exits. The value is an object with a Name property of either "always" to always restart or "on-failure" to restart only when the container exit code is non-zero. If on-failure is used, MaximumRetryCount controls the number of times to retry before giving up. The default is not to restart. (optional)}
                                   \\item{\\code{NetworkMode}:}{Sets the networking mode for the container. Supported values are: bridge, host, and container:<name|id>}
                                   \\item{\\code{Devices}:}{A list of devices to add to the container specified in the form { "PathOnHost": "/dev/deviceName", "PathInContainer": "/dev/deviceName", "CgroupPermissions": "mrw"}}
                                   \\item{\\code{...}:}{Additional arguments to pass to httr functions \\code{\\link{GET}}, \\code{\\link{POST}} etc.}
                                   }'
                                   dUrl <- dockerUrl
                                   dUrl[c("path", "query")] <- list("containers/{{id}}/start"
                                                                    , list())
                                   content(GET(whisker.render(build_url(dUrl)), ...))
                                 }
                               )
)