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
                             }
                             '
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
                           },
                           
                           resizeContainer = function(id, height = NULL, width = NULL){
                             'Resize the TTY of container id
                             \\describe{
                             \\item{\\code{id}:}{Container id.}
                             \\item{\\code{height}:}{Height for the resized container.}
                             \\item{\\code{width}:}{Width for the resized container.}
                             }'
                             if(is.null(height) || is.null(width)){
                               stop("Please provide a height and width for the resized container.")
                             }
                             dUrl <- list(scheme = "http", hostname = ip, port = port
                                          , path = "containers/{{id}}/resize", params = NULL
                                          , fragment = NULL, query = list(height = height, width = width)
                                          , username = NULL, password = NULL)
                             class(dUrl) <- "url"
                             content(GET(whisker.render(build_url(dUrl))))
                           },
                           
                           startContainer = function(id, Binds = NULL, Links = NULL, LxcConf = NULL, PortBindings = NULL
                                                      , PublishAllPorts = FALSE, Privileged = FALSE, Dns = NULL, DnsSearch = NULL
                                                     , VolumesFrom = NULL, CapAdd = NULL, Capdrop = NULL
                                                     , RestartPolicy = "\"Name\": \"on-failure\""
                                                     , NetworkMode = "Bridge"
                                                     , Devices = NULL){
                             'Start the container id
                             \\describe{
                             \\item{\\code{id}:}{Container id.}
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
                             }'
                           dUrl <- list(scheme = "http", hostname = ip, port = port
                                        , path = "containers/{{id}}/start", params = NULL
                                        , fragment = NULL, query = NULL
                                        , username = NULL, password = NULL)
                           class(dUrl) <- "url"
                           content(GET(whisker.render(build_url(dUrl))))
                           },
                           
                           stopContainer = function(id, t = NULL){
                             'Stop the container id
                             \\describe{
                             \\item{\\code{id}:}{Container id.}
                             \\item{\\code{t}:}{number of seconds to wait before killing the container.}
                             }
                             '
                             dUrl <- list(scheme = "http", hostname = ip, port = port
                                          , path = "containers/{{id}}/stop", params = NULL
                                          , fragment = NULL, query = NULL
                                          , username = NULL, password = NULL)
                             class(dUrl) <- "url"
                             POST(whisker.render(build_url(dUrl)), body = list(t = t))
                           }
                         )
)
