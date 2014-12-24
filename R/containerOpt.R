#' Create container options
#'
#' \code{containerOpt}
#' A utility function to create options suitable for creating a container.
#' @export
#' @param Image String value containing the image name to use for the container. Corresponds to docker run IMAGE.
#' @param Hostname A string value containing the desired hostname to use for the container. Corresponds to docker run -h hostname.domain.com.
#' @param Domainname A string value containing the desired domainname to use for the container. Corresponds to docker run -h hostname.domain.com.
#'  \describe{ 
#'  \item{Example:}{As as example: \code{Hostname = "hostname", Domainname = "domain.com"} would correspond to \code{docker run -h hostname.domain.com}}
#'  }
#' @param User A string value containg the user to use inside the container. Corresponds to docker run -u 
#' @param Memory Memory limit in bytes. Corresponds to docker run -m
#' @param MemorySwap Total memory usage (memory + swap); set -1 to disable swap.
#' @param CpuShares An integer value containing the CPU Shares for container (ie. the relative weight vs othercontainers). Corresponds to docker run -c
#' @param Cpuset String value containg the cgroups Cpuset to use. Corresponds to docker run --cpuset=""
#' @param AttachStdin Boolean value, attaches to stdin. Corresponds to docker run -a stdin
#' @param AttachStdout Boolean value, attaches to stdin. Corresponds to docker run -a stdout
#' @param AttachStderr Boolean value, attaches to stdin. Corresponds to docker run -a stderr
#' @param PortSpecs ADD DOCU
#' @param ExposedPorts An object mapping ports to an empty object in the form of: "ExposedPorts": { "<port>/<tcp|udp>: {}" }. See \url{http://stackoverflow.com/questions/20428302/binding-a-port-to-a-host-interface-using-the-rest-api}
#' @param Tty Boolean value, Attach standard streams to a tty, including stdin if it is not closed. Corresponds to docker run -t
#' @param OpenStdin Boolean value, opens stdin. Corresponds to docker run -i.
#' @param StdinOnce Boolean value, opens stdin. Corresponds to docker run -i.
#' @param Env A list of environment variables in the form of VAR=value. Corresponds to docker run -e
#'  \describe{ 
#'  \item{Example:}{As an example: \code{Env = list(MYVAR1=foo1, MYVAR2=foo2)} would correspond to \code{docker run -e MYVAR1=foo1 -e MYVAR2=foo2}}
#'  }
#' @param Cmd Command to run specified as a string or an array of strings. 
#' \describe{ 
#'  \item{Example:}{As an example \code{Image = "ubuntu", Cmd = list("/bin/echo","hello","world")} would correspond to "\code{docker run ubuntu /bin/echo hello world}}
#'  }
#' @param Volumes An object mapping mountpoint paths (strings) inside the container to empty objects.
#' @param WorkingDir A string value containing the working dir for commands to run in. Corresponds to docker run -w
#' @param Entrypoint Set the entrypoint for the container as a string or an array of strings. Corresponds to docker run --entrypoint=""
#' @param NetworkDisabled Boolean value, when true disables neworking for the container. Corresponds to 
#' @param MacAddress Container MAC address. Corresponds to docker run --mac-address=...
#' @param OnBuild ADD DOCU
#' @param HostConfig The host configuration. See \code{\link{hostConfig}}
#' @examples
#' \dontrun{
#' containerOpt()
#' }

containerOpt <- function(Image, Hostname = "", Domainname = "", User = "", Memory = 0
                         , MemorySwap = 0, CpuShares = 0, Cpuset = "", AttachStdin = FALSE
                         , AttachStdout = TRUE, AttachStderr = TRUE, PortSpecs = NA
                         , ExposedPorts = HostConfig[["PortBindings"]][["container"]]
                         , Tty = FALSE, OpenStdin = FALSE, StdinOnce = FALSE, Env = list(), Cmd = NA
                         , Volumes = NULL, WorkingDir = "", Entrypoint = NA, NetworkDisabled = FALSE
                         , MacAddress = "", OnBuild = NA, HostConfig = hostConfig()){
  if(missing(Image)){stop("An Image name is required", call. = FALSE)}
  cO <- list(Image = Image, Hostname = Hostname, Domainname = Domainname, User = User, Memory = Memory
               , MemorySwap = MemorySwap, CpuShares = CpuShares, Cpuset = Cpuset, AttachStdin = AttachStdin
               , AttachStdout = AttachStdout, AttachStderr = AttachStderr, PortSpecs = PortSpecs 
               , ExposedPorts = ExposedPorts, Tty = Tty, OpenStdin = OpenStdin, StdinOnce = StdinOnce 
               , Env = Env, Cmd = Cmd, Volumes = Volumes, WorkingDir = WorkingDir, Entrypoint = Entrypoint
               , NetworkDisabled = NetworkDisabled, MacAddress = MacAddress, OnBuild = OnBuild
               , HostConfig = HostConfig)
  `class<-`(cO, "containerOpts")
}

#' Create host configuration options
#'
#' \code{hostConfig}
#' A utility function to create host configuration options suitable for the HostConfig argument when creating a container.
#' @export
#' @param Binds A character vector of volume bindings for this container. Each volume binding is a string of the form container_path (to create a new volume for the container),
#'  host_path:container_path (to bind-mount a host path into the container), or host_path:container_path:ro (to make the bind-mount read-only inside the container). Corresponds to docker run -v.
#'  \describe{ 
#'  \item{Example:}{As as example: \code{Binds = c("/home/john/fldA:/var/ex/fldA", "/home/john/fldB:/var/ex/fldB")} would correspond to
#'  \code{docker run -v /home/john/fldA:/var/ex/fldA -v /home/john/fldB:/var/ex/fldB}
#'  }}
#'  @param ContainerIDFile Write the container id to this file. Corresponds to docker run -cidfile
#'  \describe{
#'  \item{Example:}{As an example \code{ContainerIDFile = "test.txt"} corresponds to \code{docker run -cidfile="test.txt"}}
#'  }
#'  @param LxcConf A data.frame of key and value columns containing LXC specific configurations. Example data.frame(Key = c("lxc.network.type", "lxc.network.ipv4"
#' ), Value = c("veth", "192.168.1.3/24")). Corresponds to docker run --lxc-conf
#'  @param Privileged Gives the container full access to the host. Specified as a boolean value. Corresponds to docker run --privileged
#'  @param PortBindings A map of exposed container ports and the host port they should map to. It should be specified in the form { <port>/<protocol>: [{ "HostPort": "<port>" }] } 
#'  Take note that port is specified as a string and not an integer value. Corresponds to docker run -p. If HostIp is non empty you may need a port exposed. See ExposedPorts in containerOpt.
#'  \describe{ 
#'  \item{Example:}{As as example: \code{PortBindings = list(`3838/tcp` = list(list(HostIp = "", HostPort = "3838")))} would correspond to
#'  \code{docker run -p 3838:3838}
#'  }
#'  }
#'  @param Links A character vector of links for the container. Each link entry should be of of the form "container_name:alias".
#'  @param PublishAllPorts Allocates a random host port for all of a container's exposed ports. Specified as a boolean value.
#'  @param Dns A character vector of dns servers for the container to use. Example c("8.8.8.8", "8.8.4.4")
#'  @param DnsSearch A character vector of DNS search domains
#'  @param ExtraHosts ADD DOCU
#'  @param VolumesFrom A list of volumes to inherit from another container. Specified in the form <container name>[:<ro|rw>]. Corresponds to docker run -volumes-from. 
#'  @param Devices ADD DOCU
#'  @param NetworkMode ADD DOCU
#'  @param IpcMode ADD DOCU
#'  @param CapAdd ADD DOCU
#'  @param CapDrop ADD DOCU
#'  @param RestartPolicy ADD DOCU
#'  @param SecurityOpt ADD DOCU
#' @examples
#' \dontrun{
#' hostConfig()
#' }

hostConfig <- function(Binds = NA, ContainerIDFile = "", LxcConf = data.frame(Key = character(0), Value = character(0)), Privileged = FALSE
                       , PortBindings = portBindings(), Links = NA, PublishAllPorts = FALSE
                       , Dns = NA, DnsSearch = NA, ExtraHosts = NA, VolumesFrom = NA
                       , Devices = list(), NetworkMode = "bridge", IpcMode = "", CapAdd = NA
                       , CapDrop = NA, RestartPolicy = list(Name = "", MaximumRetryCount = 0L)
                       , SecurityOpt = NA){
  hc <- list(Binds = Binds, ContainerIDFile = ContainerIDFile, LxcConf = LxcConf, Privileged = Privileged
             , PortBindings = PortBindings, Links = Links, PublishAllPorts = PublishAllPorts
             , Dns = Dns, DnsSearch = DnsSearch, ExtraHosts = ExtraHosts, VolumesFrom = VolumesFrom
             , Devices = Devices, NetworkMode = NetworkMode, IpcMode = IpcMode, CapAdd = CapAdd
             , CapDrop = CapDrop, RestartPolicy = RestartPolicy, SecurityOpt = SecurityOpt)
  `class<-`(hc, "hostConfig")
}

#' Create Port Bindings options
#'
#' \code{portBindings}
#' A utility function to create Port Bindings options suitable for the PortBindings argument in \code{\link{hostConfig}}.
#' @export
#' @param ipHostPort host A character vector of host interface and host port listings
#' @param containerPort A character vector of container ports. Format (80, 80/tcp 80/udp etc)
#' @examples
#' \dontrun{
#' portBindings()
#' }

portBindings <- function(ipHostPort = NULL, containerPort = NULL){
  if(length(ipHostPort) != length(containerPort)){
    stop("Number of Host ports and ips should be the same as container ports.")
  }
  if(length(ipHostPort) == 0){
    return(
      structure(list(host = data.frame(interface = character(0), port = character(0), stringsAsFactors = FALSE)
                     , container = data.frame(port = character(0), protocol = character(0), stringsAsFactors = FALSE))
                , class = "portBinding")
    )
  }
  cP <- strsplit(containerPort, "/")
  containerPorts <- lapply(cP, head, 1)
  protocol <- lapply(cP, tail, -1)
  protocol[sapply(protocol, length) == 0] <- "tcp"
  ipHP <- strsplit(ipHostPort, ":")
  hostPorts <- lapply(ipHP, tail, 1)
  hostInterface <- lapply(ipHP, head, -1)
  hostInterface[sapply(hostInterface, length) == 0] <- ""
  structure(list(host = data.frame(interface = unlist(hostInterface), port = unlist(hostPorts), stringsAsFactors = FALSE)
       , container = data.frame(port = unlist(containerPorts), protocol = unlist(protocol), stringsAsFactors = FALSE))
       , class = "portBinding")
}

#' @export
prepareJSON <- function(x, ...){UseMethod("prepareJSON")}

setGeneric("prepareJSON")

#' @export
prepareJSON.containerOpts <- function(x, ...){
  x$ExposedPorts <- if(nrow(x$ExposedPorts) == 0L){
    c()
  }else{
    setNames(list()[seq(nrow(x$ExposedPorts))]
             , paste0(x$ExposedPorts$port, "/", x$ExposedPorts$protocol))
  }
  
  x$HostConfig <- prepareJSON(x$HostConfig)
  x
}

#' @export
prepareJSON.hostConfig <- function(x, ...){
  x$PortBindings <- prepareJSON(x$PortBindings)
  x
}

#' @export
prepareJSON.portBinding <- function(x, ...){
  if(nrow(x$host) == 0L){
    c()
  }else{
    portMap <- paste(x$container$port, x$container$protocol, sep = "/")
    sapply(seq_along(portMap), function(y){
      setNames(list(data.frame("HostIp" = x$host[y, "interface"]
                                   , "HostPort" = x$host[y, "port"], stringsAsFactors = FALSE))
               , portMap[y])
    })
  }
}
