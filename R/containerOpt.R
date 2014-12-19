#' Create container options
#'
#' \code{containerOpt}
#' A utility function to create options suitable for creating a container.
#' @export
#' @param Image String value containing the image name to use for the container. Corresponds to docker run IMAGE.
#' @param Hostname A string value containing the desired hostname to use for the container. Corresponds to docker run -h hostname.domain.com.
#' @param Domainname A string value containing the desired domainname to use for the container. Corresponds to docker run -h hostname.domain.com.
#'  \describe{ As as example: \code{Hostname = "hostname", Domainname = "domain.com"} would correspond to \code{docker run -h hostname.domain.com}}
#' @param User A string value containg the user to use inside the container. Corresponds to docker run -u 
#' @param Memory Memory limit in bytes. Corresponds to docker run -m
#' @param MemorySwap Total memory usage (memory + swap); set -1 to disable swap.
#' @param CpuShares An integer value containing the CPU Shares for container (ie. the relative weight vs othercontainers). Corresponds to docker run -c
#' @param Cpuset String value containg the cgroups Cpuset to use. Corresponds to docker run --cpuset=""
#' @param AttachStdin Boolean value, attaches to stdin. Corresponds to docker run -a stdin
#' @param AttachStdout Boolean value, attaches to stdin. Corresponds to docker run -a stdout
#' @param AttachStderr Boolean value, attaches to stdin. Corresponds to docker run -a stderr
#' @examples
#' \dontrun{
#' containerOpt()
#' }

containerOpt <- function(Image, Hostname = "", Domainname = "", User = "", Memory = 0
                         , MemorySwap = 0, CpuShares = 0, Cpuset = "", AttachStdin = FALSE
                         , AttachStdout = TRUE, AttachStderr = TRUE, PortSpecs = NULL, ExposedPorts = list()
                         , Tty = FALSE, OpenStdin = FALSE, StdinOnce = FALSE, Env = list(), Cmd = NULL
                         , Volumes = list(), WorkingDir = "", Entrypoint = NULL, NetworkDisabled = FALSE
                         , MacAddress = "", OnBuild = NULL, HostConfig = hostconfig()){
  
}

#' Create host configuration options
#'
#' \code{hostConfig}
#' A utility function to create host configuration options suitable for the HostConfig argument when creating a container.
#' @export
#' @param Binds A list of volume bindings for this container. Each volume binding is a string of the form container_path (to create a new volume for the container),
#'  host_path:container_path (to bind-mount a host path into the container), or host_path:container_path:ro (to make the bind-mount read-only inside the container). Corresponds to docker run -v.
#'  \describe{ As as example: \code{Binds = list("/home/john/fldA:/var/ex/fldA", "/home/john/fldB:/var/ex/fldB")} would correspond to
#'  \code{docker run -v /home/john/fldA:/var/ex/fldA -v /home/john/fldB:/var/ex/fldB}
#'  }
#'  @param ContainerIDFile
#'  @param LxcConf
#'  @param Privileged
#'  @param PortBindings A map of exposed container ports and the host port they should map to. It should be specified in the form { <port>/<protocol>: [{ "HostPort": "<port>" }] } 
#'  Take note that port is specified as a string and not an integer value. Corresponds to docker run -p.
#'  \describe{ As as example: \code{PortBindings = list(`3838/tcp` = list(list(HostIp = "", HostPort = "3838")))} would correspond to
#'  \code{docker run -p 3838:3838}
#'  }
#' @examples
#' \dontrun{
#' hostConfig()
#' }

hostConfig <- function(Binds = NULL, ContainerIDFile = "", LxcConf = list(), Privileged = FALSE
                       , PortBindings = list(), Links = NULL, PublishAllPorts = FALSE
                       , Dns = NULL, DnsSearch = NULL, ExtraHosts = NULL, VolumesFrom = NULL
                       , Devices = list(), NetworkMode = "Bridge", IpcMode = "", CapAdd = NULL
                       , CapDrop = NULL, RestartPolicy = list(Name = "", MaximumRetryCount = 0L)
                       , SecurityOpt = NULL){
  hc <- list(Binds = Binds, ContainerIDFile = ContainerIDFile, LxcConf = LxcConf, Privileged = Privileged
             , PortBindings = PortBindings, Links = Links, PublishAllPorts = PublishAllPorts
             , Dns = Dns, DnsSearch = DnsSearch, ExtraHosts = ExtraHosts, VolumesFrom = VolumesFrom
             , Devices = Devices, NetworkMode = NetworkMode, IpcMode = IpcMode, CapAdd = CapAdd
             , CapDrop = CapDrop, RestartPolicy = RestartPolicy, SecurityOpt = SecurityOpt)
  `class<-`(hc, "hostConfig")
}