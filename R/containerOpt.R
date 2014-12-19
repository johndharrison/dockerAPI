#' Create container options
#'
#' \code{containerOpt}
#' A utility function to create options suitable for creating a container.
#' @export
#' @param Image String value containing the image name to use for the container. Corresponds to docker run IMAGE.
#' @param Hostname A string value containing the desired hostname to use for the container. Corresponds to docker run -h.
#' @param Domainname A string value containing the desired domainname to use for the container. Corresponds to -h option for docker run cli.
#' @examples
#' \dontrun{
#' containerOpt()
#' }

containerOpt <- function(Image, Hostname = "", Domainname = "", User = "", Memory = 0
                         , MemorySwap = 0, CpuShares = 0, Cpuset = "", AttachStdin = FALSE
                         , AttachStdout = TRUE, AttahcStderr = TRUE, PortSpecs = NULL, ExposedPorts = list()
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
  hc <- list(Binds = Binds, ContainerIDFile = ContainerIDFile, LxcConf = LxcConf)
  `class<-`(hc, "hostConfig")
}