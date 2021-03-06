% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/containerOpt.R
\name{hostConfig}
\alias{hostConfig}
\title{Create host configuration options}
\usage{
hostConfig(Binds = NA, ContainerIDFile = "", LxcConf = data.frame(Key =
  character(0), Value = character(0)), Privileged = FALSE,
  PortBindings = portBindings(), Links = NA, PublishAllPorts = FALSE,
  Dns = NA, DnsSearch = NA, ExtraHosts = NA, VolumesFrom = NA,
  Devices = list(), NetworkMode = "bridge", IpcMode = "", CapAdd = NA,
  CapDrop = NA, RestartPolicy = list(Name = "", MaximumRetryCount = 0L),
  SecurityOpt = NA)
}
\arguments{
\item{Binds}{A character vector of volume bindings for this container. Each volume binding is a string of the form container_path (to create a new volume for the container),
host_path:container_path (to bind-mount a host path into the container), or host_path:container_path:ro (to make the bind-mount read-only inside the container). Corresponds to docker run -v.
\describe{
\item{Example:}{As as example: \code{Binds = c("/home/john/fldA:/var/ex/fldA", "/home/john/fldB:/var/ex/fldB")} would correspond to
\code{docker run -v /home/john/fldA:/var/ex/fldA -v /home/john/fldB:/var/ex/fldB}
}}}

\item{ContainerIDFile}{Write the container id to this file. Corresponds to docker run -cidfile
\describe{
\item{Example:}{As an example \code{ContainerIDFile = "test.txt"} corresponds to \code{docker run -cidfile="test.txt"}}
}}

\item{LxcConf}{A data.frame of key and value columns containing LXC specific configurations. Example data.frame(Key = c("lxc.network.type", "lxc.network.ipv4"
), Value = c("veth", "192.168.1.3/24")). Corresponds to docker run --lxc-conf}

\item{Privileged}{Gives the container full access to the host. Specified as a boolean value. Corresponds to docker run --privileged}

\item{PortBindings}{A map of exposed container ports and the host port they should map to. It should be specified in the form { <port>/<protocol>: [{ "HostPort": "<port>" }] }
Take note that port is specified as a string and not an integer value. Corresponds to docker run -p. If HostIp is non empty you may need a port exposed. See ExposedPorts in containerOpt.
\describe{
\item{Example:}{As as example: \code{PortBindings = list(`3838/tcp` = list(list(HostIp = "", HostPort = "3838")))} would correspond to
\code{docker run -p 3838:3838}
}
}}

\item{Links}{A character vector of links for the container. Each link entry should be of of the form "container_name:alias".}

\item{PublishAllPorts}{Allocates a random host port for all of a container's exposed ports. Specified as a boolean value.}

\item{Dns}{A character vector of dns servers for the container to use. Example c("8.8.8.8", "8.8.4.4")}

\item{DnsSearch}{A character vector of DNS search domains}

\item{ExtraHosts}{ADD DOCU}

\item{VolumesFrom}{A list of volumes to inherit from another container. Specified in the form <container name>[:<ro|rw>]. Corresponds to docker run -volumes-from.}

\item{Devices}{ADD DOCU}

\item{NetworkMode}{ADD DOCU}

\item{IpcMode}{ADD DOCU}

\item{CapAdd}{ADD DOCU}

\item{CapDrop}{ADD DOCU}

\item{RestartPolicy}{ADD DOCU}

\item{SecurityOpt}{ADD DOCU}
}
\description{
\code{hostConfig}
A utility function to create host configuration options suitable for the HostConfig argument when creating a container.
}
\examples{
\dontrun{
hostConfig()
}
}

