% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/dockerImage.R
\docType{class}
\name{dockerImage-class}
\alias{dockerImage}
\alias{dockerImage-class}
\title{docker Class uses the}
\description{
dockerImage is a generator object.
}
\section{Methods}{

\describe{
\item{\code{history(...)}}{Return the history of the image.
\describe{
 \item{\code{...}:}{Additional arguments to pass to httr functions \code{\link{GET}}, \code{\link{POST}} etc.}
}}

\item{\code{inspect(...)}}{Return low-level information on the image.
\describe{
 \item{\code{...}:}{Additional arguments to pass to httr functions \code{\link{GET}}, \code{\link{POST}} etc.}
}}

\item{\code{remove(force = FALSE, noprune = FALSE, ...)}}{Remove an image
\describe{
 \item{\code{force}:}{1/True/true or 0/False/false, default false.}
 \item{\code{noprune}:}{1/True/true or 0/False/false, default false.}
 \item{\code{...}:}{Additional arguments to pass to httr functions \code{\link{GET}}, \code{\link{POST}} etc.}
 }}
}}
\examples{
\dontrun{
myConfig <- httr::config(sslcert = "../../certs/cert.pem"
, sslkey = "../../certs/key.pem"
, sslversion=1L, ssl.verifypeer = FALSE)
dckr <- docker("https://192.168.59.103:2376", myConfig)
dckr$getContainers()
}
}

