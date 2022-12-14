% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/html5.R
\name{keygen}
\alias{keygen}
\title{Generate the <keygen> HTML tag.}
\usage{
keygen(
  attr = NULL,
  separate = FALSE,
  collapse = "",
  formatted = html5_vars$formatted
)
}
\arguments{
\item{attr}{A named list or named vector, names are attribute names and values are attribute values.}

\item{separate}{TRUE/FALSE, If TRUE, returns separate tags for each item of attr if length of that item is greater than 1; if FALSE, returns one tag.}

\item{collapse}{A string. If NULL, returns a vector the same length as the longest item of attr, instead of one string.}

\item{formatted}{TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE}
}
\value{
A HTML tag string.
}
\description{
The <keygen> HTML element exists to facilitate generation of key material, and submission of the public key as part of an HTML form. This mechanism is designed for use with Web-based certificate management systems. It is expected that the <keygen> element will be used in an HTML form along with other information needed to construct a certificate request, and that the result of the process will be a signed certificate.
}
\details{
Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen}.
}
\examples{
keygen(attr = list(class = "test"))
}
