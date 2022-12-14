% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/html5.R
\name{base}
\alias{base}
\title{Generate the <base> HTML tag.}
\usage{
base(
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
The <base> HTML element specifies the base URL to use for all relative URLs in a document. There can be only one <base> element in a document.
}
\details{
Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/base}.
}
\examples{
base(attr = list(class = "test"))
}
