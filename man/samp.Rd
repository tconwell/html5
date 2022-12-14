% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/html5.R
\name{samp}
\alias{samp}
\title{Generate the <samp> HTML tag.}
\usage{
samp(
  ...,
  attr = NULL,
  separate = FALSE,
  collapse = "",
  formatted = html5_vars$formatted
)
}
\arguments{
\item{...}{A string or vector of content to pass to the tag.}

\item{attr}{A named list or named vector, names are attribute names and values are attribute values.}

\item{separate}{TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.}

\item{collapse}{A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.}

\item{formatted}{TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE}
}
\value{
A HTML tag string.
}
\description{
The <samp> HTML element is used to enclose inline text which represents sample (or quoted) output from a computer program. Its contents are typically rendered using the browser's default monospaced font (such as Courier or Lucida Console).
}
\details{
Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp}.
}
\examples{
samp(attr = list(class = "test"))
}
