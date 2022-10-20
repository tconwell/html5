
# html5

<!-- badges: start -->
<!-- badges: end -->

html5 makes it easy to build strings of valid HTML5 via R functions. This can 
be used to create entire HTML documents or dynamically created HTML snippets. 

It also can create nicely indented HTML by setting the environment variable 
html5_vars$formatted to TRUE, but this comes with a performance slow down.

Generates valid HTML tag strings for HTML5 elements documented by Mozilla. 
Attributes are passed as named lists, with names being the attribute name and values being the attribute value. 
Attribute values are automatically double-quoted. To declare a DOCTYPE, wrap html() with function doctype().
Mozilla's documentation for HTML5 is available here: <https://developer.mozilla.org/en-US/docs/Web/HTML/Element>.
Elements marked as obsolete are not included. 

## Installation

You can install the package html5 from [CRAN](https://cran.r-project.org/) with:

``` r
install.packages("html5")
```

You can install the development version of html5 from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tconwell/html5")
```

## Example: Creating a HTML document:

``` r
library(html5)
doctype(
  html(
    head(
      meta(attr = c(name = "author", content = "Timothy Conwell"))
    ),
    body(
      div(
        attr = c(class = "example class"),
        h1("Hello"),
        p("Here is some HTML")
      )
    )
  )
)
```

## Example: Creating a formatted HTML document:

``` r
library(html5)
html5_vars$formatted <- TRUE
doctype(
  html(
    head(
      meta(attr = c(name = "author", content = "Timothy Conwell"))
    ),
    body(
      div(
        attr = c(class = "example class"),
        h1("Hello"),
        p("Here is some HTML")
      )
    )
  )
)
```

## Example: Creating separate tags for each input item

This shows you how to create separate tags for each item of a vector (this is 
usually faster than using lapply but might be confusing when reviewing code):

``` r
library(html5)
## passing a vector
li(
  c("One", "Two", "Three")
)
## not a vector, but setting separate = TRUE
li(
  "One", "Two", "Three",
  separate = TRUE
)
## it also works with attributes if attr is a list.
li(
  c("One", "Two", "Three"), 
  attr = list(class = c("a", "b", "c")),
  separate = TRUE
)
```
