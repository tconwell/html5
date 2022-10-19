# boolean_attributes <- c(
#   "allowfullscreen",
#   "allowpaymentrequest",
#   "async",
#   "autofocus",
#   "autoplay",
#   "checked",
#   "controls",
#   "default",
#   "defer",
#   "disabled",
#   "formnovalidate",
#   "hidden",
#   "ismap",
#   "itemscope",
#   "loop",
#   "multiple",
#   "muted",
#   "nomodule",
#   "novalidate",
#   "open",
#   "playsinline",
#   "readonly",
#   "required",
#   "reversed",
#   "selected",
#   "truespeed"
# )
# html5_vars <- new.env()
# html5_vars$formatted <- FALSE
# save(boolean_attributes, html5_vars, file = "/home/tim/r_packages/html5/R/sysdata.rda")

#' Vector of boolean attributes
#'
#' @description A vector of boolean attributes
#' @format A vector
"boolean_attributes"

#' Environment to set formatted to TRUE/FALSE
#'
#' @description Environment to set formatted to TRUE/FALSE
#' @format An environment
"html5_vars"

#' Helper function to generate HTML5 attribute strings
#'
#' @param attr A named list, names are attribute names and values are attribute values.
  #' If the items of the list and the items of the tag content are longer than length 1,
  #' the items for the attribute will correspond with the items of the content in the same position.
  #' (ex. when generating a series of option tags, you might want to pass a different id attribute for each
  #' item of the content, you can pass the vector of ids in the named list of attributes)
#' @param separate TRUE/FALSE, if TRUE, returns a vector for creating multiple tags at once.
#' @return A HTML attribute string.
#' @examples
#' attr_helper(attr = list(class = 'test'))
attr_helper <- function (attr, separate = FALSE)
{
    if (length(attr) > 0) {
        if (separate) {
            attr_names <- names(attr)
            attr <- lapply(attr_names, function(x) {
                if (x %in% boolean_attributes) {
                  i <- rep_len(x, length(attr[[x]]))
                  i[attr[[x]] == FALSE] <- ""
                  return(i)
                }
                else {
                  return(paste0(x, "=", paste0("\"", attr[[x]], "\"")))
                }
            })
            attr <- attr[attr != ""]
            i <- seq_len(length(attr))
            return(eval(str2expression(paste0("paste0(' ', paste0(", paste0("attr[[", i, "]]", collapse = ", \" \", "), "))"))))
        }
        else {
            booleans <- names(attr) %in% boolean_attributes
            attr <- attr[(booleans & attr == TRUE) | !(booleans)]
            attr_names <- names(attr)
            booleans <- attr_names %in% boolean_attributes
            attr[booleans] <- attr_names[booleans]
            attr[!booleans] <- paste0(attr_names[!booleans], "=", paste0("\"", attr[!booleans], "\""))
            return(paste0(" ", paste0(attr, collapse = " ")))
        }
    }
    else {
        return("")
    }
}

#' Helper function to generate HTML5 strings with end tags
#'
#' @param ... A string or strings or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE. If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param tag A string. The HTML5 tag to return.
#' @return A HTML tag string.
#' @examples
#' tag_helper(attr = list(class = 'test'), tag = 'a')
tag_helper <- function (..., attr = NULL, separate = FALSE, collapse = "", tag)
{
    return(if (separate) {
        if (is.null(attr)) {
            paste0("<", tag, ">", c(...), "</", tag, ">", collapse = collapse)
        } else {
            paste0("<", tag, attr_helper(as.list(attr), separate = separate), ">", c(...), "</", tag, ">", collapse = collapse)
        }
    } else {
        if (is.null(attr)) {
            paste0("<", tag, ">", ..., "</", tag, ">", collapse = collapse)
        } else {
            paste0("<", tag, attr_helper(as.list(attr), separate = separate), ">", ..., "</", tag, ">", collapse = collapse)
        }
    })
}

#' Helper function to generate HTML5 strings without end tags
#'
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE. If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param tag A string. The HTML5 tag to return.
#' @return A HTML tag string.
#' @examples
#' empty_tag_helper(attr = list(class = 'test'), tag = 'meta')
empty_tag_helper <- function (attr = NULL, separate = FALSE, collapse = "", tag)
{
    return(if (separate) {
        if (is.null(attr)) {
            paste0("<", tag, ">", collapse = collapse)
        } else {
            paste0("<", tag, attr_helper(as.list(attr), separate = separate), ">", collapse = collapse)
        }
    } else {
        if (is.null(attr)) {
            paste0("<", tag, ">", collapse = collapse)
        } else {
            paste0("<", tag, attr_helper(as.list(attr), separate = separate), ">", collapse = collapse)
        }
    })
}

#' Add new lines and tabs to format HTML content.
#'
#' @param x A string of HTML to format.
#' @return A HTML string formatted with new lines and tabs.
content_indenter <- function (x)
{
    x <- strsplit(x, "\n", fixed = TRUE)[[1]]
    return(paste0("\n\t", x, collapse = ""))
}

#' Helper function to generate HTML5 strings formatted with new lines and tabs
#'
#' @param ... A string or strings or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE. If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param accepts_content TRUE/FALSE, if FALSE, does not include any content or end tag (ex: the input tag).
#' @param tag A string. The HTML5 tag to return.
#' @return A HTML tag string.
#' @examples
#' formatted_tag_helper(attr = list(class = 'test'), tag = 'a')
formatted_tag_helper <- function (..., attr = NULL, separate = FALSE, collapse = "", accepts_content = TRUE, tag)
{
    content <- as.character(c(...))
    open_tag <- paste0("<", tag, attr_helper(as.list(attr), separate = separate), ">")
    if (accepts_content) {
        if (length(content) == 0 | all(nchar(content) == 0)) {
            content <- "\n"
        }
        else {
            if (separate) {
                content <- unlist(lapply(content, content_indenter), use.names = FALSE)
            }
            else {
                content <- content_indenter(paste0(content, collapse = ""))
            }
            content <- paste0(content, "\n")
        }
        end_tag <- paste0("</", tag, ">\n")
    }
    else {
        open_tag <- paste0(open_tag, "\n")
        content <- ""
        end_tag <- ""
    }
    return(paste0(open_tag, content, end_tag, collapse = collapse))
}

#' Generate HTML document string with properly declared DOCTYPE.
#'
#' @param ... A string or strings of HTML element tags.
#' @param doctype A string declaring the DOCTYPE for the HTML content.
#' @return A HTML document string.
doctype <- function (..., doctype = "html")
{
    return(paste0("<!DOCTYPE ", doctype, ">\n", ...))
}

#'Generate the <a> HTML tag.
#'
#' The <a> HTML element (or anchor element), with its href attribute, creates a hyperlink to web pages, files, email addresses, locations in the same page, or anything else a URL can address.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' a(attr = list(class = "test"))
a <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'a'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'a'))
	}
}

#'Generate the <abbr> HTML tag.
#'
#' The <abbr> HTML element represents an abbreviation or acronym.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' abbr(attr = list(class = "test"))
abbr <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'abbr'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'abbr'))
	}
}

#'Generate the <address> HTML tag.
#'
#' The <address> HTML element indicates that the enclosed HTML provides contact information for a person or people, or for an organization.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' address(attr = list(class = "test"))
address <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'address'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'address'))
	}
}

#'Generate the <applet> HTML tag.
#'
#' The obsolete HTML Applet Element (<applet>) embeds a Java applet into the document; this element has been deprecated in favor of object.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/applet}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' applet(attr = list(class = "test"))
applet <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'applet'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'applet'))
	}
}

#'Generate the <area> HTML tag.
#'
#' The <area> HTML element defines an area inside an image map that has predefined clickable areas. An image map allows geometric areas on an image to be associated with Hyperlink.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/area}.
#'
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of attr if length of that item is greater than 1; if FALSE, returns one tag.
#' @param collapse A string. If NULL, returns a vector the same length as the longest item of attr, instead of one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' area(attr = list(class = "test"))
area <- function(attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(attr = attr, separate = separate, collapse = collapse, accepts_content = FALSE, tag = 'area'))
	}else{
		return(empty_tag_helper(attr = attr, separate = separate, collapse = collapse, tag = 'area'))
	}
}

#'Generate the <article> HTML tag.
#'
#' The <article> HTML element represents a self-contained composition in a document, page, application, or site, which is intended to be independently distributable or reusable (e.g., in syndication). Examples include: a forum post, a magazine or newspaper article, or a blog entry, a product card, a user-submitted comment, an interactive widget or gadget, or any other independent item of content.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' article(attr = list(class = "test"))
article <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'article'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'article'))
	}
}

#'Generate the <aside> HTML tag.
#'
#' The <aside> HTML element represents a portion of a document whose content is only indirectly related to the document's main content. Asides are frequently presented as sidebars or call-out boxes.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' aside(attr = list(class = "test"))
aside <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'aside'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'aside'))
	}
}

#'Generate the <audio> HTML tag.
#'
#' The <audio> HTML element is used to embed sound content in documents. It may contain one or more audio sources, represented using the src attribute or the source element: the browser will choose the most suitable one. It can also be the destination for streamed media, using a MediaStream.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' audio(attr = list(class = "test"))
audio <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'audio'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'audio'))
	}
}

#'Generate the <b> HTML tag.
#'
#' The <b> HTML element is used to draw the reader's attention to the element's contents, which are not otherwise granted special importance. This was formerly known as the Boldface element, and most browsers still draw the text in boldface. However, you should not use <b> for styling text; instead, you should use the CSS font-weight property to create boldface text, or the strong element to indicate that text is of special importance.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' b(attr = list(class = "test"))
b <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'b'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'b'))
	}
}

#'Generate the <base> HTML tag.
#'
#' The <base> HTML element specifies the base URL to use for all relative URLs in a document. There can be only one <base> element in a document.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/base}.
#'
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of attr if length of that item is greater than 1; if FALSE, returns one tag.
#' @param collapse A string. If NULL, returns a vector the same length as the longest item of attr, instead of one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' base(attr = list(class = "test"))
base <- function(attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(attr = attr, separate = separate, collapse = collapse, accepts_content = FALSE, tag = 'base'))
	}else{
		return(empty_tag_helper(attr = attr, separate = separate, collapse = collapse, tag = 'base'))
	}
}

#'Generate the <bdi> HTML tag.
#'
#' The <bdi> HTML element tells the browser's bidirectional algorithm to treat the text it contains in isolation from its surrounding text. It's particularly useful when a website dynamically inserts some text and doesn't know the directionality of the text being inserted.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' bdi(attr = list(class = "test"))
bdi <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'bdi'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'bdi'))
	}
}

#'Generate the <bdo> HTML tag.
#'
#' The <bdo> HTML element overrides the current directionality of text, so that the text within is rendered in a different direction.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' bdo(attr = list(class = "test"))
bdo <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'bdo'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'bdo'))
	}
}

#'Generate the <blockquote> HTML tag.
#'
#' The <blockquote> HTML element indicates that the enclosed text is an extended quotation. Usually, this is rendered visually by indentation (see Notes for how to change it). A URL for the source of the quotation may be given using the cite attribute, while a text representation of the source can be given using the cite element.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' blockquote(attr = list(class = "test"))
blockquote <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'blockquote'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'blockquote'))
	}
}

#'Generate the <body> HTML tag.
#'
#' The <body> HTML element represents the content of an HTML document. There can be only one <body> element in a document.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' body(attr = list(class = "test"))
body <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'body'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'body'))
	}
}

#'Generate the <br> HTML tag.
#'
#' The <br> HTML element produces a line break in text (carriage-return). It is useful for writing a poem or an address, where the division of lines is significant.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br}.
#'
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of attr if length of that item is greater than 1; if FALSE, returns one tag.
#' @param collapse A string. If NULL, returns a vector the same length as the longest item of attr, instead of one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' br(attr = list(class = "test"))
br <- function(attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(attr = attr, separate = separate, collapse = collapse, accepts_content = FALSE, tag = 'br'))
	}else{
		return(empty_tag_helper(attr = attr, separate = separate, collapse = collapse, tag = 'br'))
	}
}

#'Generate the <button> HTML tag.
#'
#' The <button> HTML element is an interactive element activated by a user with a mouse, keyboard, finger, voice command, or other assistive technology. Once activated, it then performs a programmable action, such as submitting a form or opening a dialog.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' button(attr = list(class = "test"))
button <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'button'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'button'))
	}
}

#'Generate the <canvas> HTML tag.
#'
#' Use the HTML <canvas> element with either the canvas scripting API or the WebGL API to draw graphics and animations.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' canvas(attr = list(class = "test"))
canvas <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'canvas'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'canvas'))
	}
}

#'Generate the <caption> HTML tag.
#'
#' The <caption> HTML element specifies the caption (or title) of a table.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' caption(attr = list(class = "test"))
caption <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'caption'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'caption'))
	}
}

#'Generate the <cite> HTML tag.
#'
#' The <cite> HTML element is used to describe a reference to a cited creative work, and must include the title of that work. The reference may be in an abbreviated form according to context-appropriate conventions related to citation metadata.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' cite(attr = list(class = "test"))
cite <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'cite'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'cite'))
	}
}

#'Generate the <code> HTML tag.
#'
#' The <code> HTML element displays its contents styled in a fashion intended to indicate that the text is a short fragment of computer code. By default, the content text is displayed using the user agent default monospace font.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' code(attr = list(class = "test"))
code <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'code'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'code'))
	}
}

#'Generate the <col> HTML tag.
#'
#' The <col> HTML element defines a column within a table and is used for defining common semantics on all common cells. It is generally found within a colgroup element.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col}.
#'
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of attr if length of that item is greater than 1; if FALSE, returns one tag.
#' @param collapse A string. If NULL, returns a vector the same length as the longest item of attr, instead of one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' col(attr = list(class = "test"))
col <- function(attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(attr = attr, separate = separate, collapse = collapse, accepts_content = FALSE, tag = 'col'))
	}else{
		return(empty_tag_helper(attr = attr, separate = separate, collapse = collapse, tag = 'col'))
	}
}

#'Generate the <colgroup> HTML tag.
#'
#' The <colgroup> HTML element defines a group of columns within a table.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' colgroup(attr = list(class = "test"))
colgroup <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'colgroup'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'colgroup'))
	}
}

#'Generate the <content> HTML tag.
#'
#' The <content> HTML element—an obsolete part of the Web Components suite of technologies—was used inside of Shadow DOM as an insertion point, and wasn't meant to be used in ordinary HTML. It has now been replaced by the slot element, which creates a point in the DOM at which a shadow DOM can be inserted.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/content}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' content(attr = list(class = "test"))
content <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'content'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'content'))
	}
}

#'Generate the <data> HTML tag.
#'
#' The <data> HTML element links a given piece of content with a machine-readable translation. If the content is time- or date-related, the time element must be used.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/data}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' data(attr = list(class = "test"))
data <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'data'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'data'))
	}
}

#'Generate the <datalist> HTML tag.
#'
#' The <datalist> HTML element contains a set of option elements that represent the permissible or recommended options available to choose from within other controls.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' datalist(attr = list(class = "test"))
datalist <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'datalist'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'datalist'))
	}
}

#'Generate the <dd> HTML tag.
#'
#' The <dd> HTML element provides the description, definition, or value for the preceding term (dt) in a description list (dl).
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' dd(attr = list(class = "test"))
dd <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'dd'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'dd'))
	}
}

#'Generate the <del> HTML tag.
#'
#' The <del> HTML element represents a range of text that has been deleted from a document. This can be used when rendering "track changes" or source code diff information, for example. The ins element can be used for the opposite purpose: to indicate text that has been added to the document.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' del(attr = list(class = "test"))
del <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'del'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'del'))
	}
}

#'Generate the <details> HTML tag.
#'
#' The <details> HTML element creates a disclosure widget in which information is visible only when the widget is toggled into an "open" state. A summary or label must be provided using the summary element.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' details(attr = list(class = "test"))
details <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'details'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'details'))
	}
}

#'Generate the <dfn> HTML tag.
#'
#' The <dfn> HTML element is used to indicate the term being defined within the context of a definition phrase or sentence. The p element, the dt/dd pairing, or the section element which is the nearest ancestor of the <dfn> is considered to be the definition of the term.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' dfn(attr = list(class = "test"))
dfn <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'dfn'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'dfn'))
	}
}

#'Generate the <dialog> HTML tag.
#'
#' The <dialog> HTML element represents a dialog box or other interactive component, such as a dismissible alert, inspector, or subwindow.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dialog}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' dialog(attr = list(class = "test"))
dialog <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'dialog'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'dialog'))
	}
}

#'Generate the <div> HTML tag.
#'
#' The <div> HTML element is the generic container for flow content. It has no effect on the content or layout until styled in some way using CSS (e.g. styling is directly applied to it, or some kind of layout model like Flexbox is applied to its parent element).
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' div(attr = list(class = "test"))
div <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'div'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'div'))
	}
}

#'Generate the <dl> HTML tag.
#'
#' The <dl> HTML element represents a description list. The element encloses a list of groups of terms (specified using the dt element) and descriptions (provided by dd elements). Common uses for this element are to implement a glossary or to display metadata (a list of key-value pairs).
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' dl(attr = list(class = "test"))
dl <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'dl'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'dl'))
	}
}

#'Generate the <dt> HTML tag.
#'
#' The <dt> HTML element specifies a term in a description or definition list, and as such must be used inside a dl element. It is usually followed by a dd element; however, multiple <dt> elements in a row indicate several terms that are all defined by the immediate next dd element.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' dt(attr = list(class = "test"))
dt <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'dt'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'dt'))
	}
}

#'Generate the <em> HTML tag.
#'
#' The <em> HTML element marks text that has stress emphasis. The <em> element can be nested, with each level of nesting indicating a greater degree of emphasis.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' em(attr = list(class = "test"))
em <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'em'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'em'))
	}
}

#'Generate the <embed> HTML tag.
#'
#' The <embed> HTML element embeds external content at the specified point in the document. This content is provided by an external application or other source of interactive content such as a browser plug-in.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed}.
#'
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of attr if length of that item is greater than 1; if FALSE, returns one tag.
#' @param collapse A string. If NULL, returns a vector the same length as the longest item of attr, instead of one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' embed(attr = list(class = "test"))
embed <- function(attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(attr = attr, separate = separate, collapse = collapse, accepts_content = FALSE, tag = 'embed'))
	}else{
		return(empty_tag_helper(attr = attr, separate = separate, collapse = collapse, tag = 'embed'))
	}
}

#'Generate the <fieldset> HTML tag.
#'
#' The <fieldset> HTML element is used to group several controls as well as labels (label) within a web form.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' fieldset(attr = list(class = "test"))
fieldset <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'fieldset'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'fieldset'))
	}
}

#'Generate the <figcaption> HTML tag.
#'
#' The <figcaption> HTML element represents a caption or legend describing the rest of the contents of its parent figure element.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' figcaption(attr = list(class = "test"))
figcaption <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'figcaption'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'figcaption'))
	}
}

#'Generate the <figure> HTML tag.
#'
#' The <figure> HTML element represents self-contained content, potentially with an optional caption, which is specified using the figcaption element. The figure, its caption, and its contents are referenced as a single unit.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' figure(attr = list(class = "test"))
figure <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'figure'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'figure'))
	}
}

#'Generate the <footer> HTML tag.
#'
#' The <footer> HTML element represents a footer for its nearest ancestor sectioning content or sectioning root element. A <footer> typically contains information about the author of the section, copyright data or links to related documents.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' footer(attr = list(class = "test"))
footer <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'footer'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'footer'))
	}
}

#'Generate the <form> HTML tag.
#'
#' The <form> HTML element represents a document section containing interactive controls for submitting information.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' form(attr = list(class = "test"))
form <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'form'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'form'))
	}
}

#'Generate the <h1> HTML tag.
#'
#' The <h1> to <h6> HTML elements represent six levels of section headings. <h1> is the highest section level and <h6> is the lowest.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/Heading_Elements}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' h1(attr = list(class = "test"))
h1 <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'h1'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'h1'))
	}
}

#'Generate the <h2> HTML tag.
#'
#' The <h1> to <h6> HTML elements represent six levels of section headings. <h1> is the highest section level and <h6> is the lowest.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/Heading_Elements}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' h2(attr = list(class = "test"))
h2 <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'h2'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'h2'))
	}
}

#'Generate the <h3> HTML tag.
#'
#' The <h1> to <h6> HTML elements represent six levels of section headings. <h1> is the highest section level and <h6> is the lowest.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/Heading_Elements}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' h3(attr = list(class = "test"))
h3 <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'h3'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'h3'))
	}
}

#'Generate the <h4> HTML tag.
#'
#' The <h1> to <h6> HTML elements represent six levels of section headings. <h1> is the highest section level and <h6> is the lowest.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/Heading_Elements}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' h4(attr = list(class = "test"))
h4 <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'h4'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'h4'))
	}
}

#'Generate the <h5> HTML tag.
#'
#' The <h1> to <h6> HTML elements represent six levels of section headings. <h1> is the highest section level and <h6> is the lowest.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/Heading_Elements}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' h5(attr = list(class = "test"))
h5 <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'h5'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'h5'))
	}
}

#'Generate the <h6> HTML tag.
#'
#' The <h1> to <h6> HTML elements represent six levels of section headings. <h1> is the highest section level and <h6> is the lowest.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/Heading_Elements}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' h6(attr = list(class = "test"))
h6 <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'h6'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'h6'))
	}
}

#'Generate the <head> HTML tag.
#'
#' The <head> HTML element contains machine-readable information (metadata) about the document, like its title, scripts, and style sheets.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/head}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' head(attr = list(class = "test"))
head <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'head'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'head'))
	}
}

#'Generate the <header> HTML tag.
#'
#' The <header> HTML element represents introductory content, typically a group of introductory or navigational aids. It may contain some heading elements but also a logo, a search form, an author name, and other elements.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' header(attr = list(class = "test"))
header <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'header'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'header'))
	}
}

#'Generate the <hr> HTML tag.
#'
#' The <hr> HTML element represents a thematic break between paragraph-level elements: for example, a change of scene in a story, or a shift of topic within a section.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr}.
#'
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of attr if length of that item is greater than 1; if FALSE, returns one tag.
#' @param collapse A string. If NULL, returns a vector the same length as the longest item of attr, instead of one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' hr(attr = list(class = "test"))
hr <- function(attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(attr = attr, separate = separate, collapse = collapse, accepts_content = FALSE, tag = 'hr'))
	}else{
		return(empty_tag_helper(attr = attr, separate = separate, collapse = collapse, tag = 'hr'))
	}
}

#'Generate the <html> HTML tag.
#'
#' The <html> HTML element represents the root (top-level element) of an HTML document, so it is also referred to as the root element. All other elements must be descendants of this element.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/html}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' html(attr = list(class = "test"))
html <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'html'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'html'))
	}
}

#'Generate the <i> HTML tag.
#'
#' The <i> HTML element represents a range of text that is set off from the normal text for some reason, such as idiomatic text, technical terms, taxonomical designations, among others. Historically, these have been presented using italicized type, which is the original source of the <i> naming of this element.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' i(attr = list(class = "test"))
i <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'i'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'i'))
	}
}

#'Generate the <iframe> HTML tag.
#'
#' The <iframe> HTML element represents a nested browsing context, embedding another HTML page into the current one.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' iframe(attr = list(class = "test"))
iframe <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'iframe'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'iframe'))
	}
}

#'Generate the <img> HTML tag.
#'
#' The <img> HTML element embeds an image into the document.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img}.
#'
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of attr if length of that item is greater than 1; if FALSE, returns one tag.
#' @param collapse A string. If NULL, returns a vector the same length as the longest item of attr, instead of one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' img(attr = list(class = "test"))
img <- function(attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(attr = attr, separate = separate, collapse = collapse, accepts_content = FALSE, tag = 'img'))
	}else{
		return(empty_tag_helper(attr = attr, separate = separate, collapse = collapse, tag = 'img'))
	}
}

#'Generate the <input> HTML tag.
#'
#' The <input> HTML element is used to create interactive controls for web-based forms in order to accept data from the user; a wide variety of types of input data and control widgets are available, depending on the device and user agent. The <input> element is one of the most powerful and complex in all of HTML due to the sheer number of combinations of input types and attributes.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input}.
#'
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of attr if length of that item is greater than 1; if FALSE, returns one tag.
#' @param collapse A string. If NULL, returns a vector the same length as the longest item of attr, instead of one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' input(attr = list(class = "test"))
input <- function(attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(attr = attr, separate = separate, collapse = collapse, accepts_content = FALSE, tag = 'input'))
	}else{
		return(empty_tag_helper(attr = attr, separate = separate, collapse = collapse, tag = 'input'))
	}
}

#'Generate the <ins> HTML tag.
#'
#' The <ins> HTML element represents a range of text that has been added to a document. You can use the del element to similarly represent a range of text that has been deleted from the document.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' ins(attr = list(class = "test"))
ins <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'ins'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'ins'))
	}
}

#'Generate the <kbd> HTML tag.
#'
#' The <kbd> HTML element represents a span of inline text denoting textual user input from a keyboard, voice input, or any other text entry device. By convention, the user agent defaults to rendering the contents of a <kbd> element using its default monospace font, although this is not mandated by the HTML standard.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' kbd(attr = list(class = "test"))
kbd <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'kbd'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'kbd'))
	}
}

#'Generate the <keygen> HTML tag.
#'
#' The <keygen> HTML element exists to facilitate generation of key material, and submission of the public key as part of an HTML form. This mechanism is designed for use with Web-based certificate management systems. It is expected that the <keygen> element will be used in an HTML form along with other information needed to construct a certificate request, and that the result of the process will be a signed certificate.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen}.
#'
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of attr if length of that item is greater than 1; if FALSE, returns one tag.
#' @param collapse A string. If NULL, returns a vector the same length as the longest item of attr, instead of one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' keygen(attr = list(class = "test"))
keygen <- function(attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(attr = attr, separate = separate, collapse = collapse, accepts_content = FALSE, tag = 'keygen'))
	}else{
		return(empty_tag_helper(attr = attr, separate = separate, collapse = collapse, tag = 'keygen'))
	}
}

#'Generate the <label> HTML tag.
#'
#' The <label> HTML element represents a caption for an item in a user interface.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' label(attr = list(class = "test"))
label <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'label'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'label'))
	}
}

#'Generate the <legend> HTML tag.
#'
#' The <legend> HTML element represents a caption for the content of its parent fieldset.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' legend(attr = list(class = "test"))
legend <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'legend'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'legend'))
	}
}

#'Generate the <li> HTML tag.
#'
#' The <li> HTML element is used to represent an item in a list. It must be contained in a parent element: an ordered list (ol), an unordered list (ul), or a menu (menu). In menus and unordered lists, list items are usually displayed using bullet points. In ordered lists, they are usually displayed with an ascending counter on the left, such as a number or letter.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' li(attr = list(class = "test"))
li <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'li'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'li'))
	}
}

#'Generate the <link> HTML tag.
#'
#' The <link> HTML element specifies relationships between the current document and an external resource. This element is most commonly used to link to CSS, but is also used to establish site icons (both "favicon" style icons and icons for the home screen and apps on mobile devices) among other things.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link}.
#'
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of attr if length of that item is greater than 1; if FALSE, returns one tag.
#' @param collapse A string. If NULL, returns a vector the same length as the longest item of attr, instead of one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' link(attr = list(class = "test"))
link <- function(attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(attr = attr, separate = separate, collapse = collapse, accepts_content = FALSE, tag = 'link'))
	}else{
		return(empty_tag_helper(attr = attr, separate = separate, collapse = collapse, tag = 'link'))
	}
}

#'Generate the <main> HTML tag.
#'
#' The <main> HTML element represents the dominant content of the body of a document. The main content area consists of content that is directly related to or expands upon the central topic of a document, or the central functionality of an application.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/main}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' main(attr = list(class = "test"))
main <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'main'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'main'))
	}
}

#'Generate the <map> HTML tag.
#'
#' The <map> HTML element is used with area elements to define an image map (a clickable link area).
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/map}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' map(attr = list(class = "test"))
map <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'map'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'map'))
	}
}

#'Generate the <mark> HTML tag.
#'
#' The <mark> HTML element represents text which is marked or highlighted for reference or notation purposes, due to the marked passage's relevance or importance in the enclosing context.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' mark(attr = list(class = "test"))
mark <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'mark'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'mark'))
	}
}

#'Generate the <menu> HTML tag.
#'
#' The <menu> HTML element is described in the HTML specification as a semantic alternative to ul, but treated by browsers (and exposed through the accessibility tree) as no different than ul. It represents an unordered list of items (which are represented by li elements).
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' menu(attr = list(class = "test"))
menu <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'menu'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'menu'))
	}
}

#'Generate the <menuitem> HTML tag.
#'
#' The <menuitem> HTML element represents a command that a user is able to invoke through a popup menu. This includes context menus, as well as menus that might be attached to a menu button.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menuitem}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' menuitem(attr = list(class = "test"))
menuitem <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'menuitem'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'menuitem'))
	}
}

#'Generate the <meta> HTML tag.
#'
#' The <meta> HTML element represents Metadata that cannot be represented by other HTML meta-related elements, like base, link, script, style or title.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meta}.
#'
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of attr if length of that item is greater than 1; if FALSE, returns one tag.
#' @param collapse A string. If NULL, returns a vector the same length as the longest item of attr, instead of one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' meta(attr = list(class = "test"))
meta <- function(attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(attr = attr, separate = separate, collapse = collapse, accepts_content = FALSE, tag = 'meta'))
	}else{
		return(empty_tag_helper(attr = attr, separate = separate, collapse = collapse, tag = 'meta'))
	}
}

#'Generate the <meter> HTML tag.
#'
#' The <meter> HTML element represents either a scalar value within a known range or a fractional value.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' meter(attr = list(class = "test"))
meter <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'meter'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'meter'))
	}
}

#'Generate the <nav> HTML tag.
#'
#' The <nav> HTML element represents a section of a page whose purpose is to provide navigation links, either within the current document or to other documents. Common examples of navigation sections are menus, tables of contents, and indexes.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' nav(attr = list(class = "test"))
nav <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'nav'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'nav'))
	}
}

#'Generate the <noscript> HTML tag.
#'
#' The <noscript> HTML element defines a section of HTML to be inserted if a script type on the page is unsupported or if scripting is currently turned off in the browser.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/noscript}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' noscript(attr = list(class = "test"))
noscript <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'noscript'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'noscript'))
	}
}

#'Generate the <object> HTML tag.
#'
#' The <object> HTML element represents an external resource, which can be treated as an image, a nested browsing context, or a resource to be handled by a plugin.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' object(attr = list(class = "test"))
object <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'object'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'object'))
	}
}

#'Generate the <ol> HTML tag.
#'
#' The <ol> HTML element represents an ordered list of items — typically rendered as a numbered list.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' ol(attr = list(class = "test"))
ol <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'ol'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'ol'))
	}
}

#'Generate the <optgroup> HTML tag.
#'
#' The <optgroup> HTML element creates a grouping of options within a select element.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' optgroup(attr = list(class = "test"))
optgroup <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'optgroup'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'optgroup'))
	}
}

#'Generate the <option> HTML tag.
#'
#' The <option> HTML element is used to define an item contained in a select, an optgroup, or a datalist element. As such, <option> can represent menu items in popups and other lists of items in an HTML document.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' option(attr = list(class = "test"))
option <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'option'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'option'))
	}
}

#'Generate the <output> HTML tag.
#'
#' The <output> HTML element is a container element into which a site or app can inject the results of a calculation or the outcome of a user action.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' output(attr = list(class = "test"))
output <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'output'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'output'))
	}
}

#'Generate the <p> HTML tag.
#'
#' The <p> HTML element represents a paragraph. Paragraphs are usually represented in visual media as blocks of text separated from adjacent blocks by blank lines and/or first-line indentation, but HTML paragraphs can be any structural grouping of related content, such as images or form fields.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' p(attr = list(class = "test"))
p <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'p'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'p'))
	}
}

#'Generate the <param> HTML tag.
#'
#' The <param> HTML element defines parameters for an object element.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param}.
#'
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of attr if length of that item is greater than 1; if FALSE, returns one tag.
#' @param collapse A string. If NULL, returns a vector the same length as the longest item of attr, instead of one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' param(attr = list(class = "test"))
param <- function(attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(attr = attr, separate = separate, collapse = collapse, accepts_content = FALSE, tag = 'param'))
	}else{
		return(empty_tag_helper(attr = attr, separate = separate, collapse = collapse, tag = 'param'))
	}
}

#'Generate the <picture> HTML tag.
#'
#' The <picture> HTML element contains zero or more source elements and one img element to offer alternative versions of an image for different display/device scenarios.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/picture}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' picture(attr = list(class = "test"))
picture <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'picture'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'picture'))
	}
}

#'Generate the <pre> HTML tag.
#'
#' The <pre> HTML element represents preformatted text which is to be presented exactly as written in the HTML file. The text is typically rendered using a non-proportional, or monospaced, font. Whitespace inside this element is displayed as written.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' pre(attr = list(class = "test"))
pre <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'pre'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'pre'))
	}
}

#'Generate the <progress> HTML tag.
#'
#' The <progress> HTML element displays an indicator showing the completion progress of a task, typically displayed as a progress bar.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' progress(attr = list(class = "test"))
progress <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'progress'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'progress'))
	}
}

#'Generate the <q> HTML tag.
#'
#' The <q> HTML element indicates that the enclosed text is a short inline quotation. Most modern browsers implement this by surrounding the text in quotation marks. This element is intended for short quotations that don't require paragraph breaks; for long quotations use the blockquote element.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' q(attr = list(class = "test"))
q <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'q'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'q'))
	}
}

#'Generate the <rb> HTML tag.
#'
#' The <rb> HTML element is used to delimit the base text component of a ruby annotation, i.e. the text that is being annotated. One <rb> element should wrap each separate atomic segment of the base text.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rb}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' rb(attr = list(class = "test"))
rb <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'rb'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'rb'))
	}
}

#'Generate the <rp> HTML tag.
#'
#' The <rp> HTML element is used to provide fall-back parentheses for browsers that do not support display of ruby annotations using the ruby element. One <rp> element should enclose each of the opening and closing parentheses that wrap the rt element that contains the annotation's text.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' rp(attr = list(class = "test"))
rp <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'rp'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'rp'))
	}
}

#'Generate the <rt> HTML tag.
#'
#' The <rt> HTML element specifies the ruby text component of a ruby annotation, which is used to provide pronunciation, translation, or transliteration information for East Asian typography. The <rt> element must always be contained within a ruby element.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' rt(attr = list(class = "test"))
rt <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'rt'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'rt'))
	}
}

#'Generate the <rtc> HTML tag.
#'
#' The <rtc> HTML element embraces semantic annotations of characters presented in a ruby of rb elements used inside of ruby element. rb elements can have both pronunciation (rt) and semantic (rtc) annotations.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rtc}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' rtc(attr = list(class = "test"))
rtc <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'rtc'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'rtc'))
	}
}

#'Generate the <ruby> HTML tag.
#'
#' The <ruby> HTML element represents small annotations that are rendered above, below, or next to base text, usually used for showing the pronunciation of East Asian characters. It can also be used for annotating other kinds of text, but this usage is less common.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' ruby(attr = list(class = "test"))
ruby <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'ruby'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'ruby'))
	}
}

#'Generate the <s> HTML tag.
#'
#' The <s> HTML element renders text with a strikethrough, or a line through it. Use the <s> element to represent things that are no longer relevant or no longer accurate. However, <s> is not appropriate when indicating document edits; for that, use the del and ins elements, as appropriate.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' s(attr = list(class = "test"))
s <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 's'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 's'))
	}
}

#'Generate the <samp> HTML tag.
#'
#' The <samp> HTML element is used to enclose inline text which represents sample (or quoted) output from a computer program. Its contents are typically rendered using the browser's default monospaced font (such as Courier or Lucida Console).
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' samp(attr = list(class = "test"))
samp <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'samp'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'samp'))
	}
}

#'Generate the <script> HTML tag.
#'
#' The <script> HTML element is used to embed executable code or data; this is typically used to embed or refer to JavaScript code. The <script> element can also be used with other languages, such as WebGL's GLSL shader programming language and JSON.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' script(attr = list(class = "test"))
script <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'script'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'script'))
	}
}

#'Generate the <section> HTML tag.
#'
#' The <section> HTML element represents a generic standalone section of a document, which doesn't have a more specific semantic element to represent it. Sections should always have a heading, with very few exceptions.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' section(attr = list(class = "test"))
section <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'section'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'section'))
	}
}

#'Generate the <select> HTML tag.
#'
#' The <select> HTML element represents a control that provides a menu of options.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' select(attr = list(class = "test"))
select <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'select'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'select'))
	}
}

#'Generate the <shadow> HTML tag.
#'
#' The <shadow> HTML element—an obsolete part of the Web Components technology suite—was intended to be used as a shadow DOM insertion point. You might have used it if you have created multiple shadow roots under a shadow host. It is not useful in ordinary HTML.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/shadow}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' shadow(attr = list(class = "test"))
shadow <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'shadow'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'shadow'))
	}
}

#'Generate the <slot> HTML tag.
#'
#' The <slot> HTML element—part of the Web Components technology suite—is a placeholder inside a web component that you can fill with your own markup, which lets you create separate DOM trees and present them together.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/slot}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' slot(attr = list(class = "test"))
slot <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'slot'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'slot'))
	}
}

#'Generate the <small> HTML tag.
#'
#' The <small> HTML element represents side-comments and small print, like copyright and legal text, independent of its styled presentation. By default, it renders text within it one font-size smaller, such as from small to x-small.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' small(attr = list(class = "test"))
small <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'small'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'small'))
	}
}

#'Generate the <source> HTML tag.
#'
#' The <source> HTML element specifies multiple media resources for the picture, the audio element, or the video element. It is a void element, meaning that it has no content and does not have a closing tag. It is commonly used to offer the same media content in multiple file formats in order to provide compatibility with a broad range of browsers given their differing support for image file formats and media file formats.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source}.
#'
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of attr if length of that item is greater than 1; if FALSE, returns one tag.
#' @param collapse A string. If NULL, returns a vector the same length as the longest item of attr, instead of one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' source(attr = list(class = "test"))
source <- function(attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(attr = attr, separate = separate, collapse = collapse, accepts_content = FALSE, tag = 'source'))
	}else{
		return(empty_tag_helper(attr = attr, separate = separate, collapse = collapse, tag = 'source'))
	}
}

#'Generate the <span> HTML tag.
#'
#' The <span> HTML element is a generic inline container for phrasing content, which does not inherently represent anything. It can be used to group elements for styling purposes (using the class or id attributes), or because they share attribute values, such as lang. It should be used only when no other semantic element is appropriate. <span> is very much like a div element, but div is a block-level element whereas a <span> is an inline element.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' span(attr = list(class = "test"))
span <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'span'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'span'))
	}
}

#'Generate the <strong> HTML tag.
#'
#' The <strong> HTML element indicates that its contents have strong importance, seriousness, or urgency. Browsers typically render the contents in bold type.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' strong(attr = list(class = "test"))
strong <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'strong'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'strong'))
	}
}

#'Generate the <style> HTML tag.
#'
#' The <style> HTML element contains style information for a document, or part of a document. It contains CSS, which is applied to the contents of the document containing the <style> element.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/style}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' style(attr = list(class = "test"))
style <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'style'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'style'))
	}
}

#'Generate the <sub> HTML tag.
#'
#' The <sub> HTML element specifies inline text which should be displayed as subscript for solely typographical reasons. Subscripts are typically rendered with a lowered baseline using smaller text.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' sub(attr = list(class = "test"))
sub <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'sub'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'sub'))
	}
}

#'Generate the <summary> HTML tag.
#'
#' The <summary> HTML element specifies a summary, caption, or legend for a details element's disclosure box. Clicking the <summary> element toggles the state of the parent <details> element open and closed.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' summary(attr = list(class = "test"))
summary <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'summary'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'summary'))
	}
}

#'Generate the <sup> HTML tag.
#'
#' The <sup> HTML element specifies inline text which is to be displayed as superscript for solely typographical reasons. Superscripts are usually rendered with a raised baseline using smaller text.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' sup(attr = list(class = "test"))
sup <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'sup'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'sup'))
	}
}

#'Generate the <table> HTML tag.
#'
#' The <table> HTML element represents tabular data — that is, information presented in a two-dimensional table comprised of rows and columns of cells containing data.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' table(attr = list(class = "test"))
table <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'table'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'table'))
	}
}

#'Generate the <tbody> HTML tag.
#'
#' The <tbody> HTML element encapsulates a set of table rows (tr elements), indicating that they comprise the body of the table (table).
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' tbody(attr = list(class = "test"))
tbody <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'tbody'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'tbody'))
	}
}

#'Generate the <td> HTML tag.
#'
#' The <td> HTML element defines a cell of a table that contains data. It participates in the table model.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' td(attr = list(class = "test"))
td <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'td'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'td'))
	}
}

#'Generate the <template> HTML tag.
#'
#' The <template> HTML element is a mechanism for holding HTML that is not to be rendered immediately when a page is loaded but may be instantiated subsequently during runtime using JavaScript.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/template}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' template(attr = list(class = "test"))
template <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'template'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'template'))
	}
}

#'Generate the <textarea> HTML tag.
#'
#' The <textarea> HTML element represents a multi-line plain-text editing control, useful when you want to allow users to enter a sizeable amount of free-form text, for example a comment on a review or feedback form.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' textarea(attr = list(class = "test"))
textarea <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'textarea'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'textarea'))
	}
}

#'Generate the <tfoot> HTML tag.
#'
#' The <tfoot> HTML element defines a set of rows summarizing the columns of the table.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' tfoot(attr = list(class = "test"))
tfoot <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'tfoot'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'tfoot'))
	}
}

#'Generate the <th> HTML tag.
#'
#' The <th> HTML element defines a cell as header of a group of table cells. The exact nature of this group is defined by the scope and headers attributes.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' th(attr = list(class = "test"))
th <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'th'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'th'))
	}
}

#'Generate the <thead> HTML tag.
#'
#' The <thead> HTML element defines a set of rows defining the head of the columns of the table.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' thead(attr = list(class = "test"))
thead <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'thead'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'thead'))
	}
}

#'Generate the <time> HTML tag.
#'
#' The <time> HTML element represents a specific period in time. It may include the datetime attribute to translate dates into machine-readable format, allowing for better search engine results or custom features such as reminders.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' time(attr = list(class = "test"))
time <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'time'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'time'))
	}
}

#'Generate the <title> HTML tag.
#'
#' The <title> HTML element defines the document's title that is shown in a Browser's title bar or a page's tab. It only contains text; tags within the element are ignored.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/title}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' title(attr = list(class = "test"))
title <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'title'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'title'))
	}
}

#'Generate the <tr> HTML tag.
#'
#' The <tr> HTML element defines a row of cells in a table. The row's cells can then be established using a mix of td (data cell) and th (header cell) elements.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' tr(attr = list(class = "test"))
tr <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'tr'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'tr'))
	}
}

#'Generate the <track> HTML tag.
#'
#' The <track> HTML element is used as a child of the media elements, audio and video. It lets you specify timed text tracks (or time-based data), for example to automatically handle subtitles. The tracks are formatted in WebVTT format (.vtt files) — Web Video Text Tracks.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track}.
#'
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of attr if length of that item is greater than 1; if FALSE, returns one tag.
#' @param collapse A string. If NULL, returns a vector the same length as the longest item of attr, instead of one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' track(attr = list(class = "test"))
track <- function(attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(attr = attr, separate = separate, collapse = collapse, accepts_content = FALSE, tag = 'track'))
	}else{
		return(empty_tag_helper(attr = attr, separate = separate, collapse = collapse, tag = 'track'))
	}
}

#'Generate the <tt> HTML tag.
#'
#' The <tt> HTML element creates inline text which is presented using the user agent default monospace font face. This element was created for the purpose of rendering text as it would be displayed on a fixed-width display such as a teletype, text-only screen, or line printer.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tt}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' tt(attr = list(class = "test"))
tt <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'tt'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'tt'))
	}
}

#'Generate the <u> HTML tag.
#'
#' The <u> HTML element represents a span of inline text which should be rendered in a way that indicates that it has a non-textual annotation. This is rendered by default as a simple solid underline, but may be altered using CSS.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' u(attr = list(class = "test"))
u <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'u'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'u'))
	}
}

#'Generate the <ul> HTML tag.
#'
#' The <ul> HTML element represents an unordered list of items, typically rendered as a bulleted list.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' ul(attr = list(class = "test"))
ul <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'ul'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'ul'))
	}
}

#'Generate the <var> HTML tag.
#'
#' The <var> HTML element represents the name of a variable in a mathematical expression or a programming context. It's typically presented using an italicized version of the current typeface, although that behavior is browser-dependent.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' var(attr = list(class = "test"))
var <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'var'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'var'))
	}
}

#'Generate the <video> HTML tag.
#'
#' The <video> HTML element embeds a media player which supports video playback into the document. You can use <video> for audio content as well, but the audio element may provide a more appropriate user experience.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video}.
#'
#' @param ... A string or vector of content to pass to the tag.
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of ...; if FALSE, returns one tag with the items of ... in the tag content.
#' @param collapse A string. If NULL, returns a vector the same length as ... instead of collapsing the tags into one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' video(attr = list(class = "test"))
video <- function(..., attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(..., attr = attr, separate = separate, collapse = collapse, accepts_content = TRUE, tag = 'video'))
	}else{
		return(tag_helper(..., attr = attr, separate = separate, collapse = collapse, tag = 'video'))
	}
}

#'Generate the <wbr> HTML tag.
#'
#' The <wbr> HTML element represents a word break opportunity—a position within text where the browser may optionally break a line, though its line-breaking rules would not otherwise create a break at that location.
#'
#' Learn more at \url{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr}.
#'
#' @param attr A named list or named vector, names are attribute names and values are attribute values.
#' @param separate TRUE/FALSE, If TRUE, returns separate tags for each item of attr if length of that item is greater than 1; if FALSE, returns one tag.
#' @param collapse A string. If NULL, returns a vector the same length as the longest item of attr, instead of one string.
#' @param formatted TRUE/FALSE, if TRUE, HTML will be generated with indents and new lines for readability at the cost of performance. Controlled by setting the environment variable html5_vars$formatted <- TRUE/FALSE
#' @return A HTML tag string.
#' @examples
#' wbr(attr = list(class = "test"))
wbr <- function(attr = NULL, separate = FALSE, collapse = '', formatted = html5_vars$formatted){
	if(formatted){
		return(formatted_tag_helper(attr = attr, separate = separate, collapse = collapse, accepts_content = FALSE, tag = 'wbr'))
	}else{
		return(empty_tag_helper(attr = attr, separate = separate, collapse = collapse, tag = 'wbr'))
	}
}

