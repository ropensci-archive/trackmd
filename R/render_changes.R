#' Render changes as HTML
#'
#' Makes and HTML rendering of the track changes markup in markdown.
#'
#' @param text The markdown as a character vector with a single value (i.e. not
#'   one value per line). Either `text` or `file` must be used, but not both.
#' @param file The file containing the markdown to be rendered. Either `text` or
#'   `file` must be used, but not both.
#' @param ... Passed to \code{\link[rmarkdown]{render}}
#'   
#' @keywords internal
render_changes <- function(text = NULL, file = NULL, ...) {
  
  # Check user input
  if (sum(c(is.null(text), is.null(file))) != 1) {
    stop("Either `text` or `file` must be used, but not both.")
  }
  if (! is.null(text) && length(text) != 1) {
    stop("Input to `text` must be of length 1")
  }
  
  # Read file if needed
  if (is.null(text)) {
    text = readr::read_file(file)
  }
  
  # Replace additions markup
  text <- gsub(text, pattern = "\\{\\+\\+(.*?)\\+\\+\\}",
               replacement = render_html_add("\\1"))
  text <- gsub(text, pattern = "\\{--(.*?)--\\}",
               replacement = render_html_delete("\\1"))
  text <- gsub(text, pattern = "\\{~~(.*?)~>(.*?)~~\\}",
               replacement = render_html_substitution("\\1", "\\2"))
  text <- gsub(text, pattern = "\\{==(.*?)==\\}\\{>>(.*?)<<\\}",
               replacement = render_html_highlight("\\1", "\\2"))
  text <- gsub(text, pattern = "\\{>>(.*?)<<\\}",
               replacement = render_html_comment("\\1"))
  
  # Convert to html
  temp_input <- tempfile()
  on.exit(file.remove(temp_input))
  readr::write_file(text, path = temp_input)
  rmarkdown::render(temp_input, rmarkdown::html_fragment(), ...)
}


#' Format text as an addition
#' 
#' Format text as an addition using HTML
#' 
#' @param text The text to format
#' 
#' @keywords internal
render_html_add <- function(text) {
  paste0('<font color="green"><ins>', text, '</ins></font>')
}


#' Format text as a deletion
#' 
#' Format text as a deletion using HTML
#' 
#' @param text The text to format
#' 
#' @keywords internal
render_html_delete <- function(text) {
  paste0('<font color="red"><del>', text, '</del></font>')
}


#' Format text as a substitution
#' 
#' Format text as a substitution using HTML
#' 
#' @param from What the text was before the change
#' @param to What the text is after the change
#' 
#' @keywords internal
render_html_substitution <- function(from, to) {
  paste0(render_html_delete(from), render_html_add(to))
}


#' Format text as a substitution
#' 
#' Format text as a substitution using HTML
#' 
#' @param from What the text was before the change
#' @param to What the text is after the change
#' 
#' @keywords internal
render_html_highlight <- function(text, comment) {
  paste0('<div class="comment"><mark>', text, '</mark><span class="commenttext">', comment, '</span></div>')
}


#' Format text as a comment
#' 
#' Format text as a comment using HTML
#' 
#' @param from What the text was before the change
#' @param to What the text is after the change
#' 
#' @keywords internal
render_html_comment <- function(comment) {
  paste0('<div class="comment"><mark>&#x1f4ac;</mark><span class="commenttext">', comment, '</span></div>')
}
