#' Create track changes diff
#'
#' Compares two strings and creates a diff using the markdown track changes syntax.
#'
#' @param before (\code{character} of length 1) The text before the changes.
#' @param after (\code{character} of length 1) The text after the changes.
#'
#' @importFrom diffobj ses
#' @keywords internal
diff_to_markup <- function(before, after) {
#  browser()
  # Convert string to characters
  before <- unlist(strsplit(before, split = ""))
  after <- unlist(strsplit(after, split = ""))

  # Calculate diff
  difference <- diffobj::ses(before, after)

  apply_addition <- function(text, index, afterStart, afterEnd) {
    io1 <- index+offset + 1
    res <- c(text[1:(index + offset)], "{", "+", "+", after[afterStart:afterEnd], "+", "+", "}",
                   text[io1:length(text)])
    if (io1 > length(text)){
      res <- c(text[1:(index + offset)],
        "{", "+", "+", after[afterStart:afterEnd], "+", "+", "}")
    }
    return(res)
  }

  apply_deletion <- function(text, beforeStart, beforeEnd) {
    bo1 <- beforeEnd + offset + 1
    res <- c(text[1:(beforeStart - 1 + offset)],
             "{", "-", "-", before[beforeStart:beforeEnd], "-", "-", "}",
             text[bo1:length(text)])
    if (bo1 > length(text)){
      res <- c(text[1:(beforeStart - 1 + offset)],
        "{", "-", "-", before[beforeStart:beforeEnd], "-", "-", "}")
    }
    return(res)
  }

  apply_change <- function(text, beforeStart, beforeEnd, afterStart, afterEnd) {
    bo1 <- beforeEnd + offset + 1
    res <- c(text[1:(beforeStart - 1 + offset)],
             "{", "~", "~", before[beforeStart:beforeEnd], "~", ">", after[afterStart:afterEnd], "~", "~", "}",
             text[bo1:length(text)])
    if (bo1 > length(text)){
      res <- c(text[1:(beforeStart - 1 + offset)],
        "{", "~", "~", before[beforeStart:beforeEnd], "~", ">", after[afterStart:afterEnd], "~", "~", "}")
    }
    return(res)
  }

  text <- before
  offset <- 0
  for (aDiff in difference) {
    length_before <- length(text)
    sesValues <- getSesValues(aDiff)
    if (sesValues$type == "a") {
      #browser()
      text <- apply_addition(text,
                             sesValues$beforeStart,
                             sesValues$afterStart,
                             sesValues$afterEnd)
    } else if (sesValues$type == "d") {
      text <- apply_deletion(text,
                             sesValues$beforeStart,
                             sesValues$beforeEnd)
    } else if (sesValues$type == "c") {
      text <- apply_change(text,
                           sesValues$beforeStart,
                           sesValues$beforeEnd,
                           sesValues$afterStart,
                           sesValues$afterEnd)
    }
    offset <- offset  + length(text) - length_before
  }

  # Convert bact to single string
  return(paste0(text, collapse = ""))
}


#' Parse SES strings
#'
#' Parse a SES string returned by \code{\link[diffobj]{ses}} into a list of values.
#'
#' @param sesText  (\code{character} of length 1) A SES string
#'
#' @keywords internal
getSesValues <- function(sesText) {
  # Extract relevant values from string
  matches <- as.list(stringr::str_match(sesText, "([0-9]+),?([0-9]*)([a-z]+)([0-9]+),?([0-9]*)")[, -1])
  names(matches) <- c("beforeStart", "beforeEnd", "type", "afterStart", "afterEnd")

  # Fill in implied values
  if (matches$beforeEnd == "") {
    matches$beforeEnd <- matches$beforeStart
  }
  if (matches$afterEnd == "") {
    matches$afterEnd <- matches$afterStart
  }

  # Convert indexes to numeric
  matches$beforeEnd <- as.numeric(matches$beforeEnd)
  matches$afterEnd <- as.numeric(matches$afterEnd)
  matches$beforeStart <- as.numeric(matches$beforeStart)
  matches$afterStart <- as.numeric(matches$afterStart)

  return(matches)
}
