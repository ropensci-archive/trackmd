#' Create track changes diff
#' 
#' Compares two strings and creates a diff using the markdown track changes syntax.
#' 
#' @param before (\code{character} of length 1) The text before the changes.
#' @param after (\code{character} of length 1) The text after the changes.
#' @param style (\code{character} of length 1) The type of change tags to add.
#'   Either "critic" for Critical Markdown tags, or "pandoc" for the track
#'   change tags used by pandoc.
#' 
#' @keywords internal
diff_to_markup <- function(before, after, style = "critic") {
  
  # Decide which diff formatting functions to use
  if (style == "critic") {
    addFunc <- criticAdd
    delFunc <- criticDel
    subFunc <- criticSub
  } else if (style == "pandoc") {
    addFunc <- pandocAdd
    delFunc <- pandocDel
    subFunc <- pandocSub
  } else {
    stop('Invlaid style "', style, '" entered.')
  }
  
  # Convert string to characters
  before <- unlist(strsplit(before, split = ""))
  after <- unlist(strsplit(after, split = ""))
  
  # Calculate diff
  difference <- diffobj::ses(before, after)
  
  apply_addition <- function(text, index, afterStart, afterEnd) {
    c(text[1:(index + offset)],
      addFunc(after[afterStart:afterEnd]),
      text[(index + 1 + offset):length(text)])
  }
  
  apply_deletion <- function(text, beforeStart, beforeEnd) {
    c(text[1:(beforeStart - 1 + offset)],
      delFunc(before[beforeStart:beforeEnd]),
      text[(beforeEnd + 1 + offset):length(text)])
  }

  apply_change <- function(text, beforeStart, beforeEnd, afterStart, afterEnd) {
    c(text[1:(beforeStart - 1 + offset)],
      subFunc(before[beforeStart:beforeEnd], after[afterStart:afterEnd]),
      text[(beforeEnd + 1 + offset):length(text)])
  }
  
  text <- before
  offset <- 0
  for (aDiff in difference) {
    length_before <- length(text)
    sesValues <- getSesValues(aDiff)
    if (sesValues$type == "a") {
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


#' @keywords internal
criticAdd <- function(text) {
  paste0("{++", paste0(text, collapse = ""), "++}")
}

#' @keywords internal
criticDel <- function(text) {
  paste0("{--", paste0(text, collapse = ""), "--}")
}

#' @keywords internal
criticSub <- function(from, to) {
  paste0("{~~", paste0(from, collapse = ""), "~>", paste0(to, collapse = ""), "~~}")
}


#' @keywords internal
pandocAdd <- function(text, author = "unknown", timestamp = lubridate::date()) {
  paste0('<span class="insertion" author="', author, '" date="', timestamp, '">', paste0(text, collapse = ""), '</span>')
}

#' @keywords internal
pandocDel <- function(text, author = "unknown", timestamp = lubridate::date()) {
  paste0('<span class="deletion" author="', author, '" date="', timestamp, '">', paste0(text, collapse = ""), '</span>')
}

#' @keywords internal
pandocSub <- function(from, to, author = "unknown", timestamp = lubridate::date()) {
  paste0(pandocDel(from, author = author, timestamp = timestamp),
         pandocAdd(from, author = author, timestamp = timestamp))
}
