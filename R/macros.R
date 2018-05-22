#' Tracking an addition
#'
#' Call this function as an addin to add text at the cursor postion.
#'
#' @export
trackAdd <- function() {
  con <- rstudioapi::getActiveDocumentContext()

  # get cursor position
  docPos <- con$selection[[1]]$range$end

  # Add markup
  rstudioapi::insertText("{++++}", id = con$id)

  # move cursor
  docPosNew <- docPos + c(0, 3)
  rstudioapi::setCursorPosition(docPosNew, id = con$id)
}

#' Tracking a substitution
#'
#' Call this function as an addin to add the markdown track changes output for substitution.
#'
#' @export
trackSubstitute <- function() {
  con <- rstudioapi::getSourceEditorContext()

  # Get selected text
  selection <- con$selection[[1]]$text

  # Add markup
  docPos <- con$selection[[1]]$range$end
  rstudioapi::insertText(paste0("{~~", selection, "~>~~}"),
    id = con$id)

  # Move cursor
  docPosNew <- docPos + c(0, 5)
  rstudioapi::setCursorPosition(docPosNew, id = con$id)
}


#' Tracking highlighting
#'
#' Call this function as an addin to add the markdown track changes output for highlighting
#'
#' @export
trackHighlight <- function() {
  con <- rstudioapi::getSourceEditorContext()

  # Get selected text
  selection <- con$selection[[1]]$text

  # Add markup
  docPos <- con$selection[[1]]$range$end
  rstudioapi::insertText(paste0("{==", selection, "==}{>><<}"),
    id = con$id)

  # Move cursor
  docPosNew <- docPos + c(0, 9)
  rstudioapi::setCursorPosition(docPosNew, id = con$id)
}

#' Tracking a deletion
#'
#' Call this function as an addin to delete highlighted text.
#'
#' @export
trackDelete <- function() {
  con <- rstudioapi::getActiveDocumentContext()

  # start of highlight
  startPos <- con$selection[[1]]$range$start

  # end of highlight
  endPos <- con$selection[[1]]$range$end

  # Add markup
  rstudioapi::insertText(location = startPos, "{--", id = con$id)
  rstudioapi::insertText(location = endPos + c(0, 3), "--}",
    id = con$id)

  # move cursor
  startPosNew <- endPos + c(0, 6)
  rstudioapi::setCursorPosition(startPosNew, id = con$id)
}

#' Insert a comment
#'
#' Call this function as an addin to add a comment in a tracked doc at cursor position.
#'
#' @export
trackComment <- function() {
  con <- rstudioapi::getActiveDocumentContext()

  # cursor position
  docPos <- con$selection[[1]]$range$end

  # insert markup
  rstudioapi::insertText(location = docPos, "{>><<}", id = con$id)

  # move cursor
  startPosNew <- docPos + c(0, 3)
  rstudioapi::setCursorPosition(startPosNew, id = con$id)
}
