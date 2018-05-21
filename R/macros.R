#' Tracking an addition
#'
#' Call this function as an addin to add text at the cursor postion.
#'
#' @export
add <- function() {
  con <- rstudioapi::getActiveDocumentContext()
  docPos <- con$selection[[1]]$range$end
  rstudioapi::insertText("{++++}", id = con$id)
  docPosNew <- docPos + c(0, 3)
  rstudioapi::setCursorPosition(docPosNew, id = con$id)
}

#' Tracking a substitution
#'
#' Call this function as an addin to add the markdown track changes output for substitution.
#'
#' @export
substitute <- function() {
  con <- rstudioapi::getSourceEditorContext()
  
  # Get selected text
  selection <- con$selection[[1]]$text
  
  # Add markup
  docPos <- con$selection[[1]]$range$end
  rstudioapi::insertText(paste0("{~~", selection, "~>~~}"), id = con$id)
  
  # Move cursor
  docPosNew <- docPos + c(0, 5)
  rstudioapi::setCursorPosition(docPosNew, id = con$id)
}


#' Tracking highlighting
#'
#' Call this function as an addin to add the markdown track changes output for highlighting
#'
#' @export
highlight <- function() {
  con <- rstudioapi::getSourceEditorContext()
  
  # Get selected text
  selection <- con$selection[[1]]$text
  
  # Add markup
  docPos <- con$selection[[1]]$range$end
  rstudioapi::insertText(paste0("{==", selection, "==}{>><<}"), id = con$id)
  
  # Move cursor
  docPosNew <- docPos + c(0, 9)
  rstudioapi::setCursorPosition(docPosNew, id = con$id)
}

#' Tracking a deletion
#'
#' Call this function as an addin to delete highlighted text.
#'
#' @export
delete <- function() {
  con <- rstudioapi::getActiveDocumentContext()
  startPos <- con$selection[[1]]$range$start
  endPos <- con$selection[[1]]$range$end
  rstudioapi::insertText(location = startPos, "{--", id = con$id)
  rstudioapi::insertText(location = endPos + c(0,3), "--}", id = con$id)
  startPosNew <- endPos + c(0, 6)
  rstudioapi::setCursorPosition(startPosNew, id = con$id)
}

#' Insert a comment
#'
#' Call this function as an addin to add a comment in a tracked doc at cursor position.
#'
#' @export
comment <- function() {
  con <- rstudioapi::getActiveDocumentContext()
  docPos <- con$selection[[1]]$range$end
  rstudioapi::insertText(location = docPos, "{>><<}", id = con$id)
  startPosNew <- docPos + c(0, 3)
  rstudioapi::setCursorPosition(startPosNew, id = con$id)
}
