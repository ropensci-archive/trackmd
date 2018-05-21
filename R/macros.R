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

