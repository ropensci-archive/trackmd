#' Tracking an addition
#'
#' Call this function as an addin to add text at the cursor postion.
#'
#' @export
add <- function() {
  con <- rstudioapi::getActiveDocumentContext()
  docPos <- con$selection[[1]]$range$end
  rstudioapi::insertText("{++  ++}", id = con$id)
  docPosNew <- docPos + c(0, 4)
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
  rstudioapi::insertText(location = startPos, "{-- ", id = con$id)
  rstudioapi::insertText(location = endPos + c(0,4), " --}", id = con$id)
  startPosNew <- endPos + c(0, 8)
  rstudioapi::setCursorPosition(startPosNew, id = con$id)
}
