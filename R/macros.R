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
