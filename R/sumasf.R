#' Insert summary(as.factor())
#'
#' @return summary(as.factor()) written with the cursor moved into the brackets
#' @export
#'
#' @examples
#' require(rstudioapi)

#' sumasf()

sumasf <- function() {
  insertText("summary(as.factor())")
  context <- rstudioapi::getActiveDocumentContext()
  current_selection <- rstudioapi::primary_selection(context)
  current_selection$range$start["column"] <- current_selection$range$start["column"]-2
  setCursorPosition(current_selection$range$start)
}
