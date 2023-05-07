#' Insert ->df%>%
#'
#' @return Insert ->df%>% written with the cursor moved to the line below
#' @export
#'
#' @examples
#' Check and try the keyboard shortcut
pipini <- function() {
  selected_text <- rstudioapi::selectionGet()
  context <- rstudioapi::getActiveDocumentContext()
  current_selection <- rstudioapi::primary_selection(context)
  current_selection$range$end["column"] <- current_selection$range$end["column"]+1
  setCursorPosition(current_selection$range$end)
  insertText(paste0("->",selected_text[[1]],"%>%"))
  current_selection$range$end["row"] <- current_selection$range$end["row"]+1
  setCursorPosition(current_selection$range$end)
}

pipini()
