#' Insert -> df %>%
#' @name pipini
#' @description
#' This function inserts the text `-> df %>%` at the current selection in the active R script in RStudio.
#'
#' @return Insert "-> df %>%"
#' @export
#'
#' @examples
#' require(rstudioapi)
#' if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
#'   # Create a new R script document if RStudio is running
#'   rstudioapi::documentNew()
#' # Insert some text to select
#' rstudioapi::insertText("example")
#' # Set the initial selection
#' rstudioapi::setSelectionRanges(rstudioapi::getActiveDocumentContext()$range)
#' # Run the function
#' pipini()
#' # Get the updated context and cursor position
#' new_context <- rstudioapi::getActiveDocumentContext()
#' new_cursor <- rstudioapi::getCursorPosition()
#' # Check if the text was inserted correctly
#' expect_true("-> example %>%" %in% new_context$contents)
#' # Check if the cursor was moved to the next line
#' cursor_line <- new_cursor$line
#' cursor_column <- new_cursor$column
#' expect_equal(cursor_line, 2) # assuming the text was initially on line 1
#' expect_equal(cursor_column, 0) # assuming the cursor should be at the start of the new line
#' # Clean up by closing the document
#' rstudioapi::documentClose(rstudioapi::getActiveDocumentContext()$id, save = FALSE)
#' } else {
#'   message("RStudio is not running. Skipping RStudio-specific examples.")
#' }
#'
library(rstudioapi)
pipini <- function() {
  selected_text <- rstudioapi::selectionGet()
  context <- rstudioapi::getActiveDocumentContext()
  current_selection <- rstudioapi::primary_selection(context)
  current_selection$range$end["column"] <- current_selection$range$end["column"] + 1
  setCursorPosition(current_selection$range$end)
  insertText(paste0("-> ", selected_text[[1]], " %>%"))
  current_selection$range$end["row"] <- current_selection$range$end["row"] + 1
  setCursorPosition(current_selection$range$end)
}
