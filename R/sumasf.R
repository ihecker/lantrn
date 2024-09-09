#' Insert summary(as.factor())
#' @name sumasf
#' @description
#' This function inserts the text `summary(as.factor())` into the active R script in RStudio.
#' After insertion, the cursor is moved to a position inside the parentheses so that you can
#' quickly start typing the arguments for `summary`.
#'
#' @return summary(as.factor()) written with the cursor moved into the brackets
#' @export
#'
#' @examples
#' require(rstudioapi)
#' if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
#'   # Create a new R script document if RStudio is running
#'   rstudioapi::documentNew()
#' # Get the initial context and cursor position
#' initial_context <- rstudioapi::getActiveDocumentContext()
#' initial_cursor <- rstudioapi::getCursorPosition()
#' # Run the function
#' sumasf()
#' # Get the context and cursor position after running the function
#' new_context <- rstudioapi::getActiveDocumentContext()
#' new_cursor <- rstudioapi::getCursorPosition()
#' # Check if the text was inserted correctly
#' expect_true("summary(as.factor())" %in% new_context$contents)
#' # Check if the cursor was moved correctly
#' expect_equal(new_cursor$column, initial_cursor$column - 2)
#' # Clean up by closing the document
#' rstudioapi::documentClose(rstudioapi::getActiveDocumentContext()$id, save = FALSE)
#' } else {
#'   message("RStudio is not running. Skipping RStudio-specific examples.")
#' }
sumasf <- function() {
  rstudioapi::insertText("summary(as.factor())")
  context <- rstudioapi::getActiveDocumentContext()
  current_selection <- rstudioapi::primary_selection(context)
  current_selection$range$start["column"] <- current_selection$range$start["column"] - 2
  rstudioapi::setCursorPosition(current_selection$range$start)
}
