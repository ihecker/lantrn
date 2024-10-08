% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipini.R
\name{pipini}
\alias{pipini}
\title{Insert -> df + pipe operator}
\value{
Insert `-> df + pipe operator`
}
\description{
This function inserts the text `-> df + pipe operator` at the current selection in the active R script in RStudio.
}
\examples{
require(rstudioapi)
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  # Create a new R script document if RStudio is running
  rstudioapi::documentNew()
# Insert some text to select
rstudioapi::insertText("example")
# Set the initial selection
rstudioapi::setSelectionRanges(rstudioapi::getActiveDocumentContext()$range)
# Run the function
pipini()
# Get the updated context and cursor position
new_context <- rstudioapi::getActiveDocumentContext()
new_cursor <- rstudioapi::getCursorPosition()
# Check if the text was inserted correctly
expect_true("-> example \%>\%" \%in\% new_context$contents)
# Check if the cursor was moved to the next line
cursor_line <- new_cursor$line
cursor_column <- new_cursor$column
expect_equal(cursor_line, 2) # assuming the text was initially on line 1
expect_equal(cursor_column, 0) # assuming the cursor should be at the start of the new line
# Clean up by closing the document
rstudioapi::documentClose(rstudioapi::getActiveDocumentContext()$id, save = FALSE)
} else {
  message("RStudio is not running. Skipping RStudio-specific examples.")
}

}
