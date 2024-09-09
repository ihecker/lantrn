library(testthat)
library(rstudioapi)

# Ensure the test is run only in RStudio
if (rstudioapi::isAvailable()) {
  test_that("pipini inserts the correct text and moves the cursor to the next line", {
    # Create a new R script document
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
    expect_true("-> example %>%" %in% new_context$contents)

    # Check if the cursor was moved to the next line
    cursor_line <- new_cursor$line
    cursor_column <- new_cursor$column
    expect_equal(cursor_line, 2) # assuming the text was initially on line 1
    expect_equal(cursor_column, 0) # assuming the cursor should be at the start of the new line

    # Clean up by closing the document
    rstudioapi::documentClose(rstudioapi::getActiveDocumentContext()$id, save = FALSE)
  })
} else {
  test_that("pipini is not tested outside of RStudio", {
    expect_true(TRUE)
  })
}
