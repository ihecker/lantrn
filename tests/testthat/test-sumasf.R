library(testthat)
library(rstudioapi)

# Ensure the test is run only in RStudio
if (rstudioapi::isAvailable()) {
  test_that("sumasf inserts the correct text and moves the cursor", {
    # Create a new R script document
    rstudioapi::documentNew()

    # Get the initial context and cursor position
    initial_context <- rstudioapi::getActiveDocumentContext()
    initial_cursor <- rstudioapi::getCursorPosition()

    # Run the function
    sumasf()

    # Get the context and cursor position after running the function
    new_context <- rstudioapi::getActiveDocumentContext()
    new_cursor <- rstudioapi::getCursorPosition()

    # Check if the text was inserted correctly
    expect_true("summary(as.factor())" %in% new_context$contents)

    # Check if the cursor was moved correctly
    expect_equal(new_cursor$column, initial_cursor$column - 2)

    # Clean up by closing the document
    rstudioapi::documentClose(rstudioapi::getActiveDocumentContext()$id, save = FALSE)
  })
} else {
  test_that("sumasf is not tested outside of RStudio", {
    expect_true(TRUE)
  })
}
