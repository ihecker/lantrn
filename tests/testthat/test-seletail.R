library(testthat)

test_that("seletail function works correctly", {
  df <- data.frame(
    Snack1 = c("Apple", "Carrot sticks", "Yogurt", "Almonds", "Celery", "Banana", "Grapes", "Cucumber", "Peanut butter", "Orange"),
    Snack2 = c("Baby carrots", "Kale chips", "String cheese", "Berries", "Bell peppers", "Pistachios", "Mango", "Cherry tomatoes", "Hummus", "Pear"),
    Snack3 = c("Trail mix", "Cottage cheese", "Pineapple", "Walnuts", "Broccoli", "Peach", "Blueberries", "Snap peas", "Greek yogurt", "Strawberries"),
    Snack4 = c("Popcorn", "Hard-boiled eggs", "Watermelon", "Cashews", "Cauliflower", "Kiwi", "Raspberries", "Zucchini", "Nut butter", "Grapefruit"),
    Snack5 = c("Rice cakes", "Greek yogurt", "Oranges", "Hazelnuts", "Cherry tomatoes", "Apple slices", "Blackberries", "Radishes", "Veggies with dip", "Mandarin")
  )

  # Select last 2 columns without printing selections or exclusions
  result <- seletail(df, 2, selec = FALSE, exclu = FALSE)
  expect_equal(colnames(result), c("Snack4", "Snack5"))

  # Select last 3 columns with both selection and exclusion output
  result <- seletail(df, 3, selec = TRUE, exclu = TRUE)
  expect_equal(colnames(result), c("Snack3", "Snack4", "Snack5"))

  # Correct variables are excluded and selected
  result <- capture.output(seletail(df, 2, selec = TRUE, exclu = TRUE))
  expect_true("Selected variables:" %in% result)
  expect_true("Snack4" %in% result)
  expect_true("Snack5" %in% result)
  expect_true("Excluded variables:" %in% result)
  expect_true("Snack1" %in% result)
  expect_true("Snack3" %in% result)

  # n is 0
  test_that("Error is raised when n is 0 or negative", {
    expect_error(
      seletail(df, 0),
      "The number of columns to select must be greater than zero. You need to select at least one column!"
    )

    expect_error(
      seletail(df, -1),
      "The number of columns to select must be greater than zero. You need to select at least one column!"
    )
  })

  # n exceeds the total number of columns
  test_that("Error is raised when n exceeds the number of columns", {
    expect_error(
      seletail(df, 6),
      "The number of columns to select exceeds the total number of columns in the dataframe. Please choose a valid number."
    )
  })
})
