#' seletail function
#' @name seletail
#' @description
#' Extract a specified number of variables starting from the last variable in a data frame.
#' @param df A data frame.
#' @param n Number of variables to keep starting from the last variable.
#' @param selec A logical value indicating whether to print the names of the selected variables. Default is `FALSE`.
#' @param exclu A logical value indicating whether to print the names of the excluded variables. Default is `FALSE`.
#' @return A data frame with the specified number of variables starting from the last variable.
#' @export
#' @examples
#' df <- data.frame(
#'   Snack1 = c("Apple", "Carrot sticks", "Yogurt", "Almonds", "Celery"),
#'   Snack2 = c("Baby carrots", "Kale chips", "String cheese", "Berries", "Bell peppers"),
#'   Snack3 = c("Trail mix", "Cottage cheese", "Pineapple", "Walnuts", "Broccoli"),
#'   Snack4 = c("Popcorn", "Hard-boiled eggs", "Watermelon", "Cashews", "Cauliflower"),
#'   Snack5 = c("Rice cakes", "Greek yogurt", "Oranges", "Hazelnuts", "Cherry tomatoes")
#' )
#'
#' seletail(df, 2, selec = TRUE, exclu = TRUE)
seletail <- function(df, n, selec = FALSE, exclu = FALSE) {
  total_cols <- ncol(df)

  if (n <= 0) {
    stop("The number of columns to select must be greater than zero. You need to select at least one column!")
  }

  if (n > total_cols) {
    stop("The number of columns to select exceeds the total number of columns in the dataframe. Please choose a valid number.")
  }

  selected <- colnames(df[, (ncol(df) - n + 1):ncol(df)])
  excluded <- colnames(df[, 1:(ncol(df) - n)])
  df <- df[, (ncol(df) - n + 1):ncol(df)]
  if (selec) {
    txt_selec <- "Selected variables:"
  } else {
    selected <- NULL
    txt_selec <- NULL
  }

  if (exclu) {
    txt_exclu <- "Excluded variables:"
  } else {
    excluded <- NULL
    txt_exclu <- NULL
  }
  cat(txt_selec, selected, "", txt_exclu, excluded, sep = "\n")
  return(df)
}
