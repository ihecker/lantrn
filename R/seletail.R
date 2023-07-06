#'seletail function
#'#'@description
#' Extract a specified number of variables starting from the last variable in a data frame.
#' @param df A data frame.
#' @param n Number of variables to keep starting from the last variable.
#' @return A data frame with the specified number of variables starting from the last variable.
#' @export
#' @examples
#' df <- data.frame(
#' Snack1 = c("Apple", "Carrot sticks", "Yogurt", "Almonds", "Celery", "Banana", "Grapes", "Cucumber", "Peanut butter", "Orange"),
#' Snack2 = c("Baby carrots", "Kale chips", "String cheese", "Berries", "Bell peppers", "Pistachios", "Mango", "Cherry tomatoes", "Hummus", "Pear"),
#' Snack3 = c("Trail mix", "Cottage cheese", "Pineapple", "Walnuts", "Broccoli", "Peach", "Blueberries", "Snap peas", "Greek yogurt", "Strawberries"),
#' Snack4 = c("Popcorn", "Hard-boiled eggs", "Watermelon", "Cashews", "Cauliflower", "Kiwi", "Raspberries", "Zucchini", "Nut butter", "Grapefruit"),
#' Snack5 = c("Rice cakes", "Greek yogurt", "Oranges", "Hazelnuts", "Cherry tomatoes", "Apple slices", "Blackberries", "Radishes", "Veggies with dip", "Mandarin")
#' )
#'
#' seletail(df,2,selec=T,exclu=T)

seletail <- function(df, n, selec = FALSE, exclu = FALSE) {

  selected <- colnames(df[, (ncol(df) - n + 1):ncol(df)])
  excluded <- colnames(df[, 1:(ncol(df) - n)])
  df<- df[, (ncol(df) - n + 1):ncol(df)]
  if (selec) { txt_selec<- "Selected variables:"
  } else{selected<- NULL
txt_selec<- NULL}

  if (exclu) { txt_exclu<- "Excluded variables:"
  } else{excluded<- NULL
  txt_exclu<- NULL}
cat(txt_selec,selected,"",txt_exclu,excluded,sep="\n")
 return(df)
}

new_df<- df %>% seletail(2,T,F)

