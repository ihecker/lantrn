#'recocheck function
#'#'@description
#'Check the links between old variables to be recoded and the new recoded variable
#' @param df A data frame.
#' @param old_vars A character vector of variable names that are recoded.
#' @param new_var A character vector of new variable created using variables from old_vars
#' @return A plot showing the links between old_vars and new_var categories
#' @export
#' @examples
#' require(dplyr)
#' set.seed(1234)
#'
#' df <- data.frame(
#'   id = 1:50) %>%
#'   mutate(snack1 = rep(c("Apple", "Banana", "Carrots", "Celery", "Cucumber"), 10),
#'          snack2 = snack1,
#'          snack3 = snack1) %>%
#'   mutate(snack1= if_else(runif(50) < 0.3, NA, snack1),
#'          snack2= if_else(runif(50) < 0.3, NA, snack2),
#'          snack3= if_else(runif(50) < 0, NA, snack3),
#'          snack=coalesce(snack1,snack2,snack3))
#' coalcheck(df,c("snack1","snack2","snack3"),"snack")

coalcheck <- function(df,old_vars,new_var) {

  TotalCount = nrow(df) #to create percentages later

  df <- df %>%
    select(all_of(old_vars),all_of(new_var))

    for(y in nrow(df)) {
    for(i in 2:ncol(df)-1) {
      if(is.na(df[y,i])){df[y,i] <- df[y,i+1]}
    }
    }

  df <- df %>%
    make_long(c( all_of(old_vars)),all_of(new_var))

  stat <- as.data.frame(df) %>%
    group_by(x,node) %>%
    summarise(n=n())

  stat <- stat%>%
    dplyr::group_by(node)%>%
    dplyr::mutate(pct = n/TotalCount)

  df <- merge(df, stat, by.x = c('x','node'), by.y = c('x','node'), all.x = TRUE)

  plot <- ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = paste0(node," n=", n, '(',  round(pct* 100,1), '%)' ))) +
    geom_sankey(flow.alpha = .6,
                node.color = "gray30",
                position="identity") +
    scale_fill_brewer(palette = "Accent")+
    geom_sankey_label(size = 3) +
    theme_sankey(base_size = 9) +
    labs(x = NULL, fill="Categories") +
    theme(legend.position = "top",
          plot.title = element_text(hjust = .5),
          legend.background = element_rect(fill = "white", colour = "black"),
          #axis.text.x = element_blank()
          )
  return(list(df,plot))
}





