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
#' df <- data.frame(
#'id = rep(1:5, each = 10),
#'snack = rep(c("Apple", "Banana", "Carrots", "Celery", "Cucumber", "Grapes", "Pumpkin seeds", "Sunflower seeds", NA, "Missing"), 5)
#') %>%
#'  mutate(
#'    category = case_when(
#'      snack %in% c("Apple", "Banana", "Orange", "Grapes") ~ "Fruit lovers",
#'      snack %in% c("Carrots", "Cucumber", "Celery", "Bell peppers") ~ "Veggies fans",
#'      TRUE ~ "Nuts and seeds"
#'    ),
#'    id_category = case_when(
#'      id %in% c(1, 2) ~ "Group 1",
#'      id %in% c(3, 4) ~ "Group 2",
#'      TRUE ~ "Other"
#'    ),
#'    new_snack_group = case_when(
#'      snack %in% c("Apple", "Banana", "Orange", "Grapes") & id %in% c(1, 2) ~ "Group 1 fruit lovers",
#'      snack %in% c("Carrots", "Cucumber", "Celery", "Bell peppers") & id %in% c(3, 4) ~ "Group 2 veggie fans"
#'    ),
#'    merge1=ifelse(row_number()<=25,"merge",NA),
#'    merge2=ifelse(row_number()>25,"merge",NA),
#'    merge=coalesce(merge1,merge2)
#'  )
#'
#'recocheck(df,c("snack","id_category"),"new_snack_group")
recocheck <- function(df,old_vars,new_var) {

  TotalCount = nrow(df) #to create percentages later

  df <- df %>%
    select(all_of(old_vars),all_of(new_var))%>%
    tidyr::pivot_longer(cols = -all_of(new_var))%>%
    select(value, name, all_of(new_var))%>%
    mutate(value=paste0(value,";;",name))%>%
    make_long(c(value, all_of(new_var))) %>%
    mutate(name=as.character(str_extract(node, "(?<=;;)[^;]+$")),
           name=case_when(is.na(name)~x,
                          T~name),
           node=as.character(str_extract(node, "^[^;]+")))

  stat <- df%>%
    dplyr::group_by(node)%>%
    tally()

  stat <- stat%>%
    dplyr::group_by(node)%>%
    dplyr::mutate(pct = n/TotalCount)

  # Step 3
  df <- merge(df, stat, by.x = 'node', by.y = 'node', all.x = TRUE)

  plot <- ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(name), label = paste0(node," n=", n, '(',  round(pct* 100,1), '%)' ))) +
    geom_sankey(flow.alpha = .6,
                node.color = "gray30",
                position="identity") +
    scale_fill_brewer(palette = "Accent")+
    geom_sankey_label(size = 3) +
    theme_sankey(base_size = 9) +
    labs(x = NULL, fill="Variables") +
    theme(legend.position = "top",
          plot.title = element_text(hjust = .5),
          legend.background = element_rect(fill = "white", colour = "black"),
          axis.text.x = element_blank())

  return(plot)
}
