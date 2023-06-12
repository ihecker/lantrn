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

####not bad old num, new cat####
library(dplyr)
library(ggplot2)

# Sample data
age <- c(5, 15, 28, 32, 40, 52, 60, 70, 80)

# Recode age groups into scores
df <- data.frame(age = age) %>%
  mutate(scores = case_when(
    between(age, 0, 10) ~ "0-10",
    between(age, 11, 20) ~ "11-20",
    between(age, 21, 30) ~ "21-30",
    between(age, 31, 40) ~ "31-40",
    TRUE ~ "40+"
  ))

# Calculate counts for each age group and score
df_counts <- df %>%
  count(scores, sort = FALSE)

# Plot grouped bar plot with ggplot2
ggplot(df_counts, aes(x = scores, y = n, fill = scores)) +
  geom_bar(stat = "identity") +
  ylab("Frequency") +
  xlab("Age Group") +
  geom_text(aes(label = n), vjust = -0.5) +
  geom_segment(aes(x = scores, xend = scores, y = 0, yend = n),
               arrow = arrow(length = unit(0.02, "npc")), color = "black", alpha = 0.5) +
  theme(legend.position = "none")

####num ####
library(ggplot2)

# Sample data
old_values1 <- c(10, 20, 30, 40)
old_values2 <- c(15, 25, 35, 45)
new_values <- c(1, 2, 3, 4)

# Create data frame
df <- data.frame(old_values1, old_values2, new_values)

# Convert data frame to long format
library(tidyr)
df_long <- pivot_longer(df, cols = c(old_values1, old_values2), names_to = "Old_Variable", values_to = "Old_Values")

# Plot scatter plot with ggplot2
ggplot(df_long, aes(x = Old_Values, y = new_values, color = Old_Variable)) +
  geom_point() +
  geom_segment(aes(x = Old_Values, xend = Old_Values, y = 0, yend = new_values),
               arrow = arrow(length = unit(0.02, "npc")), color = "black", alpha = 0.5) +
  geom_segment(aes(x = 0, xend = Old_Values, y = new_values, yend = new_values),
               arrow = arrow(length = unit(0.02, "npc")), color = "black", alpha = 0.5) +
  xlim(0, max(df_long$Old_Values) + 5) +
  ylim(0, max(df_long$new_values) + 5) +
  xlab("Old Values") +
  ylab("New Values")

####old cat
