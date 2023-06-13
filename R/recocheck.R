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



####old num, new cat####
library(ggplot2)
library(dplyr)

# Function to create a horizontal grouped bar chart with annotations
library(purrr)

create_grouped_bar_chart <- function(df, old_vars, new_var) {
  # Prepare the data for annotations
  annotations <- df %>%
    group_by(category) %>%
    summarize(across(all_of(old_vars), sum), recoded_value = n())

  # Create the grouped bar chart with annotations
  p <- ggplot(df, aes(x = category))

  for (i in seq_along(old_vars)) {
    p <- p + geom_col(aes(y = .data[[old_vars[[i]]]], fill = sprintf("Variable %d", i)), position = "stack", width = 0.5)
  }

  p <- p + annotate(
    "text",
    x = annotations$category,
    y = rowSums(annotations[, all_of(old_vars)]),
    label = paste("Total:", rowSums(annotations[, all_of(old_vars)])),
    hjust = -0.2,
    color = "black"
  ) +
    annotate(
      "text",
      x = annotations$category,
      y = rowSums(annotations[, all_of(old_vars)]),
      label = paste("Recoded:", annotations$recoded_value),
      hjust = 1.2,
      color = "black"
    ) +
    labs(x = "Category", y = "Value") +
    scale_fill_brewer(palette = "Accent") +
    theme_minimal() +
    coord_flip()  # Flips the chart to make it horizontal

  return(p)
}



# Example usage
df <- data.frame(
  category = c("A", "B", "C", "D"),
  var1 = c(10, 15, 8, 12),
  var2 = c(7, 9, 14, 5))

df<- df %>%
  mutate(recoded = if_else(var1 > var2, "High", "Low"))

create_grouped_bar_chart(df, c("var1","var2"), "recoded")


####num both####
library(ggplot2)

# Sample data
old_values1 <- c(10, 20, 30, 40)
old_values2 <- c(15, 25, 35, 45)
old_values3 <- c(40, 40, 40, 45)
new_values <- c(1, 2, 3, 4)

# Create data frame
df <- data.frame(old_values1, old_values2, old_values3, new_values)

# Convert data frame to long format
library(tidyr)

create_grouped_bar_chart <- function(df, old_vars, new_var) {
df_long <- pivot_longer(df, cols = all_of(cat_vars), names_to = "Old_Variable", values_to = "Old_Values")

# Plot scatter plot with ggplot2
plot<- ggplot(df_long, aes(x = Old_Values, y = new_var, color = Old_Variable)) +
  geom_point(size=10) +
  scale_colour_brewer(palette = "Accent") +
  geom_segment(aes(x = Old_Values, xend = Old_Values, y = 0, yend = new_var),
               arrow = arrow(length = unit(0.02, "npc")), color = "black", alpha = 0.5) +
  geom_segment(aes(x = 0, xend = Old_Values, y = new_var, yend = new_var),
               arrow = arrow(length = unit(0.02, "npc")), color = "black", alpha = 0.5) +
  xlim(0, max(df_long$Old_Values) + 5) +
  ylim(0, max(df_long$new_var) + 5) +
  xlab("Old Values") +
  ylab("New Values") +
  theme_minimal() +
  coord_flip()

return(plot)

}

create_grouped_bar_chart <- function(df, c("old_values1", "old_values2", "old_values3"), new_values)


####old cat, new num####

library(ggplot2)
library(dplyr)
library(tidyr)

# Function to create a grouped bar chart with multiple categorical variables and a numeric variable
create_grouped_bar_chart <- function(df, old_vars, new_var) {

  # Prepare the data for the grouped bar chart
  df_long <- df %>%
    pivot_longer(cols = all_of(old_vars), names_to = "Category", values_to = "Value")

  # Create the grouped bar chart
  plot <- ggplot(df_long, aes(x = Value, y = num_var, fill = Category)) +
    geom_col(position = "dodge") +
    labs(x = "Category", y = new_var) +
    theme_minimal()

  return(plot)
}

# Example usage
df <- data.frame(
  category1 = c("A", "B", "C", "D"),
  category2 = c("X", "Y", "X", "Y"),
  category3 = c("M", "M", "N", "N"),
  num_var = c(10, 15, 8, 12)
)

create_grouped_bar_chart(df, c("category1", "category2", "category3"), "num_var")

####old mix new cat####
library(ggplot2)
library(dplyr)
library(tidyr)

# Function to create a stacked bar chart with a mix of old categorical and numeric variables recoded to a categorical value
create_stacked_bar_chart <- function(df, old_cat_vars, old_num_vars, recoded_var) {

  # Recode old numeric variables to categorical values
  df <- df %>%
    mutate(
      across(all_of(old_num_vars),
             ~ ifelse(. > 0, "Positive", ifelse(. < 0, "Negative", "Zero")))
    )

  # Prepare the data for the stacked bar chart
  df_long <- df %>%
    pivot_longer(cols = all_of(old_cat_vars), names_to = "Category", values_to = "Value")

  # Create the stacked bar chart
  ggplot(df_long, aes(x = Category, fill = Value)) +
    geom_bar(position = "fill") +
    labs(x = "Category", y = "Proportion", fill = recoded_var) +
    theme_minimal()
}

# Example usage
df <- data.frame(
  category1 = c("A", "B", "C", "D"),
  category2 = c("X", "Y", "X", "Y"),
  num_var1 = c(10, -15, 8, -12),
  num_var2 = c(-5, 3, 0, -2)
)

create_stacked_bar_chart(df, c("category1", "category2"), c("num_var1", "num_var2"), "recoded")

####old mix new num####

library(ggplot2)
library(dplyr)
library(tidyr)

# Function to create a scatter plot with a mix of old categorical and numeric variables recoded to a numeric value
create_scatter_plot <- function(df, old_cat_vars, old_num_vars, recoded_var) {

  # Recode old numeric variables to numeric values
  df <- df %>%
    mutate(
      across(all_of(old_num_vars),
             ~ ifelse(. > 0, 1, ifelse(. < 0, -1, 0)))
    )

  # Prepare the data for the scatter plot
  df_long <- df %>%
    pivot_longer(cols = all_of(old_cat_vars), names_to = "Category", values_to = "Value")

  # Create the scatter plot
  ggplot(df_long, aes(x = Category, y = Value, color = as.factor({{ recoded_var }}))) +
    geom_point() +
    labs(x = "Category", y = "Value", color = recoded_var) +
    theme_minimal()
}

# Example usage
df <- data.frame(
  category1 = c("A", "B", "C", "D"),
  category2 = c("X", "Y", "X", "Y"),
  num_var1 = c(10, -15, 8, -12),
  num_var2 = c(-5, 3, 0, -2)
)

create_scatter_plot(df, c("category1", "category2"), c("num_var1", "num_var2"), "recoded")
