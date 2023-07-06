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
#'

library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)


recocheck <- function(df,old_vars,new_var) {

  if (!(any(sapply(df[c(old_vars, new_var)], is.numeric))))
    {

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

if (!(is.numeric(df[[new_var]]))&is.numeric(all_of(df[[new_var]]))){

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


####num both####

if (all(sapply(df[c(old_vars, new_var)], is.numeric)))
  {


df_long <- pivot_longer(df, cols = all_of(old_vars), names_to = "Old_Variable", values_to = "Old_Values")

# Plot scatter plot with ggplot2
plot<- ggplot(df_long, aes(x = Old_Values, y = factor(.data[[new_var]]), color = Old_Variable)) +
  geom_point(size=10) +
  scale_colour_brewer(palette = "Accent") +
  geom_segment(aes(x = Old_Values, xend = Old_Values, y = 0, yend = factor(.data[[new_var]])),
               arrow = arrow(length = unit(0.02, "npc")), color = "black", alpha = 0.5) +
  geom_segment(aes(x = 0, xend = Old_Values, y = factor(.data[[new_var]]), yend = factor(.data[[new_var]])),
               arrow = arrow(length = unit(0.02, "npc")), color = "black", alpha = 0.5) +
  xlab("Old Values") +
  ylab("New Values") +
  theme_minimal()

return(plot)

}


####old cat, new num####


# Function to create a grouped bar chart with multiple categorical variables and a numeric variable
if (!any(sapply(df[, old_vars], is.numeric)) && is.numeric(df[[new_var]])){


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

  if (any(sapply(df[, old_vars], is.numeric)) &&
      any(sapply(df[, old_vars], function(x) !is.numeric(x))) &&
      is.numeric(df[[new_var]])) {

    toKeep <- sapply(df, is.numeric)


  # Convert all variables to character type
  df <- df %>%
    mutate(across(c(old_vars, new_var), as.character))


  # Prepare the data for the scatter plot
  df_scatter <- df %>%
    select(new_var, names(toKeep)[as.numeric(toKeep) == 1]) %>%
    pivot_longer(cols = -new_var, names_to = "Variable", values_to = "Value")

  # Prepare the data for the stacked bars
  df_bars <- df %>%
    select(new_var, names(toKeep)[as.numeric(toKeep) == 0]) %>%
    pivot_longer(cols = -new_var, names_to = "Category", values_to = "Value") %>%
    mutate(Variable = "Categorical")

  # Combine the data for the scatter plot and stacked bars
  df_combined <- bind_rows(df_scatter, df_bars)

  # Create the mixed plot
  plot<- ggplot(df_combined, aes(x = Category, y = Value, fill = Variable)) +
    geom_bar(data = filter(df_combined, Variable == "Categorical"), stat = "identity", position = "stack") +
    geom_point(data = filter(df_combined, Variable != "Categorical"), position = position_dodge(width = 0.5)) +
    labs(x = "Category", y = "Value", fill = "Variable") +
    theme_minimal()
 return(plot)
}

####old mix new cat to code


  if (any(sapply(df[, old_vars], is.numeric)) &&
      any(sapply(df[, old_vars], function(x) !is.numeric(x))) &&
      !(is.numeric(df[[new_var]]))) {

    toKeep <- sapply(df, is.numeric)


  # Prepare the data for the scatter plot
  df_scatter <- df %>%
    select(new_var, names(toKeep)[as.numeric(toKeep) == 1]) %>%
    pivot_longer(cols = -new_var, names_to = "Variable", values_to = "Value")

  # Prepare the data for the stacked bars
  df_bars <- df %>%
    select(new_var, names(toKeep)[as.numeric(toKeep) == 0]) %>%
    pivot_longer(cols = -new_var, names_to = "Category", values_to = "Value") %>%
    mutate(Variable = "Categorical")

  # Combine the data for the scatter plot and stacked bars
  df_combined <- bind_rows(df_scatter, df_bars)

  # Create the mixed plot
  plot<- ggplot(df_combined, aes(x = Category, y = Value, fill = Variable)) +
    geom_bar(data = filter(df_combined, Variable == "Categorical"), stat = "identity", position = "stack") +
    geom_point(data = filter(df_combined, Variable != "Categorical"), position = position_dodge(width = 0.5)) +
    labs(x = "Category", y = "Value", fill = "Variable") +
    theme_minimal()

  return(plot)
}

}

# df <- data.frame(
#   category1 = c("A", "B", "C", "D"),
#   category2 = c("X", "Y", "X", "Y"),
#   num_var1 = c(10, -15, 8, -12),
#   num_var2 = c(-5, 3, 0, -2)
# )
#
# df <- df %>%
#   mutate(recoded=ifelse(num_var1 > 0, 1, ifelse(. < 0, -1, 0))
#   )
#
# recocheck(df, c("category1", "category2","num_var1", "num_var2"), "recoded")
#
#
#
# # Example usage
#
#
# create_scatter_plot(df, c("category1", "category2"), c("num_var1", "num_var2"), "recoded")
#
#
# # Example usage #old mix new num
# df <- data.frame(
#   category1 = c("A", "B", "C", "D"),
#   category2 = c("X", "Y", "X", "Y"),
#   num_var1 = c(10, -15, 8, -12),
#   num_var2 = c(-5, 3, 0, -2)
# )
#
# df$recoded_var <- ifelse(df$num_var1 > 0, "Positive", ifelse(df$num_var1 < 0, "Negative", "Zero"))
#
# create_mixed_plot(df, c("category1", "category2"), c("num_var1", "num_var2"), "recoded_var")
#
#
#
#
#
# # Example usage old mix new cat
# df <- data.frame(
#   category1 = c("A", "B", "C", "D"),
#   category2 = c("X", "Y", "X", "Y"),
#   num_var1 = c(10, -15, 8, -12),
#   num_var2 = c(-5, 3, 0, -2)
# )
#
# df$recoded_var <- ifelse(df$num_var1 > 0, "Positive", ifelse(df$num_var1 < 0, "Negative", "Zero"))
#
# create_mixed_plot(df, c("category1", "category2"), c("num_var1", "num_var2"), "recoded_var")
