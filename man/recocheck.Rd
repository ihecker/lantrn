% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recocheck.R
\name{recocheck}
\alias{recocheck}
\title{recocheck function
#'@description
Check the links between old variables to be recoded and the new recoded variable}
\usage{
recocheck(df, old_vars, new_var)
}
\arguments{
\item{df}{A data frame.}

\item{old_vars}{A character vector of variable names that are recoded.}

\item{new_var}{A character vector of new variable created using variables from old_vars}
}
\value{
A plot showing the links between old_vars and new_var categories
}
\description{
recocheck function
#'@description
Check the links between old variables to be recoded and the new recoded variable
}
\examples{
require(dplyr)
df <- data.frame(
id = rep(1:5, each = 10),
snack = rep(c("Apple", "Banana", "Carrots", "Celery", "Cucumber", "Grapes", "Pumpkin seeds", "Sunflower seeds", NA, "Missing"), 5)
) \%>\%
 mutate(
   category = case_when(
     snack \%in\% c("Apple", "Banana", "Orange", "Grapes") ~ "Fruit lovers",
     snack \%in\% c("Carrots", "Cucumber", "Celery", "Bell peppers") ~ "Veggies fans",
     TRUE ~ "Nuts and seeds"
   ),
   id_category = case_when(
     id \%in\% c(1, 2) ~ "Group 1",
     id \%in\% c(3, 4) ~ "Group 2",
     TRUE ~ "Other"
   ),
   new_snack_group = case_when(
     snack \%in\% c("Apple", "Banana", "Orange", "Grapes") & id \%in\% c(1, 2) ~ "Group 1 fruit lovers",
     snack \%in\% c("Carrots", "Cucumber", "Celery", "Bell peppers") & id \%in\% c(3, 4) ~ "Group 2 veggie fans"
   ),
   merge1=ifelse(row_number()<=25,"merge",NA),
   merge2=ifelse(row_number()>25,"merge",NA),
   merge=coalesce(merge1,merge2)
 )

recocheck(df,c("snack","id_category"),"new_snack_group")
}
