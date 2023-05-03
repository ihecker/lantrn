#'recocheck function
#'Check the links between old variables to be recoded and the new recoded variable

recocheck <- function(df,old_vars,new_var) {

  TotalCount = nrow(df)

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

  dagg <- df%>%
    dplyr::group_by(node)%>%
    tally()

  dagg <- dagg%>%
    dplyr::group_by(node)%>%
    dplyr::mutate(pct = n/TotalCount)

  # Step 3
  df <- merge(df, dagg, by.x = 'node', by.y = 'node', all.x = TRUE)

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
