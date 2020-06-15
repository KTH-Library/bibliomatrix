
#' Pie chart for Open Access data
#' 
#' This alternative pie chart can return three types of results - a ggplot object, a plotly object or a ggiraph svg object
#' 
#' @param df a data frame at the format produced by abm_oa_data()
#' @param type the return type, one of "ggplot", "plotly" or "ggiraph"
#' @return a ggplot object, ggiraph svg or plotly object as determined by type
#' @import ggplot2 dplyr tidyr ggiraph ktheme
#' @importFrom plotly plot_ly config layout
#' @export
abm_graph_oadata_piechart <- function(df, type = c("ggplot", "plotly", "ggiraph")) {

  unpaywall_cols <- c("#F9BC01", "#8D4EB4", "#20E168", "#CD7F32", "#BBBBBB")
  unpaywall_labels <- c("Gold", "Hybrid", "Green", "Bronze", "Not OA")
  unpaywall <- setNames(unpaywall_cols, unpaywall_labels)
  
  t1 <- 
    df %>% 
    filter(Publication_Year_ch == "Total") %>%
    select(-"oa_count" & ends_with("count")) %>% 
    pivot_longer(cols = ends_with("_count"), names_to = "group", names_pattern = "(.*?)_count", values_to = "count") %>%
    mutate(value = 100 * count / sum(count, na.rm = TRUE)) %>%
    mutate(group = tools::toTitleCase(group))  %>%
    mutate(group = dplyr::recode(group, Closed = "Not OA")) %>%
    mutate(color = dplyr::recode(group, !!!unpaywall)) %>%
    arrange(desc(value)) %>%
    mutate(ypos = sum(value) - cumsum(value) + 0.5 * value) %>%
    mutate(perc_text = paste0(sprintf("%2.1f%%", value)))

  t1$group <- ordered(t1$group, levels = names(unpaywall))

  if (type == "plotly") {
    pie <- 
      plot_ly(t1, 
        labels = ~group, values = ~value, type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~paste0(group),
        marker = list(colors = ~color,
          line = list(color = '#FFFFFF', width = 1)))  %>%
      config(displayModeBar = FALSE) %>% 
      layout(autosize = TRUE, showlegend = FALSE)
  }  else if (type == "ggplot") {
    pie <- 
      ggplot(t1, aes (x="", y = value, fill = factor(group), group = factor(group))) + 
      scale_fill_manual(values = unpaywall) +
      geom_col(position = 'stack', width = 1, color = "white") +
      geom_text(aes(label = perc_text, x = 1), color = "white",
                position = position_stack(vjust = 0.5)) +
      theme_void() +
      theme(
        legend.position = "right",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank() 
      ) +
      labs(fill = "",
           x = NULL,
           y = NULL) + 
      coord_polar("y", start = 0, direction = -1) +
      theme_kth_osc() + theme(
        axis.text.x = ggplot2::element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
      )
  }
  
  pie
  
}

#' Chart displaying shares of Open Access data publications
#' 
#' This graph is an alternative or complement to the abm_graph_oadata_piechart graph and can return three types of results - a ggplot object, a plotly object or a ggiraph svg object
#' 
#' Rationale: <https://www.perceptualedge.com/articles/visual_business_intelligence/save_the_pies_for_dessert.pdf>
#' 
#' @param df a data frame at the format produced by abm_oa_data()
#' @param type the return type, one of "ggplot", "plotly" or "ggiraph"
#' @return a ggplot object, ggiraph svg or plotly object as determined by type
#' @import ggplot2 dplyr tidyr ggiraph ktheme
#' @importFrom plotly plot_ly config layout ggplotly
#' @export
abm_graph_oadata_share <- function(df, type = c("ggplot", "plotly", "ggiraph")) {
  
  unpaywall_cols <- c("#F9BC01", "#8D4EB4", "#20E168", "#CD7F32", "#BBBBBB")
  unpaywall_labels <- c("Gold", "Hybrid", "Green", "Bronze", "Not OA")
  unpaywall <- setNames(unpaywall_cols, unpaywall_labels)
  
  t1 <- 
    df %>% 
    filter(Publication_Year_ch == "Total") %>%
    select(-"oa_count" & ends_with("count")) %>%
    pivot_longer(cols = ends_with("_count"), names_to = "group", names_pattern = "(.*?)_count", values_to = "count") %>%
    mutate(value = 100 * count / sum(count, na.rm = TRUE)) %>%
    mutate(group = tools::toTitleCase(group))  %>%
    mutate(group = dplyr::recode(group, Closed = "Not OA")) %>%
    mutate(color = dplyr::recode(group, !!!unpaywall)) %>%
    arrange(desc(value)) %>%
    mutate(ypos = sum(value) - cumsum(value) + 0.5 * value) %>%
    mutate(perc_text = paste0(sprintf("%2.1f%%", value)))
  
  t1$group <- ordered(t1$group, levels = names(unpaywall))
  
  p1 <- 
    ggplot(t1, aes(x = reorder(group, -desc(value)), y = value, fill = factor(group), group = factor(group))) + 
    scale_fill_manual(values = unpaywall, guide = NULL) +
    geom_bar(stat = "identity") + 
    coord_flip() +
#      geom_col(position = 'stack', width = 1, color = "white") +
#    geom_text(aes(label = paste0("n = ", count)), color = "gray", hjust = -0.2) +
    geom_text(aes(label = paste0(perc_text, " (n = ", count, ")"), y = value),
              vjust = 1, hjust = -0.2, color = "black") +
    theme_kth_osc() +
    scale_y_continuous(limits = c(0, max(t1$value) * 1.2), breaks = c(0:5 * 10), labels = function(x) paste0(x, "%"), position = "right") +
    theme(
      legend.position = "right",
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank()
    ) +
    labs(fill = "",
         x = NULL,
         y = NULL) 
  
  if (type == "ggplot") {
    return (p1)
  } else if (type == "plotly") {
    p2 <- 
      p1 %>% 
      ggplotly(tooltip = "y") %>%
      config(displayModeBar = FALSE) %>% 
      layout(autosize = TRUE, showlegend = FALSE)
    return (p2)
  } else if (type == "ggiraph") {
    my_gg <- p1
    return (girafe(ggobj = p1))
  }
  
}

#' Plotly chart wrapping the abm_graph_oadata_stacked graph
#' 
#' @param df a data frame at the format produced by abm_oa_data()
#' @importFrom plotly plot_ly config layout ggplotly
#' @export
abm_graph_oadata_stackedarea_plotly <- function(df) {
  abm_graph_oadata_stackedarea(df) %>%
    ggplotly(tooltip = c("fill", "y")) %>%
    config(displayModeBar = FALSE) %>% 
    layout(autosize = TRUE, showlegend = FALSE) 
}

#' Chart displaying the development of Open Access data publications over time
#' 
#' This graph is an alternative or complement to the abm_graph_oadata_stackedarea graph and can return three types of results - a ggplot object, a plotly object or a ggiraph svg object
#' 
#' Rationale: \url{https://www.perceptualedge.com/articles/visual_business_intelligence/displays_for_combining_time-series_and_part-to-whole.pdf} and 
#' \url{https://everydayanalytics.ca/2014/08/stacked-area-graphs-are-not-your-friend.html}
#' @param df a data frame at the format produced by abm_oa_data()
#' @param type the return type, one of "ggplot", "plotly" or "ggiraph"
#' @return a ggplot object, ggiraph svg or plotly object as determined by type
#' @import ggplot2 dplyr tidyr ggiraph ktheme
#' @importFrom plotly plot_ly config layout ggplotly
#' @export
abm_graph_oadata_linegraphs <- function(df, type = c("ggplot", "plotly", "ggiraph")) {

  unpaywall_cols <- c("#F9BC01", "#8D4EB4", "#20E168", "#CD7F32", "#BBBBBB")
  unpaywall_labels <- c("Gold", "Hybrid", "Green", "Bronze", "Not OA")
  unpaywall <- setNames(unpaywall_cols, unpaywall_labels)
  
  t11 <- 
    df %>% 
    rename(Year = "Publication_Year_ch", total_count = P_tot) %>%
    filter(Year != "Total") %>%
    tidyr::pivot_longer(
      cols = ends_with("_count"), 
      names_to = "series", 
      names_pattern = "(.*?)_count", 
      values_to = "Publication count"
    ) %>%
    mutate(series = tools::toTitleCase(series)) %>%
    mutate(series = recode(series, Oa = "Total OA", Total = "Grand Total", Closed = "Total Non OA")) %>%
    mutate(is_oa = ifelse(series %in% c("Gold", "Green", "Hybrid", "Bronze"), "Type of OA", "Totals"))
  
  t11$series <- factor(t11$series, levels = rev(c(names(unpaywall), "Total Non OA", "Total OA", "Grand Total")))
  
  p1 <- 
    ggplot(t11, aes(x = Year, y = `Publication count`, color = factor(series), group = factor(series))) +
    geom_line(linetype = "dashed") +
    geom_point() +
    scale_color_manual(values = c(unpaywall, "Total OA" = unname(ktheme::palette_kth()["blue"]), "Total Non OA" = unname(ktheme::palette_kth()["cerise"]), "Grand Total" = "#000000")) + 
    facet_wrap(c("is_oa"), ncol = 1, scales = "free") +
    labs(xlab = NULL, ylab = NULL, color = "Series") +
    ktheme::theme_kth_osc() + 
    theme(panel.spacing = unit(0, "lines"))
  
  if (type == "ggplot") {
    return (p1)
  } else if (type == "plotly") {
    p2 <- 
      p1 %>% ggplotly(tooltip = "y") %>%
      config(displayModeBar = FALSE) %>% 
      layout(autosize = TRUE)
    return (p2)
  } else if (type == "ggiraph") {
    ggiraph::girafe(ggobj = p1)
  }

}

