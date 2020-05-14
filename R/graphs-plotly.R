#' Create alternate bullet graph with reference line (using ggplot conversion to plotly)
#'
#' @param label a label for the indicator, shown to the left of the gauge
#' @param value the value of the indicator, displayed as a horizontal wide line
#' @param reference a reference value displayed as a vertical thin line
#' @param roundto number of digits after the decimal point (default = 1)
#' @param pct boolean, set to TRUE if given value is a share (default = FALSE)
#' @return a ggplot object
#' @import ggplot2 ktheme
#' @importFrom plotly add_trace config layout ggplotly
#' @export
abm_bullet_plotly <- function(label, value, reference, roundto = 1, pct = FALSE){
  
  p1 <- 
    abm_bullet(label, value, reference, roundto, pct) +
    theme_kth() + 
    # override some theme settings
    theme(
      plot.title=element_text(size = 12),
      axis.text.x=element_text(size = 8),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.line.y=element_blank(),
      axis.line.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.x=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank()
    ) +
    labs(title = "")
  
  accent <- unname(ktheme::palette_kth(10, type = "qual")["cerise"])
  #accent <- bibliomatrix::palette_kth()["cerise"]
  
  a <- list(
    text = "Title",
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "left",
    align = "left",
    x = 0.04,
    y = 0.85,
    showarrow = FALSE
  )
  
  if (pct) {
    value <- 100 * value
    reference <- 100 * reference
  }
  
  value <- round(value, roundto)
  
  title <- sprintf(paste0("%s = %.", roundto, "f%s"), 
                   label, value, ifelse(pct, "%", ""))
  
  a$text <- title
  
  
  # a few issues with plotly and geom_errorbar etc
  # https://github.com/ropensci/plotly/issues/1751
  # https://community.rstudio.com/t/plotly-does-not-display-in-flexdashboard-page/32696/3
  
  p1 %>%
    ggplotly()  %>%
    #    add_trace(x = reference, y = 1.8, color = I(tolower(accent)), 
    #      width = 0.4, mode = "lines") %>% 
    config(displayModeBar = FALSE) %>% 
    layout(autosize = TRUE, showlegend = FALSE, 
           margin = list(l = 10, r = 10, b = 15, t = 30, pad = 4)) %>%
    layout(annotations = a)
}

#' Create alternate bullet graph with reference line (using plotly directly)
#'
#' @param caption a label for the indicator, shown to the left of the gauge
#' @param value the value of the indicator, displayed as a horizontal wide line
#' @param reference a reference value displayed as a vertical thin line
#' @param span number of digits after the decimal point (default = 1)
#' @return a plotly object
#' @import ggplot2 ktheme
#' @importFrom plotly add_trace config layout ggplotly
#' @export
abm_bullet_plotly2 <- function(
  caption = "Field normalized citations (Cf) = 1.13",
  value = 1.13, reference = 1, span = c(0, 2)) 
{
  p <- plot_ly(
    type = "indicator",
    height = 100,
    width = 1000,
    mode = "gauge",
    value = value,
    domain = list(x = c(0, 1), y = c(0, 1)),
    #  delta = list(reference = 1, position = "top"),
    #  title = list(
    #    text = "<b>Profit</b><br><span style='color: gray; font-size:0.8em'>U.S. $</span>",
    #    font = list(size = 14)),
    gauge = list(
      shape = "bullet",
      bordercolor = "lightgray",
      axis = list(range = span),
      threshold = list(
        line = list(color = palette_kth()["cerise"], width = 5, gradient = list(yanchor = "vertical")),
        thickness = 0.75,
        value = reference),
      bgcolor = "lightgray", #palette_kth()["gray"],
      #   steps = list(list(range = c(0, 150), color = palette_kth()["gray"])),
      bar = list(color = "darkblue", thickness = 0.5))) %>% 
    config(displayModeBar = FALSE)
  
  m <- list(
    l = 10,
    r = 10,
    b = 20,
    t = 30,
    pad = 4
  )
  
  p
  #  p %>% layout(
  #    autosize = TRUE, showlegend = FALSE, 
  #margin = list(l = 10, r = 10, b = 20, t = 30, pad = 4), 
  #title = list(text = caption, xref = "paper", x = 0, xanchor = "left"), 
  #    modebar = list(orientation = "v"))
  
}

#' Create alternate waffle chart (with plotly)
#'
#' @param pct a percentage expressed as a decimal number 0 <= pct <= 1
#' @param col a vector with colors for filling (optional)
#' @param label a title for the chart, displayed above the waffle (optional)
#' @return a plotly object
#' @return a plotly object
#' @import ggplot2 ktheme
#' @importFrom plotly add_trace config layout ggplotly
#' @export
abm_waffle_pct_plotly <- function(pct, col = as.character(c(palette_kth()["blue"], "gray")), label = NULL) {
  p1 <- 
    abm_waffle_pct(pct, col, label) + 
    theme(panel.spacing = unit(0, "lines")) +
    labs(title = NULL)
  
  p1 %>% ggplotly() %>% 
    config(displayModeBar = FALSE) %>% 
    layout(autosize = TRUE, showlegend = FALSE, 
           margin = list(l = 10, r = 10, b = 15, t = 30, pad = 4)) %>%
    layout(annotations = list(text = label,
                              xref = "paper",
                              yref = "paper",
                              yanchor = "bottom",
                              xanchor = "left",
                              align = "left",
                              x = 0.0,
                              y = 0.85,
                              showarrow = FALSE))
  
}

#' Create graph over DiVA publication types by year, using plotly
#' 
#' @param df a data frame at the format produced by abm_table1()
#' @return a plotly object
#' @importFrom plotly ggplotly
#' @export
abm_graph_diva_plotly <- function(df)
  abm_graph_diva(df) %>% ggplotly()

#' Create graph over WoS coverage by year, using plotly
#' 
#' @param df a data frame at the format produced by abm_table1()
#' @return a plotly object
#' @importFrom plotly ggplotly
#' @export
abm_graph_wos_coverage_plotly <- function(df) {
  p1 <- abm_graph_wos_coverage(df_diva) #+ theme_kth_osc()
  p1 %>% ggplotly(tooltip = "text")
}

#' Create graph over DiVA publication types by year, using plotly
#' 
#' @param df a data frame at the format produced by abm_table1()
#' @return a plotly object
#' @importFrom plotly ggplotly
#' @export
abm_graph_cf_plotly <- function(df)
  abm_graph_cf(df) %>% ggplotly()


#' Create graph over Cf by year, using plotly
#' 
#' @param df a data frame at the format produced by abm_table3()
#' @return a plotly object
#' @importFrom plotly ggplotly
#' @export
abm_graph_top10_plotly <- function(df)
  abm_graph_top10(df) %>% ggplotly()

#' Create graph over Top 20\% journals by year, using plotly
#' 
#' @param df a data frame at the format produced by abm_table4()
#' @return a plotly object
#' @importFrom plotly ggplotly
#' @export
abm_graph_top20_plotly <- function(df)
  abm_graph_top20(df) %>% ggplotly()

#' Create graph over jcf by year, using plotly
#' 
#' @param df a data frame at the format produced by abm_table4()
#' @return a plotly object
#' @importFrom plotly ggplotly
#' @export
abm_graph_jcf_plotly <- function(df)
  abm_graph_jcf(df) %>% ggplotly()

#' Create graph over international and Swedish non-university copublications by year, using plotly
#' 
#' @param df a data frame at the format produced by abm_table4()
#' @return a plotly object
#' @importFrom plotly ggplotly
#' @export
abm_graph_copub_plotly <- function(df)
  abm_graph_copub(df) %>% ggplotly()
