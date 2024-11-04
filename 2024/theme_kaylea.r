theme_kaylea <- function(family = "courier",
                               base_size = 18,
                               hjust = 0.5,
                               text_col = "black",
                               bg_col = "white") {
  ggplot2::theme_minimal(
    base_family = family,
    base_size = base_size
  ) +
    ggplot2::theme(
      # title
      plot.title = ggplot2::element_text(
        face = "bold",
        hjust = hjust,
        colour = text_col, 
        size = rel(1.5), 
        margin = margin(b = 5)
      ),
      plot.title.position = "plot",
      # subtitle
      plot.subtitle = ggplot2::element_text(
        hjust = hjust,
        colour = text_col, 
        margin = margin(t = 5)
      ),
      # caption
      plot.caption = ggplot2::element_text(
        hjust = 1,
        colour = text_col, 
        margin = margin(t = 5)
      ),
      plot.caption.position = "plot",
      
      axis.text = ggplot2::element_text(
        colour = text_col
      ),
      # plot
      plot.margin = ggplot2::margin(5, 5, 5, 5),
      plot.background = ggplot2::element_rect(
        fill = bg_col,
        colour = bg_col
      ),
      panel.background = ggplot2::element_rect(
        fill = bg_col,
        colour = bg_col
      ),
      # legend
      legend.position = "bottom",
      legend.text = ggplot2::element_text(
        colour = text_col
      ),
      legend.title = ggplot2::element_text(
        colour = text_col
      ),
      legend.background = ggplot2::element_rect(
        fill = "transparent",
        colour = "transparent"
      ),
      legend.key = ggplot2::element_rect(
        fill = "transparent",
        colour = "transparent"
      ),
      # facets
      strip.background = ggplot2::element_rect(
        fill = "transparent",
        colour = "transparent"
      ),
      strip.text = ggplot2::element_text(
        colour = text_col
      )
    )
}
