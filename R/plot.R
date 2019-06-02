#' plot-decomp
#' @param x a decomp data.frame
#' @param base_size base size for ggplot theme
#' @export
plot.decomp = function(x, base_size = 12){

  if (exists("category", x)){
    category_col = "category"
  } else {
    category_col = "variable"
  }

  if (is.character(x[["id"]]) || is.factor(x[["id"]])) {
    plot_func = function(data, x, y, group, size) {
      ezplot::bar_plot(data, x, y, group, size = base_size, label_cutoff = 1)
    }
    x_discrete = TRUE
    id_levels = as.character(unique(x[["id"]]))
    x[["id"]] = as.numeric(factor(x[["id"]], id_levels))
  } else {
    plot_func = ezplot::area_plot
    x_discrete = FALSE
  }

  g = plot_func(x,
                "id", "value",
                category_col,
                size = base_size)

  if (x_discrete) {
    g = suppressMessages(
      g +
        ggplot2::scale_x_continuous(
          breaks = seq_along(id_levels),
          labels = id_levels,
          expand = c(0, 0.6)
        )
    ) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90,
                                            vjust = 0.38,
                                            hjust = 1)
      )
  }

  g

}
