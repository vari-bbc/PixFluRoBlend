#' Plot pixel, RFU, and combined curves
#'
#' @param result Output of [weighted_geomean()].
#' @param raw Optional raw data (as returned by [prepare_data()], filtered to
#'   one marker) to overlay as points.
#' @param title Optional plot title.
#' @param point_offset Added to raw `Signal` for plotting points only.
#' @return A ggplot object.
#' @export
plot_weighted_geomean <- function(result, raw = NULL, title = NULL,
                                  point_offset = 1) {
  long <- do.call(rbind, lapply(c("pixels", "RFU", "combined"), function(v) {
    data.frame(concentration = result$concentration,
               value = result[[v]], method = v)
  }))
  long$method <- factor(long$method, levels = c("pixels", "RFU", "combined"))

  cols <- c(pixels = pal_aaas()(2)[2], RFU = pal_aaas()(2)[1],
            combined = "black")

  p <- ggplot(long, aes(.data$concentration, .data$value,
                        color = .data$method)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = cols, name = NULL) +
    scale_x_continuous(transform = "log2") +
    scale_y_continuous(transform = "log2") +
    labs(x = "Concentration", y = "Signal", title = title) +
    theme_classic()

  if (!is.null(raw)) {
    pts <- data.frame(
      concentration = raw$Concentration,
      value = raw$Signal + point_offset,
      method = factor(as.character(raw$QuantMethod),
                      levels = levels(long$method))
    )
    p <- p + geom_point(data = pts, alpha = 0.25)
  }
  p
}
