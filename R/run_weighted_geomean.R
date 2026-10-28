#' Run the full pipeline for one marker
#'
#' Filters to a marker and sample type, fits monotone curves per
#' quantification method, computes the weighted geometric mean, and plots.
#'
#' @param marker Marker name, e.g. `"MUC16:sWFL"`.
#' @param data A data frame from [prepare_data()] (preferred when looping over
#'   markers), or a path to an `.xlsx` file.
#' @param sample_type `SampleType` value to analyse.
#' @inheritParams weighted_geomean
#' @param point_offset Added to raw `Signal` when plotting points.
#' @param plot Print the plot?
#' @param save_plot Optional file path to save the plot (via [ggplot2::ggsave()]).
#' @return Invisibly, a list with `result`, `plot`, `model`, `curves`,
#'   `crossings`, and the filtered `data`.
#' @examples
#' \dontrun{
#' dat <- prepare_data("example_data.xlsx")
#' out <- run_weighted_geomean("MUC16:sWFL", data = dat)
#' out$result
#' }
#' @export
run_weighted_geomean <- function(
    marker,
    data,
    sample_type = "CFPAC1_CM_conc",
    crossings = NULL,
    low_pixels_weight = 0.95,
    high_pixels_weight = 0.05,
    sd_power = 2,
    transition_width = 1,
    point_offset = 1,
    plot = TRUE,
    save_plot = NULL
) {
  data <- prepare_data(data)

  sub <- data[data$SampleType == sample_type & data$Marker == marker, ]
  if (nrow(sub) == 0L) {
    stop("No rows for marker '", marker, "' and sample type '", sample_type,
         "'.\nAvailable markers: ",
         paste(sort(unique(data$Marker)), collapse = ", "), call. = FALSE)
  }

  fc <- fit_curves(sub)
  dat <- fc$curves

  result <- weighted_geomean(
    concentration = dat$concentration,
    pixels = dat$pixels,
    RFU = dat$RFU,
    sd_pixels = dat$sd_pixels,
    sd_RFU = dat$sd_RFU,
    crossings = crossings,
    low_pixels_weight = low_pixels_weight,
    high_pixels_weight = high_pixels_weight,
    sd_power = sd_power,
    transition_width = transition_width
  )

  cr <- attr(result, "crossings")
  message("Crossings for ", marker, ": ",
          if (length(cr)) paste(signif(cr, 4), collapse = ", ") else "none")

  p <- plot_weighted_geomean(result, raw = sub, title = marker,
                             point_offset = point_offset)
  if (!is.null(save_plot)) ggsave(save_plot, p, width = 6, height = 4)
  if (plot) print(p)

  invisible(list(result = result, plot = p, model = fc$fit, curves = dat,
                 crossings = cr, data = sub))
}
