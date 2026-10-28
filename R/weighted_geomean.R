#' Weighted geometric mean of pixel and RFU estimates
#'
#' @param concentration,pixels,RFU Positive numeric vectors, one value per
#'   unique concentration.
#' @param sd_pixels,sd_RFU Positive SDs (length 1 or matching the data).
#' @param crossings Optional concentrations (max 2) where the curves cross.
#'   If `NULL` they are detected by interpolation.
#' @param low_pixels_weight,high_pixels_weight Weight on pixels well below /
#'   above the crossings.
#' @param sd_power Exponent on the SD ratio; 2 gives inverse-variance weights.
#' @param transition_width Width (log2 units) of the taper around a crossing.
#' @return A data frame with the weights and `combined` estimate; the
#'   crossings are in `attr(, "crossings")`.
#' @export
weighted_geomean <- function(
    concentration, pixels, RFU, sd_pixels, sd_RFU,
    crossings = NULL,
    low_pixels_weight = 0.95,
    high_pixels_weight = 0.05,
    sd_power = 2,
    transition_width = 1
) {
  n <- length(concentration)
  if (n < 2L || length(pixels) != n || length(RFU) != n) {
    stop("Supply at least two concentrations and matching pixels/RFU values.")
  }

  expand_sd <- function(z) {
    if (length(z) == 1L) z <- rep(z, n)
    if (length(z) != n) stop("SDs must have length 1 or match the data.")
    z
  }
  sd_pixels <- expand_sd(sd_pixels)
  sd_RFU <- expand_sd(sd_RFU)

  values <- c(concentration, pixels, RFU, sd_pixels, sd_RFU)
  if (any(!is.finite(values)) || any(values <= 0)) {
    stop("Concentrations, responses, and SDs must be finite and positive.")
  }

  settings <- list(low_pixels_weight, high_pixels_weight,
                   sd_power, transition_width)
  if (any(lengths(settings) != 1L)) {
    stop("Each weighting setting must be a single number.")
  }
  if (any(!is.finite(unlist(settings))) ||
      any(c(low_pixels_weight, high_pixels_weight) < 0) ||
      any(c(low_pixels_weight, high_pixels_weight) > 1) ||
      sd_power < 0 || transition_width <= 0) {
    stop("Weights must be in [0, 1], sd_power >= 0, and width > 0.")
  }

  ord <- order(concentration)
  concentration <- concentration[ord]
  pixels <- pixels[ord]
  RFU <- RFU[ord]
  sd_pixels <- sd_pixels[ord]
  sd_RFU <- sd_RFU[ord]
  x <- log2(concentration)

  if (anyDuplicated(x)) {
    stop("Supply one response and SD per unique concentration.")
  }

  # Interpolate log-response differences against log2 concentration.
  if (is.null(crossings)) {
    difference <- log(pixels) - log(RFU)
    left <- head(difference, -1)
    right <- tail(difference, -1)
    i <- which((left < 0 & right > 0) | (left > 0 & right < 0))
    interpolated <- x[i] - difference[i] * (x[i + 1L] - x[i]) /
      (difference[i + 1L] - difference[i])
    crossing_x <- sort(unique(c(x[difference == 0], interpolated)))
  } else {
    if (any(!is.finite(crossings)) || any(crossings <= 0)) {
      stop("Supplied crossings must be finite and positive.")
    }
    crossing_x <- sort(unique(log2(crossings)))
  }

  if (length(crossing_x) > 2L) {
    stop("More than two crossings detected; supply the intended crossings.")
  }

  precision_weight <- plogis(sd_power * (log(sd_RFU) - log(sd_pixels)))
  weight_pixels <- rep(low_pixels_weight, n)

  # With no crossing, use the low-concentration weight throughout.
  if (length(crossing_x) > 0L) {
    first <- crossing_x[1]
    second <- if (length(crossing_x) == 2L) crossing_x[2] else Inf
    before <- x < first
    after <- x > second
    middle <- !(before | after)

    distance <- pmin(abs(x - first), abs(x - second))
    taper <- 1 - exp(-(distance / transition_width)^2)

    weight_pixels[before] <- 0.5 + taper[before] * (low_pixels_weight - 0.5)
    weight_pixels[middle] <- 0.5 + taper[middle] *
      (precision_weight[middle] - 0.5)
    weight_pixels[after] <- 0.5 + taper[after] * (high_pixels_weight - 0.5)
  }

  result <- data.frame(
    concentration, pixels, RFU,
    weight_pixels,
    weight_RFU = 1 - weight_pixels,
    combined = exp(weight_pixels * log(pixels) +
                     (1 - weight_pixels) * log(RFU))
  )
  attr(result, "crossings") <- 2^crossing_x
  result
}
