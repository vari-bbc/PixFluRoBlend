#' Read and clean quantification data
#'
#' Reads an Excel file (or takes a data frame), coerces `Signal` to numeric,
#' drops missing values, and replaces zero signals with
#' `min(nonzero signal) / 10`. The minimum is computed across the whole
#' dataset, not per marker.
#'
#' @param data Path to an `.xlsx` file, or a data frame.
#' @return A data frame with `QuantMethod` as a factor (`pixels`, `RFU`).
#'   Must contain columns `Signal`, `QuantMethod`, `Concentration`,
#'   `SampleType`, and `Marker`.
#' @export
prepare_data <- function(data) {
  if (is.character(data)) data <- read_excel(data)
  data <- as.data.frame(data)

  needed <- c("Signal", "QuantMethod", "Concentration", "SampleType", "Marker")
  missing_cols <- setdiff(needed, names(data))
  if (length(missing_cols)) {
    stop("Data is missing column(s): ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  data$Signal <- as.numeric(data$Signal)
  data$QuantMethod <- factor(data$QuantMethod, levels = c("pixels", "RFU"))
  data <- data[!is.na(data$Signal), ]
  data$Signal[data$Signal == 0] <-
    min(data$Signal[data$Signal != 0], na.rm = TRUE) / 10
  data
}
