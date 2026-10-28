# Internal: fit monotone-increasing smooths per quantification method and
# return predictions and SDs (log2 scale) at each observed concentration.
fit_curves <- function(df) {
  fit <- scam(
    log2(Signal) ~ QuantMethod +
      s(log2(Concentration), by = QuantMethod, bs = "mpi"),
    data = df
  )

  grid <- expand.grid(
    QuantMethod = factor(c("pixels", "RFU"), levels = c("pixels", "RFU")),
    Concentration = sort(unique(df$Concentration))
  )
  p <- predict(fit, newdata = grid, se.fit = TRUE)
  grid$pred <- 2^as.numeric(p$fit)
  grid$SD <- sqrt(as.numeric(p$se.fit)^2 + fit$sig2)

  px  <- grid[grid$QuantMethod == "pixels", ]
  rfu <- grid[grid$QuantMethod == "RFU", ]
  dat <- merge(
    data.frame(concentration = px$Concentration,  pixels = px$pred,
               sd_pixels = px$SD),
    data.frame(concentration = rfu$Concentration, RFU = rfu$pred,
               sd_RFU = rfu$SD),
    by = "concentration"
  )
  list(fit = fit, curves = dat)
}
