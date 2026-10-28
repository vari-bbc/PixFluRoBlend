# PixFluRoBlend

Blend pixel-based and RFU-based quantification into one estimate using
monotone dose-response fits (`scam`) and a precision-weighted geometric mean.

## Install

```r
# install.packages("remotes")
remotes::install_github("vari-bbc/PixFluRoBlend")
```

## Use

```r
library(PixFluRoBlend)

dat <- prepare_data("example_data.xlsx")
out <- run_weighted_geomean("MUC16:sWFL", data = dat)

out$result      # weights + combined estimate
out$crossings   # detected crossing concentrations
out$plot        # ggplot object

# several markers
markers <- c("MUC16:sWFL", "AnotherMarker")
outs <- lapply(markers, run_weighted_geomean, data = dat)
names(outs) <- markers
```

`data` must have columns `Signal`, `QuantMethod` (`pixels` / `RFU`),
`Concentration`, `SampleType`, and `Marker`.
