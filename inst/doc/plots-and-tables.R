## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4,
  dpi = 150
)
library(regsensitivity)
library(ggplot2)

## ----data---------------------------------------------------------------------
data(bfg2020)
bfg2020$statea <- factor(bfg2020$statea)
w1 <- c("log_area_2010", "lat", "lon", "temp_mean", "rain_mean",
        "elev_mean", "d_coa", "d_riv", "d_lak", "ave_gyi")
form <- avgrep2000to2016 ~ tye_tfe890_500kNI_100_l6 +
    log_area_2010 + lat + lon + temp_mean + rain_mean + elev_mean +
    d_coa + d_riv + d_lak + ave_gyi + statea

res <- regsen_bounds(form, bfg2020, compare = w1,
                      cbar = c(0.1, 0.5, 1))

## ----basic--------------------------------------------------------------------
plot(res)

## ----unbounded----------------------------------------------------------------
plot(res, ylim = c(-10, 14))

## ----xline--------------------------------------------------------------------
plot(res, xline = c(0.5, 1))

## ----sizing, fig.width = 3.3, fig.height = 2.6--------------------------------
plot(res, base_size = 9, show_legend = FALSE)

## ----font, eval = FALSE-------------------------------------------------------
# plot(res) + theme_regsen(base_family = "Times New Roman")

## ----greyscale----------------------------------------------------------------
plot(res) + scale_colour_grey(start = 0, end = 0.6)

## ----override-----------------------------------------------------------------
plot(res, subtitle = NA) +
    theme_regsen(grid = "y", base_size = 10) +
    labs(caption = "BFG (2020); bounds under DMP (2026).")

## ----asdf---------------------------------------------------------------------
head(as.data.frame(res, digits = 3))

## ----table-md-----------------------------------------------------------------
regsen_table(res, format = "markdown", digits = 3, max_rows = 6)

## ----table-tex, results = "asis"----------------------------------------------
cat(regsen_table(res, format = "latex", digits = 3, max_rows = 4,
                  caption = "Identified sets for the long-regression coefficient",
                  label = "idset", notes = TRUE, escape = FALSE),
    sep = "\n")

## ----infinite-----------------------------------------------------------------
regsen_table(res, format = "markdown", digits = 3, max_rows = 8)

## ----tidy---------------------------------------------------------------------
head(regsen_tidy(res), 4)
regsen_glance(res)

