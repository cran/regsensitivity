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

## ----compare-map, echo = FALSE, out.width = "100%", fig.alt = "A regsen_summary call with braces under its parts: y is the outcome Y, x is the treatment X, the terms named by compare are the calibration covariates W1, and the remaining right-hand-side terms are the control covariates W0, which are partialled out. A dashed circle stands for the unobserved W2, which appears in no formula."----
knitr::include_graphics("figures/covariates.png")

## ----summary------------------------------------------------------------------
summ <- regsen_summary(form, bfg2020, compare = w1)
summ

## ----bounds-one---------------------------------------------------------------
b1 <- regsen_bounds(form, bfg2020, compare = w1, cbar = 0.1)
plot(b1)

## ----bounds-grid--------------------------------------------------------------
b2 <- regsen_bounds(form, bfg2020, compare = w1,
                     cbar = seq(0, 1, 0.2))
plot(b2)

## ----bounds-rxbar-------------------------------------------------------------
b3 <- regsen_bounds(form, bfg2020, compare = w1,
                     cbar = seq(0, 1, 0.2), rxbar = c(0, 2))
plot(b3)

## ----breakdown----------------------------------------------------------------
bd <- regsen_breakdown(form, bfg2020, compare = w1,
                        cbar = seq(0, 1, 0.1))
plot(bd)

## ----breakdown-ub-------------------------------------------------------------
bd_ub <- regsen_breakdown(form, bfg2020, compare = w1,
                           cbar = seq(0, 1, 0.1), beta = bnd_ub(4))
bd_ub

## ----rybar--------------------------------------------------------------------
b_ry  <- regsen_bounds(form, bfg2020, compare = w1, rybar = 2)
b_eq  <- regsen_bounds(form, bfg2020, compare = w1,
                        rybar_expr = function(rx) rx)
c(rybar_2 = b_ry$breakdown, rybar_eq = b_eq$breakdown)

## ----oster--------------------------------------------------------------------
os <- regsen_bounds(form, bfg2020, compare = w1,
                     analysis = "oster",
                     delta = seq(-3, 3, 0.05), delta_type = "eq")
plot(os, ylim = c(-5, 8), xline = 1)

## ----oster-breakdown----------------------------------------------------------
os_bd <- regsen_breakdown(form, bfg2020, compare = w1,
                           analysis = "oster",
                           r2long = seq(0, 1, 0.1),
                           beta = bnd_eq(0))
os_sign <- regsen_breakdown(form, bfg2020, compare = w1,
                             analysis = "oster",
                             r2long = seq(0, 1, 0.1),
                             beta = "sign")
c(explain_away = os_bd$results$breakdown[1],
  sign_change  = os_sign$results$breakdown[1])
plot(os_sign)

## ----direction----------------------------------------------------------------
regsen_breakdown(form, bfg2020, compare = w1, cbar = 1,
                  direction = "rybar",
                  rxbar = c(0.5, 1, 2, 4))$results

## ----clow---------------------------------------------------------------------
regsen_bounds(form, bfg2020, compare = w1, cbar = 1, clow = 0.99,
               rxbar = c(0.5, 0.9))$results

## ----region-------------------------------------------------------------------
regsen_bounds(form, bfg2020, compare = w1, cbar = 1,
               rxbar = 2, rybar = 0.5)$results

