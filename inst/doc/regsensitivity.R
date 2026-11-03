## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6, fig.height = 4
)

## ----model-fig, echo = FALSE, out.width = "100%", fig.alt = "A diagram of the model. Boxes mark the observed variables W1, X and Y; a dashed circle marks the unobserved W2. Solid arrows pi-1 and gamma-1 run from W1 into X and Y, dashed arrows pi-2 and gamma-2 run from W2 into X and Y, and a heavy arrow labelled beta-long runs from X to Y. A dotted double-headed arrow between W1 and W2 is labelled association left unrestricted."----
knitr::include_graphics("figures/model.png")

## -----------------------------------------------------------------------------
library(regsensitivity)
library(ggplot2)
data(bfg2020)
bfg2020$statea <- factor(bfg2020$statea)

form <- avgrep2000to2016 ~ tye_tfe890_500kNI_100_l6 +
    log_area_2010 + lat + lon + temp_mean + rain_mean + elev_mean +
    d_coa + d_riv + d_lak + ave_gyi + statea
compare <- c("log_area_2010", "lat", "lon", "temp_mean", "rain_mean",
             "elev_mean", "d_coa", "d_riv", "d_lak", "ave_gyi")

## ----compare-fig, echo = FALSE, out.width = "100%", fig.alt = "A regsen_summary call with braces under its parts: y is the outcome Y, x is the treatment X, the terms named by compare are the calibration covariates W1, and the remaining right-hand-side terms are the control covariates W0, which are partialled out. A dashed circle stands for the unobserved W2, which appears in no formula."----
knitr::include_graphics("figures/covariates.png")

## ----sens-fig, echo = FALSE, out.width = "100%", fig.alt = "The same model diagram, with the dashed arrow from W2 into X tagged A3, the dashed arrow from W2 into Y tagged A5, and the dotted W1-W2 edge tagged A6. A key gives r-X as the ratio of the standard deviations of the two indices entering X, bounded by rxbar; r-Y likewise for Y, bounded by rybar; and the partial correlation of W2 with W1 given W0 lying in the interval from clow to cbar."----
knitr::include_graphics("figures/sensparams.png")

## -----------------------------------------------------------------------------
short <- lm(avgrep2000to2016 ~ tye_tfe890_500kNI_100_l6, data = bfg2020)
med   <- lm(form, data = bfg2020)
coef(short)[2]
coef(med)["tye_tfe890_500kNI_100_l6"]

## -----------------------------------------------------------------------------
bnds <- regsen_bounds(form, bfg2020, compare = compare, cbar = 0.1)
print(bnds)

## -----------------------------------------------------------------------------
plot(bnds)

## -----------------------------------------------------------------------------
multi <- regsen_bounds(form, bfg2020, compare = compare,
                        rxbar = seq(0, 2, length.out = 21),
                        cbar = c(0, 0.2, 0.4, 0.6, 0.8, 1.0))
plot(multi)

## -----------------------------------------------------------------------------
fin <- regsen_bounds(form, bfg2020, compare = compare, rybar = 2)
print(fin)

## -----------------------------------------------------------------------------
expr <- regsen_bounds(form, bfg2020, compare = compare,
                       rybar_expr = function(rx) rx,
                       rxbar = seq(0, 1, 0.1))
identified_set(expr)

## -----------------------------------------------------------------------------
bd <- regsen_breakdown(form, bfg2020, compare = compare,
                        cbar = seq(0, 1, 0.05))
plot(bd)

## -----------------------------------------------------------------------------
hyp <- regsen_breakdown(form, bfg2020, compare = compare,
                         beta = bnd_lb(seq(-1, 1, 0.2)))
print(hyp)

## -----------------------------------------------------------------------------
fr <- regsen_breakdown(form, bfg2020, compare = compare, cbar = 1,
                        direction = "rybar",
                        rxbar = c(0.5, 1, 2, 4))
breakdown_frontier(fr)

## -----------------------------------------------------------------------------
vapply(c(0, 0.9, 0.99), function(cl) {
    breakdown_point(regsen_breakdown(form, bfg2020, compare = compare,
                                     cbar = 1, clow = cl))
}, numeric(1))

## -----------------------------------------------------------------------------
o <- regsen_bounds(form, bfg2020, compare = compare,
                    analysis = "oster")
print(o)

## -----------------------------------------------------------------------------
o2 <- regsen_bounds(form, bfg2020, compare = compare,
                     analysis = "oster",
                     delta = seq(-3, 3, 0.05))
plot(o2, ylim = c(-5, 8))

## -----------------------------------------------------------------------------
ob <- regsen_bounds(form, bfg2020, compare = compare,
                     analysis = "oster",
                     delta = seq(0, 0.999, 0.001),
                     delta_type = "bound")
plot(ob, ylim = c(-50, 50))

## -----------------------------------------------------------------------------
obd_eq <- regsen_breakdown(form, bfg2020, compare = compare,
                            analysis = "oster",
                            r2long = seq(0.15, 1, 0.05),
                            beta = bnd_eq(0))
plot(obd_eq)

obd_sign <- regsen_breakdown(form, bfg2020, compare = compare,
                              analysis = "oster",
                              r2long = seq(0.15, 1, 0.05))
plot(obd_sign)

## ----cores, eval = FALSE------------------------------------------------------
# regsen_cores(2)        # a number of cores

## -----------------------------------------------------------------------------
s <- regsen_summary(form, bfg2020, compare = compare)
print(s)

