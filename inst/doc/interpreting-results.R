## ----setup, include = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6, fig.height = 4
)
# Wider output for the tables below. Put back at the end of the vignette,
# so building it leaves the reader's options as it found them.
old_options <- options(width = 90)
library(regsensitivity)
library(ggplot2)

## ----world------------------------------------------------------------------------------
# A world with six observed covariates w1..w6 and one omitted variable u.
# `beta` is the true effect of x; pi2 and g2 are u's effects on x and y.
# The seed is set by the caller rather than inside here, so that calling
# this does not reach into the session's random stream behind your back.
make_world <- function(beta, pi2, g2, pi1 = rep(0.6, 6), sy = 1,
                       n = 2000) {
    w <- matrix(rnorm(n * 6), n, 6, dimnames = list(NULL, paste0("w", 1:6)))
    u <- 0.3 * w[, 1] + rnorm(n)
    x <- w %*% pi1 + pi2 * u + rnorm(n)
    y <- beta * x + w %*% rep(c(0.4, -0.3), 3) + g2 * u + rnorm(n) * sy
    d <- data.frame(y = as.numeric(y), x = as.numeric(x), w)
    d$u <- u          # kept only so we can check the truth; never used
    d
}
w1   <- paste0("w", 1:6)
form <- y ~ x + w1 + w2 + w3 + w4 + w5 + w6
truth <- function(d) unname(coef(lm(update(form, . ~ . + u), d))["x"])

## ----robust-----------------------------------------------------------------------------
set.seed(11)
d <- make_world(beta = 1, pi2 = 0.2, g2 = 0.3, sy = 0.6)
res <- regsen_bounds(form, d, compare = w1, cbar = 1)
rho <- calibrate_rho(form, d, compare = w1)
res$breakdown
rho

## ----robust-plot------------------------------------------------------------------------
plot(res, xline = res$breakdown)

## ----fragile----------------------------------------------------------------------------
set.seed(12)
d <- make_world(beta = 0, pi2 = 0.8, g2 = 0.8)
res <- regsen_bounds(form, d, compare = w1, cbar = 1)
rho <- calibrate_rho(form, d, compare = w1)
c(beta_med = res$dgp$beta_med, breakdown = res$breakdown)
range(rho$rho) / 100

## ----fragile-plot-----------------------------------------------------------------------
plot(res, xline = res$breakdown)

## ----zero-------------------------------------------------------------------------------
set.seed(11)
d <- make_world(beta = 1, pi2 = 0.2, g2 = 0.3, sy = 0.6)
regsen_breakdown(form, d, compare = w1, beta = bnd_lb(2))$results

## ----weak-------------------------------------------------------------------------------
set.seed(13)
d <- make_world(beta = 1, pi2 = 0.5, g2 = 0.5, pi1 = rep(0.08, 6))
res <- regsen_bounds(form, d, compare = w1, cbar = 1)
r2 <- res$dgp$covwx_norm_sq / res$dgp$var_x        # R2 of X on W1
c(R2_x_on_w1 = r2, rmax = sqrt(1 - r2), breakdown = res$breakdown)
calibrate_rho(form, d, compare = w1)

## ----bfg-setup--------------------------------------------------------------------------
data(bfg2020)
bfg2020$statea <- factor(bfg2020$statea)
geo <- c("log_area_2010", "lat", "lon", "temp_mean", "rain_mean",
         "elev_mean", "d_coa", "d_riv", "d_lak", "ave_gyi")
bform <- reformulate(c("tye_tfe890_500kNI_100_l6", geo, "statea"),
                     response = "avgrep2000to2016")

## ----bfg-compare------------------------------------------------------------------------
bp_geo <- regsen_bounds(bform, bfg2020, compare = geo, cbar = 1)$breakdown
bp_all <- regsen_bounds(bform, bfg2020, cbar = 1)$breakdown  # FE in W1 too
c(geography_only = bp_geo, geography_and_state_FE = bp_all)

## ----cbar-------------------------------------------------------------------------------
set.seed(11)
d <- make_world(beta = 1, pi2 = 0.2, g2 = 0.3, sy = 0.6)
regsen_breakdown(form, d, compare = w1, cbar = c(0, 0.25, 0.5, 0.75, 1))$results

## ----rybar------------------------------------------------------------------------------
set.seed(11)
d <- make_world(beta = 1, pi2 = 0.2, g2 = 0.3, sy = 0.6)
sapply(c(Inf, 2, 1, 0.5), function(ry) {
    regsen_breakdown(form, d, compare = w1, cbar = 1, rybar = ry)$results$breakdown
})

## ----frontier---------------------------------------------------------------------------
fr <- regsen_breakdown(form, d, compare = w1, cbar = 1,
                       direction = "rybar", rxbar = c(0.25, 0.5, 1, 2))
fr$results

## ----oster------------------------------------------------------------------------------
set.seed(12)
d <- make_world(beta = 0, pi2 = 0.8, g2 = 0.8)
d_eq   <- regsen_breakdown(form, d, compare = w1, analysis = "oster",
                           r2long = 1, beta = bnd_eq(0))$results$breakdown
d_sign <- regsen_breakdown(form, d, compare = w1, analysis = "oster",
                           r2long = 1)$results$breakdown
c(explain_away = d_eq, sign_change = d_sign)

## ----oster-plot-------------------------------------------------------------------------
o <- regsen_bounds(form, d, compare = w1, analysis = "oster",
                   delta = seq(-2, 2, 0.02), r2long = 1)
plot(o, ylim = c(-3, 3))

## ----boot-------------------------------------------------------------------------------
bb <- regsen_boot(bform, bfg2020, compare = geo, cbar = 1,
                  R = 199, cluster = "km_grid_cel_code", seed = 1,
                  show_progress = FALSE)
bb

## ----multi------------------------------------------------------------------------------
regsen_multi(bform, bfg2020,
             treatments = c("tye_tfe890_500kNI_100_l6", "lat", "temp_mean"),
             compare = geo, cbar = 1)

## ----cleanup, include = FALSE-------------------------------------------------
options(old_options)

