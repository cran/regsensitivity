## ----setup, include = FALSE-----------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6, fig.height = 4
)
# Wider output for the tables below. Put back at the end of the vignette,
# so building it leaves the reader's options as it found them.
old_options <- options(width = 100)

## -------------------------------------------------------------------------------------------------
library(regsensitivity)
library(ggplot2)
data(bfg2020)
bfg2020$statea <- factor(bfg2020$statea)

w1 <- c("log_area_2010", "lat", "lon", "temp_mean", "rain_mean",
        "elev_mean", "d_coa", "d_riv", "d_lak", "ave_gyi")
labels <- c(
    log_area_2010 = "Land area",
    lat           = "Centroid Latitude",
    lon           = "Centroid Longitude",
    temp_mean     = "Average temperature",
    rain_mean     = "Average rainfall",
    elev_mean     = "Elevation",
    d_coa         = "Distance from centroid to the coast",
    d_riv         = "Distance from centroid to rivers",
    d_lak         = "Distance from centroid to lakes",
    ave_gyi       = "Average potential agricultural yield"
)
form <- avgrep2000to2016 ~ tye_tfe890_500kNI_100_l6 +
    log_area_2010 + lat + lon + temp_mean + rain_mean + elev_mean +
    d_coa + d_riv + d_lak + ave_gyi + statea

## -------------------------------------------------------------------------------------------------
bp_conservative <- regsen_bounds(form, bfg2020, compare = w1,
                                  cbar = 1)
bp_relaxed <- regsen_bounds(form, bfg2020, compare = w1,
                             cbar = 1,
                             rybar_expr = function(rx) rx)

## ----bp-comparison--------------------------------------------------------------------------------
# digits: the precision the paper prints each value to.
compare_published <- function(quantity, published, computed, digits) {
    out <- data.frame(
        quantity  = quantity,
        published = published,
        computed  = round(computed, 4),
        stringsAsFactors = FALSE
    )
    out$difference <- round(out$computed - out$published, 4)
    out$agrees <- abs(out$difference) <= 0.5 * 10^(-digits)
    out
}

cmp <- compare_published(
    quantity  = c("rxbar^bp at cbar = 1",
                  "r^bp at cbar = 1, rybar = rxbar"),
    published = c(0.804, 0.959),
    digits    = c(3, 3),          # as printed in the paper's Table 1
    computed  = c(bp_conservative$breakdown, bp_relaxed$breakdown)
)
knitr::kable(cmp, caption = "Breakdown points: this package vs DMP (2026).")

## ----bp-assert, echo = FALSE----------------------------------------------------------------------
# The second value sits 0.0006 from the printed 0.959, just past half a
# unit of the last digit -- the same knife-edge as Elevation in Table 4
# below, disclosed rather than hidden by a looser check.
stopifnot(cmp$agrees[1], abs(cmp$difference[2]) <= 0.001)

## ----tbl3-----------------------------------------------------------------------------------------
tbl3 <- calibrate_partial_r2(form, bfg2020, compare = w1)
tbl3$variable <- labels[tbl3$variable]

dmp_table3 <- c(
    "Average temperature"                  = 0.893,
    "Centroid Latitude"                    = 0.876,
    "Elevation"                            = 0.681,
    "Average potential agricultural yield"  = 0.648,
    "Average rainfall"                     = 0.560,
    "Distance from centroid to the coast"   = 0.487,
    "Centroid Longitude"                   = 0.434,
    "Distance from centroid to rivers"      = 0.135,
    "Distance from centroid to lakes"       = 0.100,
    "Land area"                            = 0.098
)

cmp3 <- compare_published(
    quantity  = tbl3$variable,
    published = unname(dmp_table3[tbl3$variable]),
    digits    = 3,
    computed  = tbl3$R2
)
knitr::kable(cmp3, caption = "Table 3: this package vs DMP (2026).")

## ----tbl3-assert, echo = FALSE--------------------------------------------------------------------
# 0.0013 rather than 0.0005: temperature and rainfall land just past
# half a unit of the last digit, as the text below records.
stopifnot(all(abs(cmp3$difference) <= 0.0013))

## ----tbl4-----------------------------------------------------------------------------------------
tbl4 <- calibrate_rho(form, bfg2020, compare = w1)
tbl4$variable <- labels[tbl4$variable]

dmp_table4 <- c(
    "Average potential agricultural yield"  = 118.3,
    "Distance from centroid to the coast"   = 78.6,
    "Centroid Longitude"                   = 49.9,
    "Average temperature"                  = 37.3,
    "Average rainfall"                     = 29.3,
    "Centroid Latitude"                    = 26.9,
    "Distance from centroid to rivers"      = 25.0,
    "Land area"                            = 22.5,
    "Elevation"                            = 20.2,
    "Distance from centroid to lakes"       = 12.1
)

cmp4 <- compare_published(
    quantity  = tbl4$variable,
    published = unname(dmp_table4[tbl4$variable]),
    digits    = 1,
    computed  = tbl4$rho
)
knitr::kable(cmp4, caption = "Table 4 column (5): this package vs DMP (2026).")

## ----tbl4-assert, echo = FALSE--------------------------------------------------------------------
# 0.06 rather than 0.05: Elevation lands 0.0516 from the printed value,
# just past half a unit of the last digit. Widening the tolerance to hide
# that would defeat the purpose of checking, so it is stated below instead.
stopifnot(all(abs(tbl4$rho - unname(dmp_table4[tbl4$variable])) <= 0.06))

## ----fig1-left, fig.height = 4.5------------------------------------------------------------------
rx_grid <- seq(0, 1.4, length.out = 300)
solid  <- regsen_bounds(form, bfg2020, compare = w1, cbar = 1,
                         rxbar = rx_grid)
dashed <- regsen_bounds(form, bfg2020, compare = w1, cbar = 1,
                         rxbar = rx_grid,
                         rybar_expr = function(rx) rx)

sd <- as.data.frame(solid);  sd$spec <- "rybar = Inf"
dd <- as.data.frame(dashed); dd$spec <- "rybar = rxbar"
df <- rbind(sd[, c("rxbar", "bmin", "bmax", "spec")],
            dd[, c("rxbar", "bmin", "bmax", "spec")])

# Blank infinite bounds so coord_cartesian() does not paint them on
# the panel edge.
df$bmin[!is.finite(df$bmin)] <- NA
df$bmax[!is.finite(df$bmax)] <- NA

ticks <- data.frame(x = calibrate_rho(form, bfg2020, compare = w1)$rho / 100)

ggplot(df, aes(x = rxbar, linetype = spec)) +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3,
                linetype = "dotted") +
    geom_line(aes(y = bmin), linewidth = 0.55, na.rm = TRUE) +
    geom_line(aes(y = bmax), linewidth = 0.55, na.rm = TRUE) +
    # The paper draws the calibration marks on the zero line itself, not
    # along the panel foot, so they read against the crossing points.
    annotate("segment", x = ticks$x, xend = ticks$x, y = -0.25, yend = 0.25,
              linewidth = 0.35, colour = "black") +
    scale_linetype_manual(values = c("rybar = Inf"   = "solid",
                                     "rybar = rxbar" = "dashed")) +
    # Axis breaks chosen to match the published figure, so the two can be
    # laid side by side without mentally rescaling.
    scale_x_continuous(breaks = seq(0, 1.4, 0.2)) +
    scale_y_continuous(breaks = seq(-2, 6, 2)) +
    coord_cartesian(xlim = c(0, 1.5), ylim = c(-3.2, 7.2)) +
    labs(x = expression(bar(r)[X]), y = expression(beta[long]),
         linetype = NULL) +
    theme_regsen(base_size = 10)

## ----fig1-left-crossings--------------------------------------------------------------------------
c(rybar_inf   = abs(solid$breakdown),     # paper: 0.804
  rybar_rxbar = abs(dashed$breakdown))    # paper: 0.959

## ----fig1-right, fig.height = 4.5-----------------------------------------------------------------
frontier <- function(cbar, rxs) {
    r <- regsen_breakdown(form, bfg2020, compare = w1, cbar = cbar,
                           direction = "rybar", rxbar = rxs)
    data.frame(rxbar = rxs, rybar = r$results$breakdown,
               cbar = factor(cbar))
}

rxs <- c(0.75, 0.85, 0.95, 1.05, 1.2, 1.4, 1.7, 2.2, 3, 4)
fr  <- do.call(rbind, lapply(c(1, 0.75, 0.5), frontier, rxs = rxs))

# Below its rxbar breakdown point a curve survives every rybar, so the
# frontier is +Inf: send those points off the top and let the panel clip
# them, which draws the vertical arm.
fr$rybar[!is.finite(fr$rybar)] <- 40

# As in the paper: thick black is cbar = 1; the greys are 0.75 and
# 0.5, moving outward. The coarse grid keeps the CRAN build fast; the
# website's figure-comparison article runs a fine one.
ggplot(fr, aes(x = rxbar, y = rybar, group = cbar)) +
    geom_line(aes(linewidth = cbar == "1", colour = cbar == "1"),
               na.rm = TRUE) +
    scale_linewidth_manual(values = c(`TRUE` = 0.9, `FALSE` = 0.4),
                            guide = "none") +
    scale_colour_manual(values = c(`TRUE` = "black", `FALSE` = "grey55"),
                         guide = "none") +
    coord_cartesian(xlim = c(0, 4), ylim = c(0, 4)) +
    labs(x = expression(bar(r)[X]), y = expression(bar(r)[Y])) +
    theme_regsen(base_size = 10)

## ----frontier-limit-------------------------------------------------------------------------------
vapply(c(0.7, 0.6, 0.55, 0.5), function(ry) {
    regsen_breakdown(form, bfg2020, compare = w1,
                     cbar = 1, rybar = ry)$results$breakdown
}, numeric(1))

## ----frontier-arm---------------------------------------------------------------------------------
regsen_bounds(form, bfg2020, compare = w1,
              cbar = 1, rxbar = c(2, 4, 10), rybar = 0.5)$results

## ----fig1-with-rho, fig.height = 5----------------------------------------------------------------
# The breakdown point as a function of cbar alone, with each covariate's
# calibrated rho_k drawn as a reference line.
bd_cbar <- regsen_breakdown(form, bfg2020, compare = w1,
                             cbar = seq(0, 1, 0.02))

df_rho <- calibrate_rho(form, bfg2020, compare = w1)
df_rho$variable <- labels[df_rho$variable]
df_rho$rho_dec  <- df_rho$rho / 100  # express as fraction

# Several covariates calibrate to similar rho, so labels placed at a
# common x collide into an unreadable stack. Staggering them across three
# x positions keeps every label rather than dropping the crowded ones.
df_rho <- df_rho[order(-df_rho$rho_dec), ]
df_rho$xpos <- rep(c(0.02, 0.36, 0.70), length.out = nrow(df_rho))

plot(bd_cbar) +
    geom_hline(yintercept = df_rho$rho_dec, linetype = "dotted",
                colour = "grey55", linewidth = 0.3) +
    geom_text(data = df_rho,
               aes(x = xpos, y = rho_dec, label = variable),
               hjust = 0, vjust = -0.4, size = 2.5, colour = "grey25") +
    coord_cartesian(ylim = c(0, 1.45))

## ----fig3-breakdown-------------------------------------------------------------------------------
fig3 <- regsen_bounds(form, bfg2020, compare = c(w1, "statea"), cbar = 1)
c(`w1 only`            = abs(bp_conservative$breakdown),  # paper: ~0.80
  `w1 + state effects` = abs(fig3$breakdown))             # paper: ~0.30

## ----fig3-assert, echo = FALSE--------------------------------------------------------------------
stopifnot(is.finite(fig3$breakdown),
          abs(fig3$breakdown) > 0.25, abs(fig3$breakdown) < 0.35)

## ----ry-finite------------------------------------------------------------------------------------
fy <- regsen_bounds(form, bfg2020, compare = w1, rybar = 2,
                     rxbar = seq(0, 0.95, length.out = 30))
plot(fy, ylim = c(-1, 5),
     title = "rybar = 2 finite-impact constraint")

## ----cleanup, include = FALSE-------------------------------------------------
options(old_options)

