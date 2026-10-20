## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6, fig.height = 4
)

## -----------------------------------------------------------------------------
library(regsensitivity)
library(ggplot2)

set.seed(2208)
n <- 5000
w1 <- rnorm(n)
w2 <- rnorm(n)
w3 <- rnorm(n)
x  <- 0.3 * w1 + 0.3 * w2 + 0.5 * w3 + rnorm(n)
y  <- 0.8 * x + 0.4 * w1 - 0.3 * w2 + 0.6 * w3 + rnorm(n)
dat <- data.frame(y = y, x = x, w1 = w1, w2 = w2)   # w3 is unobserved!

form <- y ~ x + w1 + w2

## -----------------------------------------------------------------------------
coef(lm(form, data = dat))["x"]

## ----mp-fig1------------------------------------------------------------------
res <- regsen_bounds(form, dat,
                      analysis = "oster",
                      delta = seq(-2, 2, 0.02),
                      r2long = 1)
plot(res, ylim = c(-2, 4),
     title = "Figure 1 (stylized): Oster identified set vs delta")

## ----mp-fig2------------------------------------------------------------------
res2 <- regsen_bounds(form, dat,
                       analysis = "oster",
                       delta = seq(0, 1, 0.005),
                       delta_type = "bound",
                       r2long = 1)
plot(res2, ylim = c(0, 2),
     title = "Figure 2 (stylized): cumulative identified set")

## ----mp-maxovb-inf------------------------------------------------------------
bmed <- regsen_bounds(form, dat, analysis = "oster",
                       delta = 0, r2long = 1)$dgp$beta_med
sapply(c(0.5, 1, round(bmed, 3) + 0.001), function(M) {
    regsen_breakdown(form, dat, analysis = "oster",
                      r2long = 1, maxovb = M)$results$breakdown[1]
})

## ----mp-maxovb----------------------------------------------------------------
ovbs <- seq(1.1, 3, 0.1)
out <- lapply(ovbs, function(M) {
    r <- regsen_breakdown(form, dat,
                           analysis = "oster",
                           r2long = 1, maxovb = M)
    data.frame(maxovb = M, breakdown = r$results$breakdown[1])
})
out <- do.call(rbind, out)
uncapped <- regsen_breakdown(form, dat, analysis = "oster",
                              r2long = 1)$results$breakdown[1]

ggplot(out, aes(maxovb, breakdown)) +
    geom_hline(yintercept = 1, linetype = "dotted", colour = "grey40") +
    geom_line() + geom_point() +
    labs(x = "maxovb", y = "Delta (breakdown)",
         title = "Oster sign-change breakdown vs max OVB") +
    theme_regsen()

