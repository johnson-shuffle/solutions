Simulation for checking statistical procedures and model fits
================

## Question 1

### (a)

``` r
set.seed(1)

tpar <- c(0, 5, 4)
beta <- c(3, 0.1, 0.5)
x1 <- 1:100
x2 <- rbinom(100, 1, 0.5)
y  <- cbind(1, x1, x2) %*% beta + tpar[2] * rt(100, tpar[3])

reg <- lm(y ~ x1 + x2)
confint(reg, '(Intercept)', level = 0.68)
```

    ##                   16 %     84 %
    ## (Intercept) -0.3258496 3.525355

``` r
confint(reg, 'x1', level = 0.68)
```

    ##          16 %      84 %
    ## x1 0.09985148 0.1590144

``` r
confint(reg, 'x2', level = 0.68)
```

    ##         16 %     84 %
    ## x2 -2.065578 1.352763

``` r
names(beta) <- names(coef(reg))
```

The true coefficients are all covered by the 68% confidence intervals.

### (b)

``` r
sim_fun <- function(tpar, beta, level) {
  x1 <- 1:100
  x2 <- rbinom(100, 1, 0.5)
  y  <- cbind(1, x1, x2) %*% beta + tpar[2] * rt(100, tpar[3])
  reg <- lm(y ~ x1 + x2)
  lab <- names(coef(reg))
  
  cis <- map(lab, ~confint(reg, .x, level = level)) ; names(cis) <- lab
  map_lgl(lab, ~beta[.x] >= cis[[.x]][1] & beta[.x] <= cis[[.x]][2])
}

n_sims <- replicate(1000, sim_fun(tpar, beta, 0.68))

# coverages
apply(n_sims, 1, mean)
```

    ## [1] 0.676 0.677 0.665

## (c)

``` r
library(hett)

sim_fun <- function(tpar, beta, level) {
  x1 <- 1:100
  x2 <- rbinom(100, 1, 0.5)
  y  <- cbind(1, x1, x2) %*% beta + tpar[2] * rt(100, tpar[3])
  reg <- tlm(y ~ x1 + x2, start = list(dof = 4))
  cof <- summary(reg)$loc.summary$coefficients
  lab <- names(cof[, 1])
  
  cis <- map(lab, ~c(cof[.x, ][1] - cof[.x, ][2], cof[.x, ][1] + cof[.x, ][2]))
  names(cis) <- lab
  map_lgl(lab, ~beta[.x] >= cis[[.x]][1] & beta[.x] <= cis[[.x]][2])
}
n_sims <- replicate(1000, sim_fun(tpar, beta, 0.68))

# coverages
apply(n_sims, 1, mean)
```

    ## [1] 0.679 0.665 0.688

## Question 2

Skip

## Question 3

Skip

### Question 4

``` r
# reload chapter 6, exercise 1
dat <- knitr::load_cache('arm06-q01_', 'dat')
```

### (a)

``` r
reg <- glm(fupacts ~ bs_hiv, family = poisson(link = 'log'), data = dat)

# third quartile
tq <- quantile(dat$fupacts, probs = 0.75)

sim_fun <- function(reg, dat) {
  p <- sim(reg, 1)
  m <- cbind(1, dat$bs_hiv) %*% t(coef(p))
  r <- predict(reg, type = 'response')
  x <- length(r[round(r) == 0]) / length(r)
  y <- length(r[round(r) > tq]) / length(r)
  c(zero = x, five = y)
}
n_sims <- apply(replicate(1000, sim_fun(reg, dat)), 1, mean)

# simulated vs actual
cat(n_sims[['zero']], 'vs', table(dat$fupacts == 0)[[2]] / nrow(dat))
```

    ## 0 vs 0.2761905

``` r
cat(n_sims[['five']], 'vs', table(dat$fupacts > tq)[[2]] / nrow(dat))
```

    ## 0 vs 0.2452381

### (b)

``` r
reg <- glm(fupacts ~ bs_hiv, family = quasipoisson(link = 'log'), data = dat)

sim_fun <- function(reg, dat) {
  p <- sim(reg, 1)
  m <- cbind(1, dat$bs_hiv) %*% t(coef(p))
  r <- map_dbl(1:nrow(dat), ~rnbinom(1, m[.x] / (sigma.hat(p) - 1), mu = m[.x]))
  x <- length(r[round(r) == 0]) / length(r)
  y <- length(r[round(r) > tq]) / length(r)
  c(zero = x, five = y)
}
n_sims <- apply(replicate(1000, sim_fun(reg, dat)), 1, mean)

# simulated vs actual
cat(n_sims[['zero']], 'vs', table(dat$fupacts == 0)[[2]] / nrow(dat))
```

    ## 1 vs 0.2761905

``` r
cat(n_sims[['five']], 'vs', table(dat$fupacts > tq)[[2]] / nrow(dat))
```

    ## 1 vs 0.2452381

### (b)

``` r
reg <- glm(
  fupacts ~ bs_hiv + bupacts + women_alone,
  family = quasipoisson(link = 'log'),
  data = dat
  )

sim_fun <- function(reg, dat) {
  p <- sim(reg, 1)
  m <- cbind(1, as.matrix(model.frame(reg)[, -1])) %*% t(coef(p))
  r <- map_dbl(1:nrow(dat), ~rnbinom(1, m[.x] / (sigma.hat(p) - 1), mu = m[.x]))
  x <- length(r[round(r) == 0]) / length(r)
  y <- length(r[round(r) > tq]) / length(r)
  c(zero = x, five = y)
}
n_sims <- apply(replicate(1000, sim_fun(reg, dat)), 1, mean)

# simulated vs actual
cat(n_sims[['zero']], 'vs', table(dat$fupacts == 0)[[2]] / nrow(dat))
```

    ## 1 vs 0.2761905

``` r
cat(n_sims[['five']], 'vs', table(dat$fupacts > tq)[[2]] / nrow(dat))
```

    ## 1 vs 0.2452381
