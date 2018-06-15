# ----- preample ----------------------------------------------------------

load_tidy()


# ----- properties of regression ------------------------------------------

# figure 3
set.seed(1)

dat <- tibble(
  x = rnorm(1E4, 0, 1),
  u = rnorm(1E4, 0, 1),
  y = 5.5 * x + 12 * u
  )

reg <- lm(y ~ x, data = dat)

dat %<>% mutate(
  yhat1 = predict(reg),
  yhat2 = 0.0732608 + 5.685033 * x,
  uhat1 = reg$residuals,
  uhat2 = y - yhat2
  )

select(dat, yhat1, yhat2) %>% skim()
select(dat, uhat1, uhat2) %>% skim()

ggplot(dat, aes(x, y)) +
  geom_point() +
  stat_smooth(method = 'lm', col = 'red') +
  labs(title = 'OLS Regression Line') +
  ggthemes::theme_stata()

# figure 4
ggplot(dat, aes(yhat1, uhat1)) +
  geom_point() +
  labs(x = 'Fitted Values', y = 'Residuals') +
  ggthemes::theme_stata()

# table 6
set.seed(2)

dat <- tibble(
  x = 9 * rnorm(10, 0, 1),
  u = 36 * rnorm(10, 0, 1),
  y = 3 + 2 * x + u
  )

reg <- lm(y ~ x, data = dat)

dat %<>% mutate(
  yhat = predict(reg),
  uhat = reg$residuals,
  x_uhat = t(x) * uhat,
  yhat_uhat = t(yhat) * uhat
  )

summarise_at(dat, vars(x, u, y, yhat, uhat, x_uhat, yhat_uhat), sum)

# figure 5
ols <- function() {
  dat <- tibble(
    x = 9 * rnorm(1E4, 0, 1),
    u = 36 * rnorm(1E4, 0, 1),
    y = 3 + 2 * x + u
    )
  reg <- lm(y ~ x, data = dat)
  coef(reg)['x']
}

beta <- replicate(1E3, ols())
skim(beta)

ggplot(tibble(x = beta)) +
  geom_histogram(aes(x)) +
  ggthemes::theme_stata()


# ----- regression anatomy ------------------------------------------------

# sysuse auto
dat <- read_dta('/Applications/Stata/auto.dta')

# bivariate regression
reg_b <- lm(price ~ length, data = dat)

# multivariate regression
reg_m <- lm(price ~ length + weight + headroom + mpg, data = dat)

# auxiliary regression 1 
reg_a1 <- lm(length ~ weight + headroom + mpg, data = dat)
dat %<>% mutate(length_resid = reg_a1$residuals)

# auxiliary regression 2
reg_a2 <- lm(price ~ length_resid, data = dat)

# check the values
cat(
  coef(reg_m)[['length']],
  coef(reg_a2)[['length_resid']],
  cov(dat$price, dat$length_resid) / var(dat$length_resid)
  )

# shift factor (mean adjustment of length requires adjustment of intercept)
s_factor <- coef(reg_b)['length'] * mean(dat$length)

# figure 6
tibble(
  price = rep(dat$price, 2),
  length = c(dat$length - mean(dat$length), dat$length_resid),
  type = unlist(map(c('BV', 'MV'), rep, times = nrow(dat)))
  ) %>%
  ggplot() +
  geom_point(aes(length, price, colour = type)) +
  scale_colour_manual(values = c('red', 'green')) +
  geom_abline(
    intercept = coef(reg_b)['(Intercept)'] + s_factor,
    slope = coef(reg_b)['length'],
    col = 'red'
    ) +
  geom_abline(
    intercept = coef(reg_a2)['(Intercept)'],
    slope = coef(reg_a2)['length_resid'],
    col = 'green'
    ) +
  labs(title = 'Regression Anatomy', x = 'Length', y = 'Price') +
  ggthemes::theme_stata()
