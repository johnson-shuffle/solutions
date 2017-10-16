set.seed(314159)

X1 <- tibble(
  x1 = 1,
  x2 = runif(1000, 0, 1),
  x3 = rbinom(1000, 1, 0.5),
)
X1 %<>% as.matrix()
y1 <- X1 %*% c(-4, 7, 1.5) + rt(1000, 7)
y1 <- if_else(y1 >= 0.5, 1, 0)

X2 <- tibble(
  x1 = 1,
  x2 = runif(1000, 0, 1),
  x3 = rbinom(1000, 1, 0.5),
)
X2 %<>% as.matrix()
y2 <- X2 %*% c(-4, 7, 1.5) + rnorm(1000, 0, 1)
y2 <- if_else(y2 >= 0.5, 1, 0)

par0 <- list(beta = rep(0, 3), nu = 1000)
rob1 <- robit_em(par0, y1, X1) ; print(rob1)
rob2 <- robit_em(par0, y2, X2) ; print(rob2)
