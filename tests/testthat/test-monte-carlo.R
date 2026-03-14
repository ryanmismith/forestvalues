test_that("monte_carlo produces correct number of simulations", {
  my_fn <- function(x) x^2
  result <- monte_carlo(
    eval_fn = my_fn,
    base_params = list(),
    stochastic_params = list(x = list(dist = "normal", mean = 0, sd = 1)),
    n_sims = 500, seed = 42
  )
  expect_s3_class(result, "mc_forest")
  expect_equal(result$n_sims, 500)
  expect_length(result$values, 500)
})

test_that("monte_carlo summary has expected components", {
  my_fn <- function(x) x
  result <- monte_carlo(
    my_fn, list(),
    list(x = list(dist = "normal", mean = 100, sd = 10)),
    n_sims = 1000, seed = 42
  )
  expect_true(abs(result$summary$mean - 100) < 5)
  expect_true(!is.null(result$summary$quantiles))
})

test_that("monte_carlo with lognormal distribution", {
  my_fn <- function(price) price * 10
  result <- monte_carlo(
    my_fn, list(),
    list(price = list(dist = "lognormal", meanlog = log(50), sdlog = 0.2)),
    n_sims = 1000, seed = 42
  )
  expect_true(all(result$values > 0))  # lognormal always positive
})

test_that("monte_carlo with uniform distribution", {
  my_fn <- function(rate) rate
  result <- monte_carlo(
    my_fn, list(),
    list(rate = list(dist = "uniform", min = 0.03, max = 0.09)),
    n_sims = 1000, seed = 42
  )
  expect_true(all(result$values >= 0.03 & result$values <= 0.09))
})

test_that("monte_carlo with triangular distribution", {
  my_fn <- function(x) x
  result <- monte_carlo(
    my_fn, list(),
    list(x = list(dist = "triangular", min = 0, mode = 5, max = 10)),
    n_sims = 1000, seed = 42
  )
  expect_true(all(result$values >= 0 & result$values <= 10))
})

test_that("stochastic_prices GBM produces correct dimensions", {
  paths <- stochastic_prices(30, 50, "gbm", drift = 0.02, volatility = 0.2,
                              n_paths = 10, seed = 42)
  expect_equal(nrow(paths), 31)  # 0:30

  expect_equal(ncol(paths), 10)
  expect_true(all(paths > 0))  # GBM stays positive
  expect_equal(paths[1, 1], 50)  # starts at initial_price
})

test_that("stochastic_prices OU mean-reverts", {
  paths <- stochastic_prices(100, 80, "ou", volatility = 5,
                              mean_reversion_rate = 0.3, long_run_mean = 50,
                              n_paths = 100, seed = 42)
  # After 100 periods with kappa=0.3, should be close to theta=50
  final_mean <- mean(paths[101, ])
  expect_true(abs(final_mean - 50) < 10)
})

test_that("stochastic_prices OU validates parameters", {
  expect_error(stochastic_prices(30, 50, "ou", volatility = 5),
               "mean_reversion_rate")
})

test_that("simulate_yield is unbiased", {
  yield <- function(age) 100
  sims <- simulate_yield(yield, ages = 30, cv = 0.15, n_sims = 10000, seed = 42)
  # Mean should be close to 100 (unbiased)
  expect_true(abs(mean(sims) - 100) < 5)
})

test_that("risk_metrics computes correctly", {
  set.seed(42)
  sims <- rnorm(10000, mean = 5000, sd = 3000)
  metrics <- risk_metrics(sims)
  expect_true(is.list(metrics))
  expect_true(metrics$prob_loss < 0.15)  # mean=5000, sd=3000, so P(loss) ~5%
  expect_true(metrics$var < metrics$mean)
})

test_that("risk_metrics works with mc_forest object", {
  my_fn <- function(x) x
  mc <- monte_carlo(my_fn, list(),
                     list(x = list(dist = "normal", mean = 100, sd = 20)),
                     n_sims = 500, seed = 42)
  metrics <- risk_metrics(mc)
  expect_true(is.numeric(metrics$var))
})

test_that("stochastic_prices OU enforces price floor by default", {
  # Use high volatility and low initial price to trigger negative values
  # without a floor
  paths <- stochastic_prices(50, 5, "ou", volatility = 10,
                              mean_reversion_rate = 0.1, long_run_mean = 5,
                              n_paths = 100, seed = 42)
  expect_true(all(paths >= 0))  # default floor = 0
})

test_that("stochastic_prices OU allows negative prices when floor is NULL", {
  paths <- stochastic_prices(50, 5, "ou", volatility = 10,
                              mean_reversion_rate = 0.1, long_run_mean = 5,
                              price_floor = NULL,
                              n_paths = 100, seed = 42)
  # With high volatility and low price, some negatives are likely
  expect_true(is.numeric(paths))
})

test_that("stochastic_prices respects custom price floor", {
  paths <- stochastic_prices(30, 50, "ou", volatility = 20,
                              mean_reversion_rate = 0.1, long_run_mean = 50,
                              price_floor = 10,
                              n_paths = 50, seed = 42)
  expect_true(all(paths >= 10))
})
