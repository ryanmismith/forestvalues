test_that("irr finds correct rate for simple case", {
  # Invest $1000, receive $2000 in 10 years
  # IRR: 1000 = 2000/(1+r)^10, so r = (2000/1000)^(1/10) - 1 = 0.07177
  result <- irr(c(-1000, 2000), c(0, 10))
  expect_equal(round(result, 4), 0.0718)
})

test_that("irr is consistent with npv", {
  cf <- c(-1500, -800, 2500, 6600)
  t <- c(0, 12, 20, 40)
  rate <- irr(cf, t)
  # NPV at IRR should be ~0
  npv_at_irr <- npv(cf, t, rate)
  expect_true(abs(npv_at_irr) < 0.01)
})

test_that("irr warns on multiple sign changes", {
  expect_warning(
    irr(c(-1000, 3000, -2500), c(0, 1, 2)),
    "Multiple sign changes"
  )
})

test_that("irr validates inputs", {
  expect_error(irr(c(100, 200), c(0, 1)), "positive and one negative")
  expect_error(irr(c(-100), c(0)), "At least two")
  expect_error(irr(c(-100, 200), c(0)), "same length")
})

test_that("mirr computes correctly", {
  result <- mirr(
    cash_flows = c(-1500, 6600),
    times = c(0, 30),
    finance_rate = 0.06,
    reinvest_rate = 0.04
  )
  expect_true(is.numeric(result))
  expect_true(result > 0)
  # MIRR should be between finance and reinvest rates for typical cases
})

test_that("mirr validates inputs", {
  expect_error(mirr(c(100, 200), c(0, 1), 0.06, 0.04), "positive and one negative")
})
