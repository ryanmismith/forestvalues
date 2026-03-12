test_that("lev computes correct Faustmann value", {
  # LEV = 52000 / ((1.06)^40 - 1) = 52000 / 9.2857 = 5600.44
  result <- lev(52000, rotation_age = 40, discount_rate = 0.06)
  expect_equal(round(result, 2), 5600.44)
})

test_that("lev with is_npv applies perpetuity multiplier", {
  # If NPV = 1000 and rotation = 30, r = 0.06
  # LEV = 1000 * (1.06^30) / ((1.06^30) - 1) = 1000 * 5.7435 / 4.7435 = 1210.83
  result <- lev(1000, rotation_age = 30, discount_rate = 0.06, is_npv = TRUE)
  expect_true(result > 1000)  # perpetuity multiplier > 1
})

test_that("lev validates inputs", {
  expect_error(lev("abc", 40, 0.06), "must be a single numeric")
  expect_error(lev(1000, -5, 0.06), "must be positive")
  expect_error(lev(1000, 40, 0), "must be positive")
  expect_warning(lev(1000, 40, 0.30), "exceeds 25%")
})

test_that("bare_land_value is identical to lev", {
  expect_equal(
    bare_land_value(52000, 40, 0.06),
    lev(52000, 40, 0.06)
  )
})

test_that("lev_schedule computes from a management schedule", {
  schedule <- data.frame(
    amount = c(-750, -50, 6600),
    year = c(0, 0, 30),
    frequency = c("once", "annual", "once"),
    period_length = c(NA, NA, NA)
  )
  result <- lev_schedule(schedule, discount_rate = 0.06, rotation_age = 30)
  expect_true(is.numeric(result))
  expect_true(is.finite(result))
})

test_that("higher discount rates reduce LEV", {
  lev_low <- lev(50000, 40, 0.04)
  lev_high <- lev(50000, 40, 0.08)
  expect_true(lev_low > lev_high)
})
