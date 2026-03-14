test_that("npv computes correct value for simple case", {
  # Single harvest: -$1,500 at year 0, +$45,000 at year 30, 6% discount
  # NPV = -1500 + 45000/(1.06^30) = -1500 + 7834.95 = 6334.95
  result <- npv(c(-1500, 45000), c(0, 30), 0.06)
  expect_equal(round(result, 2), 6334.95)
})

test_that("npv returns 0 when all flows are 0", {
  expect_equal(npv(c(0, 0, 0), c(0, 10, 20), 0.06), 0)
})

test_that("npv handles year 0 correctly", {
  # Cash flow at year 0 is not discounted
  expect_equal(npv(c(-1000), c(0), 0.06), -1000)
})

test_that("npv validates inputs", {
  expect_error(npv("abc", c(0), 0.06), "must be numeric")
  expect_error(npv(c(100), c(0), "abc"), "must be a single numeric")
  expect_error(npv(c(100, 200), c(0), 0.06), "same length")
  expect_warning(npv(c(100), c(0), 0.50), "exceeds 25%")
})

test_that("incremental_npv identifies preferred alternative", {
  result <- incremental_npv(
    alt_flows = c(-1000, 5000), alt_times = c(0, 10),
    base_flows = c(-500, 2000), base_times = c(0, 10),
    discount_rate = 0.06
  )
  expect_true(is.list(result))
  expect_true(result$incremental_npv > 0)
  expect_equal(result$preferred, "alternative")
})

test_that("incremental_npv correctly identifies baseline as preferred", {
  result <- incremental_npv(
    alt_flows = c(-10000, 5000), alt_times = c(0, 10),
    base_flows = c(-500, 2000), base_times = c(0, 10),
    discount_rate = 0.06
  )
  expect_equal(result$preferred, "baseline")
})

test_that("npv_schedule computes for one-time flows", {
  schedule <- data.frame(
    amount = c(-1500, 45000),
    year = c(0, 30),
    frequency = c("once", "once"),
    period_length = c(NA, NA)
  )
  result <- npv_schedule(schedule, 0.06, 30)
  expected <- npv(c(-1500, 45000), c(0, 30), 0.06)
  expect_equal(round(result, 2), round(expected, 2))
})

test_that("npv_schedule handles annual payments", {
  # Annual cost of $50 for 40 years at 6%
  # PV = 50 * ((1.06^41 - 1) / (0.06 * 1.06^41))
  schedule <- data.frame(
    amount = c(-50),
    year = c(0),
    frequency = c("annual"),
    period_length = c(NA)
  )
  result <- npv_schedule(schedule, 0.06, 40)
  expect_true(result < 0)  # costs are negative
  expect_true(abs(result) > 50 * 10)  # many years of $50
})

test_that("npv_schedule validates inputs", {
  expect_error(npv_schedule("not_a_df", 0.06, 30), "must be a data.frame")
  bad_schedule <- data.frame(amount = 100, year = 0, frequency = "invalid")
  expect_error(npv_schedule(bad_schedule, 0.06, 30), "Invalid frequency")
})

test_that("npv_schedule handles rate = 0 for annual payments", {
  schedule <- data.frame(
    amount = c(-100),
    year = c(0),
    frequency = c("annual"),
    period_length = c(NA)
  )
  # At rate 0, PV of $100/yr for years 0-10 = 100 * 11 = 1100
  result <- npv_schedule(schedule, 0, 10)
  expect_equal(result, -1100)
})

test_that("npv_schedule handles rate = 0 for periodic payments", {
  schedule <- data.frame(
    amount = c(-500),
    year = c(0),
    frequency = c("periodic"),
    period_length = c(5)
  )
  # At rate 0, payments at years 0, 5, 10 = 3 payments of $500
  result <- npv_schedule(schedule, 0, 10)
  expect_equal(result, -1500)
})

test_that("npv_schedule handles periodic flows starting after year 0", {
  schedule <- data.frame(
    amount = c(-500),
    year = c(5),
    frequency = c("periodic"),
    period_length = c(10)
  )
  # Periodic $500 starting at year 5 with period 10, horizon 30
  # years_remaining = 25, PV at year 5. Then discount to year 0
  result <- npv_schedule(schedule, 0.06, 30)
  expect_true(is.numeric(result))
  expect_true(result < 0)  # costs are negative
})
