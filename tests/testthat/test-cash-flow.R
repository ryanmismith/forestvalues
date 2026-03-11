test_that("cash_flow_schedule expands activities correctly", {
  activities <- data.frame(
    name = c("Planting", "Tax", "Harvest"),
    amount = c(-750, -50, 6600),
    year = c(0, 0, 30),
    frequency = c("once", "annual", "once"),
    period_length = c(NA, NA, NA)
  )
  result <- cash_flow_schedule(activities, 30, 0.06)
  expect_equal(nrow(result), 31)  # years 0-30
  expect_equal(result$cash_flow[1], -750 + -50)  # year 0: planting + tax
  expect_equal(result$cash_flow[31], 6600 + -50)  # year 30: harvest + tax
  expect_equal(result$cash_flow[2], -50)  # year 1: just tax
})

test_that("cash_flow_schedule handles periodic activities", {
  activities <- data.frame(
    name = "Validation",
    amount = -500,
    year = 10,
    frequency = "periodic",
    period_length = 10
  )
  result <- cash_flow_schedule(activities, 40, 0.06)
  # Should have costs at years 10, 20, 30, 40
  expect_equal(result$cash_flow[11], -500)  # year 10
  expect_equal(result$cash_flow[21], -500)  # year 20
  expect_equal(result$cash_flow[31], -500)  # year 30
  expect_equal(result$cash_flow[41], -500)  # year 40
  expect_equal(result$cash_flow[1], 0)      # year 0: nothing
})

test_that("cash_flow_schedule cumulative NPV is correct", {
  activities <- data.frame(
    name = "Harvest",
    amount = 10000,
    year = 10,
    frequency = "once",
    period_length = NA
  )
  result <- cash_flow_schedule(activities, 10, 0.06)
  expected_npv <- 10000 / (1.06^10)
  expect_equal(round(result$cumulative_npv[11], 2), round(expected_npv, 2))
})

test_that("project_income returns complete results", {
  yield <- function(age) 200 * (1 - exp(-0.03 * age))^3
  result <- project_income(yield, 50, c(25, 40), regen_cost = 750,
                            annual_cost = 5, discount_rate = 0.06)
  expect_true(is.list(result))
  expect_true(is.data.frame(result$harvests))
  expect_true(is.numeric(result$npv))
  expect_true(is.numeric(result$lev))
  expect_equal(nrow(result$harvests), 2)
})

test_that("project_income harvest fractions work", {
  yield <- function(age) 100  # constant yield for simplicity
  result <- project_income(yield, 10, c(20, 40),
                            harvest_fractions = c(0.5, 1.0),
                            discount_rate = 0.06)
  # First harvest: 100 * 1.0 * 0.5 = 50
  expect_equal(result$harvests$volume[1], 50)
  # Second harvest: 100 * 0.5 * 1.0 = 50 (half remaining)
  expect_equal(result$harvests$volume[2], 50)
})

test_that("annualize converts PV to annual equivalent", {
  # PV of $1000 annuity at 6% for 30 years = 1000 * ((1.06^30-1)/(0.06*1.06^30))
  # = 1000 * 13.7648 = 13764.83
  # So annualize(13764.83, 0.06, 30) should return ~1000
  pv <- 1000 * ((1.06^30 - 1) / (0.06 * 1.06^30))
  result <- annualize(pv, 0.06, 30)
  expect_equal(round(result, 2), 1000)
})

test_that("annualize handles zero discount rate", {
  expect_equal(annualize(3000, 0, 30), 100)
})
