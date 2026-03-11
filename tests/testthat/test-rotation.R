test_that("optimal_rotation finds LEV-maximizing age", {
  yield <- function(age) 200 * (1 - exp(-0.03 * age))^3
  result <- optimal_rotation(yield, 50, 750, 5, 0.06, criterion = "lev")
  expect_true(is.list(result))
  expect_true(result$optimal_age > 10 && result$optimal_age < 150)
  expect_equal(result$criterion, "lev")
})

test_that("Faustmann rotation < MAI rotation", {
  yield <- function(age) 200 * (1 - exp(-0.03 * age))^3
  lev_rot <- optimal_rotation(yield, 50, 750, 5, 0.06, criterion = "lev")
  mai_rot <- optimal_rotation(yield, 50, 750, 5, 0.06, criterion = "mai")
  expect_true(lev_rot$optimal_age < mai_rot$optimal_age)
})

test_that("higher discount rate -> shorter Faustmann rotation", {
  yield <- function(age) 200 * (1 - exp(-0.03 * age))^3
  rot_low <- optimal_rotation(yield, 50, 750, 5, 0.04, criterion = "lev")
  rot_high <- optimal_rotation(yield, 50, 750, 5, 0.08, criterion = "lev")
  expect_true(rot_high$optimal_age < rot_low$optimal_age)
})

test_that("optimal_rotation accepts data.frame yield table", {
  yield_df <- data.frame(
    age = seq(10, 100, by = 5),
    volume = 200 * (1 - exp(-0.03 * seq(10, 100, by = 5)))^3
  )
  result <- optimal_rotation(yield_df, 50, 750, 5, 0.06)
  expect_true(result$optimal_age > 10)
})

test_that("rotation_comparison returns correct structure", {
  yield <- function(age) 200 * (1 - exp(-0.03 * age))^3
  comp <- rotation_comparison(yield, 50, 750, 5, 0.06)
  expect_true(is.data.frame(comp))
  expect_true(all(c("age", "volume", "mai", "revenue", "npv", "lev") %in% names(comp)))
})
