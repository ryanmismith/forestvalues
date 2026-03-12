test_that("sensitivity_1way varies parameter correctly", {
  my_fn <- function(x, y) x + y
  result <- sensitivity_1way(
    my_fn, "x", seq(1, 5),
    base_params = list(x = 3, y = 10)
  )
  expect_s3_class(result, "sensitivity_1way")
  expect_equal(nrow(result), 5)
  expect_equal(result$outcome, c(11, 12, 13, 14, 15))
})

test_that("sensitivity_1way validates param_name", {
  my_fn <- function(x) x
  expect_error(
    sensitivity_1way(my_fn, "missing", 1:5, list(x = 1)),
    "not found"
  )
})

test_that("sensitivity_2way creates correct grid", {
  my_fn <- function(x, y) x * y
  result <- sensitivity_2way(
    my_fn, "x", c(1, 2, 3), "y", c(10, 20),
    base_params = list(x = 2, y = 15)
  )
  expect_s3_class(result, "sensitivity_2way")
  expect_equal(dim(result$matrix), c(3, 2))
  expect_equal(result$matrix[1, 1], 10)   # 1 * 10
  expect_equal(result$matrix[3, 2], 60)   # 3 * 20
})

test_that("tornado_plot ranks by impact", {
  my_fn <- function(a, b, c) a + b + c
  result <- tornado_plot(
    my_fn,
    param_ranges = list(a = c(0, 100), b = c(0, 10), c = c(0, 50)),
    base_params = list(a = 50, b = 5, c = 25)
  )
  expect_s3_class(result, "tornado")
  # 'a' has range 100, 'c' has range 50, 'b' has range 10
  expect_equal(result$parameter[1], "a")
  expect_equal(result$parameter[2], "c")
  expect_equal(result$parameter[3], "b")
})

test_that("breakeven_analysis finds correct value", {
  my_fn <- function(x) x - 5
  result <- breakeven_analysis(
    my_fn, "x",
    base_params = list(x = 10),
    target = 0
  )
  expect_equal(round(result$breakeven_value, 6), 5)
})

test_that("breakeven_analysis with NPV = 0", {
  # Price where NPV of $price*150 in 30 years minus $750 today = 0
  my_npv <- function(price, cost, discount_rate) {
    npv(c(-cost, price * 150), c(0, 30), discount_rate)
  }
  result <- breakeven_analysis(
    my_npv, "price",
    base_params = list(price = 50, cost = 750, discount_rate = 0.06),
    target = 0
  )
  # Verify: NPV at breakeven price should be ~0
  check_npv <- my_npv(result$breakeven_value, 750, 0.06)
  expect_true(abs(check_npv) < 0.01)
})
