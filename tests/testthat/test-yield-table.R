test_that("yield_table creates valid object", {
  ages <- seq(10, 60, by = 10)
  yt <- yield_table(
    ages = ages,
    products = list(
      sawlog = data.frame(volume = c(0, 2, 5, 10, 16, 20), price = 254),
      pulp = data.frame(volume = c(5, 12, 18, 22, 24, 25), price = 9)
    ),
    product_units = c(sawlog = "mbf", pulp = "ton")
  )
  expect_s3_class(yt, "yield_table")
  expect_equal(yt$product_names, c("sawlog", "pulp"))
  expect_length(yt$ages, 6)
  expect_true(is.function(yt$total_value_fn))
})

test_that("yield_table total_value_fn sums products", {
  ages <- seq(10, 60, by = 10)
  yt <- yield_table(
    ages = ages,
    products = list(
      sawlog = data.frame(volume = c(0, 0, 5, 10, 16, 20), price = 100),
      pulp = data.frame(volume = c(5, 10, 15, 20, 20, 20), price = 10)
    )
  )
  # At age 10: sawlog=0*100=0, pulp=5*10=50, total=50
  expect_equal(yt$total_value_fn(10), 50)
  # At age 60: sawlog=20*100=2000, pulp=20*10=200, total=2200
  expect_equal(yt$total_value_fn(60), 2200)
})

test_that("yield_table validates inputs", {
  expect_error(yield_table("bad", list()), "must be a numeric")
  expect_error(yield_table(1:5, list(a = "bad")), "must be a data.frame")
  expect_error(
    yield_table(1:5, list(a = data.frame(x = 1:5))),
    "must have a 'volume' column"
  )
  expect_error(
    yield_table(1:5, list(a = data.frame(volume = 1:3))),
    "must have 5 rows"
  )
})

test_that("optimal_rotation_mp finds optimum", {
  ages <- seq(10, 80, by = 10)
  yt <- yield_table(
    ages = ages,
    products = list(
      sawlog = data.frame(volume = c(0, 0, 2, 5, 10, 16, 20, 22), price = 254),
      pulp = data.frame(volume = c(5, 12, 18, 22, 24, 25, 25, 24), price = 9)
    )
  )
  result <- optimal_rotation_mp(yt, regen_cost = 750, annual_cost = 50,
                                 discount_rate = 0.06)
  expect_true(is.list(result))
  expect_true(result$optimal_age >= 10 && result$optimal_age <= 80)
  expect_true(is.data.frame(result$product_detail))
  expect_equal(nrow(result$product_detail), 2)
  expect_equal(result$criterion, "lev")
})

test_that("optimal_rotation_mp requires prices", {
  ages <- seq(10, 40, by = 10)
  yt <- yield_table(
    ages = ages,
    products = list(sawlog = data.frame(volume = c(0, 5, 10, 20)))
  )
  expect_error(optimal_rotation_mp(yt, 750, 0, 0.06), "no prices")
})

test_that("rotation_comparison_mp returns correct structure", {
  ages <- seq(10, 60, by = 10)
  yt <- yield_table(
    ages = ages,
    products = list(
      sawlog = data.frame(volume = c(0, 2, 5, 10, 16, 20), price = 254),
      pulp = data.frame(volume = c(5, 12, 18, 22, 24, 25), price = 9)
    )
  )
  comp <- rotation_comparison_mp(yt, 750, 50, 0.06, ages = seq(20, 60, by = 10))
  expect_true(is.data.frame(comp))
  expect_true("sawlog_vol" %in% names(comp))
  expect_true("pulp_val" %in% names(comp))
  expect_true("npv" %in% names(comp))
  expect_true("lev" %in% names(comp))
})

test_that("print.yield_table works without error", {
  ages <- seq(10, 40, by = 10)
  yt <- yield_table(
    ages = ages,
    products = list(
      sawlog = data.frame(volume = c(0, 5, 10, 20), price = 254)
    ),
    product_units = c(sawlog = "mbf")
  )
  expect_output(print(yt), "Multi-Product Yield Table")
})
