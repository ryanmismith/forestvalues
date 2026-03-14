test_that("stumpage_value returns correct structure", {
  result <- suppressMessages(stumpage_value("SM", 10, "sawlog"))
  expect_true(is.data.frame(result))
  expect_true(all(c("species", "volume", "product_class", "unit_price", "total_value")
                   %in% names(result)))
})

test_that("stumpage_value uses default prices with message", {
  expect_message(stumpage_value("SM", 10, "sawlog"), "2019 Maine")
})

test_that("stumpage_value looks up correct default price for sugar maple", {
  result <- suppressMessages(stumpage_value("SM", 1, "sawlog"))
  expect_equal(result$unit_price, 254)  # $254/MBF for sugar maple sawlog
  expect_equal(result$total_value, 254)
})

test_that("stumpage_value handles custom price table", {
  custom <- create_price_table(
    species = "SM", product_class = "sawlog", price = 300, unit = "mbf"
  )
  result <- stumpage_value("SM", 10, "sawlog", price_table = custom)
  expect_equal(result$unit_price, 300)
  expect_equal(result$total_value, 3000)
})

test_that("stumpage_value warns for unknown species", {
  custom <- create_price_table("SM", "sawlog", 100, "mbf")
  expect_warning(stumpage_value("ZZ", 10, "sawlog", price_table = custom), "No price found")
})

test_that("create_price_table validates inputs", {
  expect_error(create_price_table(123, "sawlog", 100), "must be character")
  expect_error(create_price_table("SM", "sawlog", "abc"), "must be numeric")
})

test_that("update_prices adjusts for inflation", {
  pt <- create_price_table("SM", "sawlog", 100, "mbf")
  updated <- update_prices(pt, 0.03, 2019, 2024)
  expected <- 100 * (1.03)^5
  expect_equal(updated$price, expected)
})

test_that("stumpage_value warns for $0 price species", {
  expect_warning(
    suppressMessages(stumpage_value("JP", 10, "sawlog")),
    "is \\$0 in the price table"
  )
})

test_that("stumpage_value returns $0 value for $0 price species", {
  result <- suppressWarnings(suppressMessages(stumpage_value("JP", 10, "sawlog")))
  expect_equal(result$unit_price, 0)
  expect_equal(result$total_value, 0)
})
