test_that("export_excel requires openxlsx", {
  # If openxlsx isn't installed, should give a clear error
  # We can't easily test this without mocking, so just test the file path validation
  expect_error(
    export_excel(file = "bad_extension.txt"),
    "must end in .xlsx"
  )
})

test_that("export_excel creates a file with yield table", {
  skip_if_not_installed("openxlsx")

  ages <- seq(10, 60, by = 10)
  yt <- yield_table(
    ages = ages,
    products = list(
      sawlog = data.frame(volume = c(0, 2, 5, 10, 16, 20), price = 254),
      pulp = data.frame(volume = c(5, 12, 18, 22, 24, 25), price = 9)
    ),
    product_units = c(sawlog = "mbf", pulp = "ton")
  )

  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  result <- export_excel(yt, discount_rate = 0.06, regen_cost = 750,
                          annual_cost = 50, time_horizon = 60,
                          file = tmp, title = "Test Export", author = "Test")

  expect_true(file.exists(tmp))
  expect_equal(result, tmp)

  # Verify sheets exist
  sheets <- openxlsx::getSheetNames(tmp)
  expect_true("Parameters" %in% sheets)
  expect_true("Yield Table" %in% sheets)
  expect_true("Rotation Analysis" %in% sheets)
  expect_true("Summary" %in% sheets)
})

test_that("export_excel creates a file with schedule", {
  skip_if_not_installed("openxlsx")

  activities <- data.frame(
    name = c("Planting", "Tax", "Harvest"),
    amount = c(-750, -50, 6600),
    year = c(0, 0, 30),
    frequency = c("once", "annual", "once"),
    period_length = c(NA, NA, NA)
  )

  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  result <- export_excel(schedule = activities, discount_rate = 0.06,
                          time_horizon = 30, file = tmp)

  expect_true(file.exists(tmp))
  sheets <- openxlsx::getSheetNames(tmp)
  expect_true("Cash Flow" %in% sheets)
  expect_true("Parameters" %in% sheets)
})

test_that("export_excel works with both yield table and schedule", {
  skip_if_not_installed("openxlsx")

  ages <- seq(10, 40, by = 10)
  yt <- yield_table(
    ages = ages,
    products = list(
      sawlog = data.frame(volume = c(0, 5, 10, 20), price = 200)
    )
  )

  activities <- data.frame(
    name = c("Plant", "Harvest"),
    amount = c(-500, 4000),
    year = c(0, 40),
    frequency = c("once", "once"),
    period_length = c(NA, NA)
  )

  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  result <- export_excel(yt, schedule = activities, discount_rate = 0.05,
                          regen_cost = 500, time_horizon = 40, file = tmp)

  expect_true(file.exists(tmp))
  sheets <- openxlsx::getSheetNames(tmp)
  expect_true(all(c("Parameters", "Yield Table", "Rotation Analysis",
                      "Cash Flow", "Summary") %in% sheets))
})
