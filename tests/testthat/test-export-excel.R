test_that("export_excel requires openxlsx", {
  expect_error(
    export_excel(file = "bad_extension.txt"),
    "must end in .xlsx"
  )
})

test_that("export_excel creates all sheets with yield table", {
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
                          file = tmp, title = "Test Export", author = "Test",
                          include_charts = FALSE)

  expect_true(file.exists(tmp))
  expect_equal(result, tmp)

  sheets <- openxlsx::getSheetNames(tmp)
  expect_true("Parameters" %in% sheets)
  expect_true("Yield Table" %in% sheets)
  expect_true("Rotation Analysis" %in% sheets)
  expect_true("Sensitivity" %in% sheets)
  expect_true("Break-Even" %in% sheets)
  expect_true("Scenarios" %in% sheets)
  expect_true("Summary" %in% sheets)
})

test_that("export_excel creates all sheets with schedule", {
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
                          time_horizon = 30, file = tmp,
                          include_charts = FALSE)

  expect_true(file.exists(tmp))
  sheets <- openxlsx::getSheetNames(tmp)
  expect_true("Cash Flow" %in% sheets)
  expect_true("Financial Metrics" %in% sheets)
  expect_true("Parameters" %in% sheets)
  expect_true("Summary" %in% sheets)
})

test_that("export_excel creates full workbook with yield + schedule", {
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
                          regen_cost = 500, time_horizon = 40, file = tmp,
                          include_charts = FALSE)

  expect_true(file.exists(tmp))
  sheets <- openxlsx::getSheetNames(tmp)
  # Should have all 10 sheets minus Charts (include_charts = FALSE)
  expected <- c("Parameters", "Yield Table", "Rotation Analysis",
                "Cash Flow", "Financial Metrics", "Sensitivity",
                "Break-Even", "Scenarios", "Summary")
  expect_true(all(expected %in% sheets))
})

test_that("export_excel sensitivity parameters work", {
  skip_if_not_installed("openxlsx")

  ages <- seq(10, 60, by = 10)
  yt <- yield_table(
    ages = ages,
    products = list(
      timber = data.frame(volume = c(0, 2, 5, 10, 16, 20), price = 200)
    )
  )

  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  # Custom sensitivity grid
  result <- export_excel(yt, discount_rate = 0.05, regen_cost = 500,
                          file = tmp,
                          sensitivity_rates = seq(0.03, 0.08, by = 0.01),
                          sensitivity_prices = seq(0.7, 1.3, by = 0.1),
                          include_charts = FALSE)

  expect_true(file.exists(tmp))
  expect_true("Sensitivity" %in% openxlsx::getSheetNames(tmp))
})

test_that("export_excel Parameters sheet has correct cell values", {
  skip_if_not_installed("openxlsx")

  ages <- seq(10, 40, by = 10)
  yt <- yield_table(
    ages = ages,
    products = list(
      sawlog = data.frame(volume = c(0, 5, 10, 20), price = 300)
    )
  )

  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  export_excel(yt, discount_rate = 0.08, regen_cost = 1000,
               annual_cost = 75, time_horizon = 50,
               file = tmp, include_charts = FALSE)

  wb <- openxlsx::loadWorkbook(tmp)
  params <- openxlsx::read.xlsx(tmp, sheet = "Parameters", colNames = FALSE,
                                 rows = 8:11, cols = 2)

  # B8 = discount rate, B9 = regen, B10 = annual, B11 = horizon
  expect_equal(params[1, 1], 0.08)
  expect_equal(params[2, 1], 1000)
  expect_equal(params[3, 1], 75)
  expect_equal(params[4, 1], 50)
})
