# =========================================================================
# timber_tax
# =========================================================================
test_that("timber_tax 631b computes capital gains correctly", {
  tt <- timber_tax(gross_revenue = 45000, cost_basis = 8000,
                    holding_period_years = 25, method = "631b")
  expect_s3_class(tt, "timber_tax")
  expect_equal(tt$depletion_amount, 8000)
  expect_equal(tt$capital_gain, 37000)
  expect_equal(tt$ordinary_income, 0)
  expect_equal(tt$tax_liability, 37000 * 0.15)
  expect_equal(tt$net_after_tax, 45000 - 37000 * 0.15)
  expect_equal(tt$method_used, "631b")
})

test_that("timber_tax 631a treats gain as capital gain for long holding", {
  tt <- timber_tax(gross_revenue = 30000, cost_basis = 5000,
                    holding_period_years = 10, method = "631a")
  expect_equal(tt$capital_gain, 25000)
  expect_equal(tt$ordinary_income, 0)
  expect_equal(tt$tax_liability, 25000 * 0.15)
})

test_that("timber_tax ordinary treats all gain as ordinary income", {
  tt <- timber_tax(gross_revenue = 30000, cost_basis = 5000,
                    holding_period_years = 10, method = "ordinary")
  expect_equal(tt$capital_gain, 0)
  expect_equal(tt$ordinary_income, 25000)
  expect_equal(tt$tax_liability, 25000 * 0.24)
})

test_that("timber_tax short holding period forces ordinary for 631b", {
  tt <- timber_tax(gross_revenue = 20000, cost_basis = 3000,
                    holding_period_years = 0.5, method = "631b")
  # held < 1 year: no capital gains treatment

  expect_equal(tt$capital_gain, 0)
  expect_equal(tt$ordinary_income, 17000)
})

test_that("timber_tax applies NIIT and state tax", {
  tt <- timber_tax(gross_revenue = 50000, cost_basis = 10000,
                    holding_period_years = 20, method = "631b",
                    state_tax_rate = 0.05, niit = TRUE)
  gain <- 40000
  expected_tax <- gain * 0.15 + gain * 0.038 + gain * 0.05
  expect_equal(tt$tax_liability, expected_tax)
})

test_that("timber_tax validates inputs", {
  expect_error(timber_tax(-100, 0, holding_period_years = 5))
  expect_error(timber_tax(100, -50, holding_period_years = 5))
  expect_error(timber_tax(100, 0, holding_period_years = 5,
                           marginal_tax_rate = 1.5))
})

test_that("timber_tax depletion capped at revenue", {
  tt <- timber_tax(gross_revenue = 5000, cost_basis = 20000,
                    holding_period_years = 10, method = "631b")
  expect_equal(tt$depletion_amount, 5000)  # capped at revenue
  expect_equal(tt$capital_gain, 0)         # no taxable gain
  expect_equal(tt$tax_liability, 0)
})

test_that("timber_tax detail has correct line items", {
  tt <- timber_tax(45000, 8000, holding_period_years = 25)
  expect_true(is.data.frame(tt$detail))
  expect_equal(nrow(tt$detail), 11)
  expect_true("Gross Revenue" %in% tt$detail$item)
  expect_true("Net After Tax" %in% tt$detail$item)
})

# =========================================================================
# depletion_schedule
# =========================================================================
test_that("depletion_schedule tracks basis correctly", {
  harvests <- data.frame(
    year = c(2020, 2025, 2030),
    volume = c(2500, 3000, 4500),
    revenue = c(600000, 780000, 1200000)
  )
  ds <- depletion_schedule(600000, 10000, harvests)
  expect_s3_class(ds, "depletion_schedule")
  expect_equal(ds$initial_basis, 600000)
  # Total depleted should equal initial basis (all volume harvested)
  expect_equal(ds$total_depleted, 600000)
  expect_equal(ds$remaining_basis, 0)
  expect_equal(nrow(ds$schedule), 3)
})

test_that("depletion_schedule partial harvest leaves remaining basis", {
  harvests <- data.frame(
    year = 2025, volume = 2000, revenue = 400000
  )
  ds <- depletion_schedule(600000, 10000, harvests)
  # Depletion rate = 600000/10000 = 60 per unit
  # Depletion = 60 * 2000 = 120000
  expect_equal(ds$schedule$depletion_rate[1], 60)
  expect_equal(ds$schedule$depletion_amount[1], 120000)
  expect_equal(ds$remaining_basis, 480000)
  expect_equal(ds$schedule$remaining_volume[1], 8000)
})

test_that("depletion_schedule sorts by year", {
  harvests <- data.frame(
    year = c(2030, 2020), volume = c(3000, 2000),
    revenue = c(500000, 300000)
  )
  ds <- depletion_schedule(100000, 10000, harvests)
  expect_equal(ds$schedule$year, c(2020, 2030))
})

test_that("depletion_schedule validates inputs", {
  harvests <- data.frame(year = 2025, volume = 100, revenue = 5000)
  expect_error(depletion_schedule(-100, 1000, harvests))
  expect_error(depletion_schedule(100, 0, harvests))
  expect_error(depletion_schedule(100, 1000, data.frame(year = 2025)))
})

test_that("depletion_schedule handles exhausted volume", {
  harvests <- data.frame(
    year = c(2020, 2025),
    volume = c(10000, 5000),  # second harvest exceeds remaining
    revenue = c(500000, 300000)
  )
  ds <- depletion_schedule(100000, 10000, harvests)
  # First harvest depletes entire basis (10000 of 10000 volume)
  expect_equal(ds$schedule$remaining_basis[1], 0)
  # Second harvest: no more basis to deplete
  expect_equal(ds$schedule$depletion_amount[2], 0)
  expect_equal(ds$schedule$taxable_gain[2], 300000)
})

# =========================================================================
# after_tax_npv
# =========================================================================
test_that("after_tax_npv returns lower NPV than pretax for positive revenue", {
  cf <- c(-750, rep(-50, 29), 8500)
  times <- 0:30
  result <- after_tax_npv(cf, times, discount_rate = 0.06,
                           cost_basis = 750, tax_method = "631b",
                           holding_period = 30)
  expect_true(result$npv_after_tax < result$npv_pretax)
  expect_true(result$tax_impact > 0)
  expect_true(result$effective_rate > 0)
  expect_true(result$effective_rate < 1)
})

test_that("after_tax_npv with zero basis taxes all revenue", {
  cf <- c(-1000, 5000)
  times <- c(0, 10)
  result <- after_tax_npv(cf, times, discount_rate = 0.06,
                           cost_basis = 0, tax_method = "631b",
                           holding_period = 10)
  # Tax on 5000 at 15% = 750
  expect_equal(result$year_detail$tax[2], 5000 * 0.15)
})

test_that("after_tax_npv ordinary method taxes more than 631b", {
  cf <- c(-1000, 10000)
  times <- c(0, 20)
  res_631b <- after_tax_npv(cf, times, 0.06, cost_basis = 1000,
                              tax_method = "631b", holding_period = 20)
  res_ord <- after_tax_npv(cf, times, 0.06, cost_basis = 1000,
                             tax_method = "ordinary", holding_period = 20)
  expect_true(res_631b$npv_after_tax > res_ord$npv_after_tax)
})

test_that("after_tax_npv validates input lengths", {
  expect_error(after_tax_npv(c(1, 2), c(0, 1, 2), 0.06))
})

test_that("after_tax_npv cost flows get tax deduction benefit", {
  cf <- c(-1000, 0)
  times <- c(0, 1)
  result <- after_tax_npv(cf, times, 0.06, cost_basis = 0,
                           marginal_rate = 0.24)
  # Cost of -1000 gets 24% tax savings = 240
  # After-tax flow = -1000 + 240 = -760
  expect_equal(result$year_detail$after_tax_flow[1], -760)
  expect_equal(result$year_detail$tax[1], -240)
})

# =========================================================================
# tax_comparison
# =========================================================================
test_that("tax_comparison compares all three methods", {
  tc <- tax_comparison(45000, 8000, 25)
  expect_s3_class(tc, "tax_comparison")
  expect_equal(nrow(tc$methods), 3)
  expect_true(all(c("631a", "631b", "ordinary") %in% tc$methods$method))
})

test_that("tax_comparison identifies best method", {
  tc <- tax_comparison(45000, 8000, 25)
  # For long holding period, 631a and 631b should beat ordinary
  expect_true(tc$best_method %in% c("631a", "631b"))
  expect_true(tc$savings_vs_ordinary > 0)
})

test_that("tax_comparison savings match method difference", {
  tc <- tax_comparison(45000, 8000, 25)
  ord_tax <- tc$methods$tax_liability[tc$methods$method == "ordinary"]
  best_tax <- tc$methods$tax_liability[tc$methods$method == tc$best_method]
  expect_equal(tc$savings_vs_ordinary, ord_tax - best_tax)
})

test_that("tax_comparison 631a and 631b identical for long holding", {
  tc <- tax_comparison(50000, 10000, 20)
  tax_a <- tc$methods$tax_liability[tc$methods$method == "631a"]
  tax_b <- tc$methods$tax_liability[tc$methods$method == "631b"]
  expect_equal(tax_a, tax_b)
})

test_that("tax_comparison has recommendation string", {
  tc <- tax_comparison(45000, 8000, 25)
  expect_true(nchar(tc$recommendation) > 0)
})

# =========================================================================
# reforestation_deduction
# =========================================================================
test_that("reforestation_deduction caps year 1 at $10,000", {
  rd <- reforestation_deduction(45000, tax_year = 2025)
  expect_s3_class(rd, "reforestation_deduction")
  expect_equal(rd$year_1_deduction, 10000)
  expect_equal(rd$year_1_savings, 10000 * 0.24)
})

test_that("reforestation_deduction small cost fully deducted year 1", {
  rd <- reforestation_deduction(5000, tax_year = 2025)
  expect_equal(rd$year_1_deduction, 5000)
  expect_equal(nrow(rd$amortization_schedule), 1)
  expect_equal(rd$total_tax_savings, 5000 * 0.24)
})

test_that("reforestation_deduction 84-month amortization schedule", {
  rd <- reforestation_deduction(45000, tax_year = 2025)
  sched <- rd$amortization_schedule
  expect_equal(nrow(sched), 8)  # years 2025-2032
  expect_equal(sched$year[1], 2025)
  expect_equal(sched$year[8], 2032)

  # Total deductions should equal total cost
  expect_equal(sum(sched$deduction), 45000, tolerance = 0.01)

  # Year 1: $10,000 direct + half-year amort (35000/14 = 2500)
  expect_equal(sched$deduction[1], 12500, tolerance = 0.01)
  # Years 2-7: full-year amort (35000/7 = 5000)
  expect_equal(sched$deduction[2], 5000, tolerance = 0.01)
  # Year 8: final half-year (35000/14 = 2500)
  expect_equal(sched$deduction[8], 2500, tolerance = 0.01)
})

test_that("reforestation_deduction effective subsidy rate reasonable", {
  rd <- reforestation_deduction(45000, tax_year = 2025)
  # At 24% marginal rate, subsidy rate = 24% of cost
  expect_equal(rd$effective_subsidy_rate, 0.24, tolerance = 0.001)
})

test_that("reforestation_deduction amortize_excess = FALSE", {
  rd <- reforestation_deduction(45000, tax_year = 2025, amortize_excess = FALSE)
  expect_equal(rd$year_1_deduction, 10000)
  expect_equal(nrow(rd$amortization_schedule), 1)
  expect_equal(rd$total_tax_savings, 10000 * 0.24)
})

test_that("reforestation_deduction validates inputs", {
  expect_error(reforestation_deduction(-100))
})

test_that("reforestation_deduction cumulative savings are monotonic", {
  rd <- reforestation_deduction(50000, tax_year = 2025)
  sched <- rd$amortization_schedule
  expect_true(all(diff(sched$cumulative_savings) > 0))
})
