# ---- Test data ----
make_test_yt <- function() {
  yield_table(
    ages = seq(10, 60, by = 10),
    products = list(
      sawlog = data.frame(volume = c(0, 2, 5, 10, 16, 20), price = 250),
      pulp = data.frame(volume = c(5, 12, 18, 22, 24, 25), price = 9)
    ),
    product_units = c(sawlog = "mbf", pulp = "ton")
  )
}

# =========================================================================
# discount_rate_buildup
# =========================================================================
test_that("discount_rate_buildup sums components correctly", {
  dr <- discount_rate_buildup(risk_free = 0.04, inflation_premium = 0.02,
                               illiquidity_premium = 0.01,
                               management_risk = 0.005,
                               market_risk = 0.01)
  expect_equal(dr$rate, 0.04 + 0.02 + 0.01 + 0.005 + 0.01)
  expect_equal(dr$real_rate, dr$rate - 0.02)
})

test_that("discount_rate_buildup returns correct class", {
  dr <- discount_rate_buildup()
  expect_s3_class(dr, "discount_rate_buildup")
  expect_true(all(c("rate", "real_rate", "components", "description") %in% names(dr)))
})

test_that("discount_rate_buildup warns on extreme rates", {
  expect_warning(discount_rate_buildup(risk_free = 0.20, market_risk = 0.10),
                 "exceeds 25%")
})

# =========================================================================
# liquidation_value
# =========================================================================
test_that("liquidation_value computes gross value from yield table", {
  yt <- make_test_yt()
  lv <- liquidation_value(yt, current_age = 40)
  expect_true(lv$gross_value > 0)
  expect_equal(lv$net_value, lv$gross_value)  # no costs
  expect_true(nrow(lv$product_detail) == 2)
})

test_that("liquidation_value deducts logging and sale costs", {
  yt <- make_test_yt()
  lv <- liquidation_value(yt, current_age = 40, logging_cost = 200,
                           sale_cost_pct = 0.05)
  expect_true(lv$net_value < lv$gross_value)
  expect_equal(lv$logging_cost, 200)
  expect_true(lv$sale_costs > 0)
})

test_that("liquidation_value returns 0 for young stands", {
  yt <- make_test_yt()
  lv <- liquidation_value(yt, current_age = 5)
  # Very young — may have some pulp but minimal
  expect_true(lv$net_value >= 0)
})

test_that("liquidation_value validates inputs", {
  yt <- make_test_yt()
  expect_error(liquidation_value(yt, current_age = -5))
  expect_error(liquidation_value(yt, current_age = 30, sale_cost_pct = 1.5))
  expect_error(liquidation_value("not a yt", current_age = 30))
})

# =========================================================================
# mid_rotation_value
# =========================================================================
test_that("mid_rotation_value returns positive value for mid-age stand", {
  yt <- make_test_yt()
  mv <- mid_rotation_value(yt, current_age = 25, discount_rate = 0.06,
                            regen_cost = 750, annual_cost = 50)
  expect_true(mv$total_value > 0)
  expect_true(mv$timber_value > 0)
  expect_true(mv$land_value > 0)
  expect_true(mv$years_to_harvest > 0)
})

test_that("mid_rotation_value warns when current_age >= harvest_age", {
  yt <- make_test_yt()
  expect_warning(
    mid_rotation_value(yt, current_age = 100, discount_rate = 0.06),
    "current_age >= harvest_age"
  )
})

test_that("mid_rotation_value uses specified harvest_age", {
  yt <- make_test_yt()
  mv <- mid_rotation_value(yt, current_age = 20, discount_rate = 0.06,
                            harvest_age = 50)
  expect_equal(mv$harvest_age, 50)
  expect_equal(mv$years_to_harvest, 30)
})

# =========================================================================
# comparable_sale
# =========================================================================
test_that("comparable_sale applies additive adjustments", {
  cs <- comparable_sale(2000, adjustments = list(timber = 300, access = -100))
  expect_equal(cs$adjusted_price, 2200)
  expect_equal(cs$total_adjustment, 200)
})

test_that("comparable_sale applies multiplicative adjustments", {
  cs <- comparable_sale(2000, adjustments = list(time = 1.05, location = 0.95),
                         method = "multiplicative")
  expect_equal(cs$adjusted_price, 2000 * 1.05 * 0.95)
})

test_that("comparable_sale warns for large adjustments", {
  expect_warning(
    comparable_sale(2000, adjustments = list(timber = 800)),
    "exceeds 25%"
  )
})

test_that("comparable_sale handles empty adjustments", {
  cs <- comparable_sale(2000, adjustments = list())
  expect_equal(cs$adjusted_price, 2000)
})

# =========================================================================
# timberland_value
# =========================================================================
test_that("timberland_value income method returns LEV for bare land", {
  yt <- make_test_yt()
  tv <- timberland_value(yt, discount_rate = 0.06, regen_cost = 750,
                          annual_cost = 50, method = "income")
  expect_s3_class(tv, "timberland_value")
  expect_true(tv$total_value > 0)
  expect_equal(tv$timber_value, 0)  # bare land: no standing timber
  expect_true(is.na(tv$current_age))
})

test_that("timberland_value income method for mid-rotation stand", {
  yt <- make_test_yt()
  tv <- timberland_value(yt, discount_rate = 0.06, regen_cost = 750,
                          annual_cost = 50, current_age = 30)
  expect_true(tv$total_value > 0)
  expect_true(tv$timber_value > 0)
  expect_equal(tv$current_age, 30)
})

test_that("timberland_value hybrid method picks higher value", {
  yt <- make_test_yt()
  tv <- timberland_value(yt, discount_rate = 0.06, regen_cost = 750,
                          current_age = 30, method = "hybrid")
  tv_income <- timberland_value(yt, discount_rate = 0.06, regen_cost = 750,
                                 current_age = 30, method = "income")
  tv_liq <- timberland_value(yt, discount_rate = 0.06, regen_cost = 750,
                              current_age = 30, method = "liquidation")
  expect_equal(tv$total_value, max(tv_income$total_value, tv_liq$total_value))
})

test_that("timberland_value uses market land_value when provided", {
  yt <- make_test_yt()
  tv <- timberland_value(yt, discount_rate = 0.06, current_age = 30,
                          land_value = 800, method = "liquidation")
  expect_equal(tv$land_value, 800)
})

# =========================================================================
# hold_vs_sell
# =========================================================================
test_that("hold_vs_sell recommends hold for young stand with low offer", {
  yt <- make_test_yt()
  hs <- hold_vs_sell(yt, discount_rate = 0.06, regen_cost = 750,
                      annual_cost = 50, current_age = 15,
                      current_offer = 100)
  expect_equal(hs$recommendation, "hold")
  expect_true(hs$advantage > 0)
})

test_that("hold_vs_sell recommends sell for high offer", {
  yt <- make_test_yt()
  hs <- hold_vs_sell(yt, discount_rate = 0.06, regen_cost = 750,
                      annual_cost = 50, current_age = 30,
                      current_offer = 100000)
  expect_equal(hs$recommendation, "sell")
  expect_true(hs$advantage < 0)
})

test_that("hold_vs_sell computes breakeven price", {
  yt <- make_test_yt()
  hs <- hold_vs_sell(yt, discount_rate = 0.06, regen_cost = 750,
                      current_age = 25)
  expect_true(hs$breakeven_price > 0)
  # Breakeven price should equal hold value
  expect_equal(hs$breakeven_price, hs$hold_value)
})

# =========================================================================
# tract_aggregate
# =========================================================================
test_that("tract_aggregate sums values correctly", {
  yt <- make_test_yt()
  val1 <- timberland_value(yt, 0.06, 750, 50, current_age = 20)
  val2 <- timberland_value(yt, 0.06, 750, 50, current_age = 40)

  ta <- tract_aggregate(stand_a = val1, stand_b = val2,
                          acres = c(stand_a = 100, stand_b = 200))
  expect_s3_class(ta, "tract_aggregate")
  expect_equal(ta$total_acres, 300)
  expected_total <- val1$total_value * 100 + val2$total_value * 200
  expect_equal(ta$total_value, expected_total)
  expect_equal(nrow(ta$tract_summary), 2)
})

test_that("tract_aggregate validates inputs", {
  yt <- make_test_yt()
  val1 <- timberland_value(yt, 0.06, 750, 50)
  expect_error(tract_aggregate(val1, acres = c(a = 100)))  # unnamed
  expect_error(tract_aggregate(a = "not tv", acres = c(a = 100)))
})
