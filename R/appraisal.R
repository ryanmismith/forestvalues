# Timberland appraisal and divestment analysis functions
#
# Functions for valuing timberland properties, comparing hold-vs-sell
# decisions, aggregating multi-stand portfolios, and constructing
# discount rates.


#' Discount Rate Build-Up Method
#'
#' Constructs a timberland discount rate by summing component risk premia.
#' This is the standard practitioner approach for illiquid real assets like
#' timberland, where CAPM is inappropriate due to infrequent transactions
#' and non-traded markets.
#'
#' @param risk_free Numeric. Risk-free rate (e.g., 10-year Treasury yield).
#'   Default 0.04.
#' @param inflation_premium Numeric. Expected inflation premium. Default 0.02.
#' @param illiquidity_premium Numeric. Premium for timberland's illiquidity
#'   (cannot sell quickly at fair value). Default 0.01.
#' @param management_risk Numeric. Operational/biological risk (fire, pest,
#'   weather, regeneration failure). Default 0.005.
#' @param market_risk Numeric. Timber price volatility premium. Default 0.01.
#' @param property_specific Numeric. Tract-specific adjustment (access issues,
#'   regulatory risk, title concerns). Default 0.
#' @param tax_adjustment Numeric. After-tax rate adjustment (negative if
#'   converting pre-tax to after-tax). Default 0.
#'
#' @return A list of class \code{"discount_rate_buildup"} with:
#' \describe{
#'   \item{rate}{Total nominal discount rate (sum of all components)}
#'   \item{real_rate}{Real rate (total minus inflation premium)}
#'   \item{components}{Named numeric vector of each component}
#'   \item{description}{Character vector explaining each component}
#' }
#'
#' @details
#' The build-up method is widely used in timberland appraisal because:
#' \enumerate{
#'   \item Timberland is not publicly traded (no beta for CAPM)
#'   \item Transaction costs are high (5-10\% of value)
#'   \item Each property is unique (species, age, location, access)
#'   \item Returns are partly biological (growth) and partly market (prices)
#' }
#'
#' **Typical ranges for U.S. timberland (2020s):**
#' \itemize{
#'   \item Risk-free: 3-5\% (10-year Treasury)
#'   \item Inflation: 2-3\%
#'   \item Illiquidity: 0.5-2\%
#'   \item Management/biological risk: 0.5-1.5\%
#'   \item Market risk: 0.5-2\%
#'   \item Total nominal: 7-13\%
#'   \item Total real: 5-10\%
#' }
#'
#' The \code{real_rate} component is what you should use as the
#' \code{discount_rate} argument in \code{\link{timberland_value}},
#' \code{\link{lev}}, and other functions (which assume real cash flows).
#'
#' @references
#' Bullard, S.H. & Straka, T.J. (2011). *Basic Concepts in Forest Valuation
#' and Investment Analysis*. 3rd ed. Ch. 2.
#'
#' Wagner, J.E. (2012). *Forestry Economics: A Managerial Approach*. Routledge.
#' Ch. 6.
#'
#' @examples
#' # Conservative timberland rate
#' dr <- discount_rate_buildup(risk_free = 0.045, inflation_premium = 0.025)
#' dr$rate       # ~9%
#' dr$real_rate   # ~6.5%
#'
#' # Using the rate_components() benchmark dataset
#' rc <- rate_components()
#' dr <- discount_rate_buildup(
#'   risk_free = rc$typical_mid[1],
#'   inflation_premium = rc$typical_mid[2],
#'   illiquidity_premium = rc$typical_mid[3],
#'   management_risk = rc$typical_mid[4],
#'   market_risk = rc$typical_mid[5],
#'   property_specific = rc$typical_mid[6]
#' )
#'
#' # Use the real rate in LEV calculation
#' lev(52000, rotation_age = 40, discount_rate = dr$real_rate)
#'
#' @family appraisal
#' @export
discount_rate_buildup <- function(risk_free = 0.04,
                                    inflation_premium = 0.02,
                                    illiquidity_premium = 0.01,
                                    management_risk = 0.005,
                                    market_risk = 0.01,
                                    property_specific = 0,
                                    tax_adjustment = 0) {
  components <- c(
    risk_free = risk_free,
    inflation = inflation_premium,
    illiquidity = illiquidity_premium,
    management_risk = management_risk,
    market_risk = market_risk,
    property_specific = property_specific,
    tax_adjustment = tax_adjustment
  )

  for (nm in names(components)) {
    if (!is.numeric(components[nm]) || length(components[nm]) != 1) {
      stop("'", nm, "' must be a single number", call. = FALSE)
    }
  }

  rate <- sum(components)
  real_rate <- rate - inflation_premium

  if (rate < 0) warning("Total rate is negative. Check inputs.", call. = FALSE)
  if (rate > 0.25) warning("Total rate exceeds 25%. Verify assumptions.", call. = FALSE)

  descriptions <- c(
    risk_free = paste0("Risk-free rate (e.g., Treasury): ", round(risk_free * 100, 2), "%"),
    inflation = paste0("Expected inflation: ", round(inflation_premium * 100, 2), "%"),
    illiquidity = paste0("Illiquidity premium (timberland): ", round(illiquidity_premium * 100, 2), "%"),
    management_risk = paste0("Management/biological risk: ", round(management_risk * 100, 2), "%"),
    market_risk = paste0("Timber price volatility: ", round(market_risk * 100, 2), "%"),
    property_specific = paste0("Property-specific adjustment: ", round(property_specific * 100, 2), "%"),
    tax_adjustment = paste0("Tax adjustment: ", round(tax_adjustment * 100, 2), "%")
  )

  structure(
    list(
      rate = rate,
      real_rate = real_rate,
      components = components,
      description = descriptions
    ),
    class = "discount_rate_buildup"
  )
}


#' Timberland Income-Approach Valuation
#'
#' Computes the value of a timberland property using the income approach,
#' which is the standard appraisal method for commercial timberland. The
#' income approach capitalizes expected future timber income into a present
#' value.
#'
#' @param yield_tbl A \code{yield_table} object.
#' @param discount_rate Numeric. Real discount rate.
#' @param regen_cost Numeric. Regeneration cost ($/acre). Default 0.
#' @param annual_cost Numeric. Annual carrying cost ($/acre/yr). Default 0.
#' @param current_age Numeric or NULL. Current stand age. NULL means
#'   bare land (no existing timber). Default NULL.
#' @param land_value Numeric or NULL. If provided, overrides the LEV-based
#'   bare land value with a market-derived value (e.g., from comparable
#'   sales). Default NULL.
#' @param method Character. Valuation method:
#'   \describe{
#'     \item{"income"}{(Default) Faustmann income approach. If bare land,
#'       returns LEV. If mid-rotation, returns anticipated value.}
#'     \item{"liquidation"}{Immediate harvest value + bare land value.}
#'     \item{"hybrid"}{Maximum of income and liquidation — the
#'       highest-and-best-use value.}
#'   }
#'
#' @return A list of class \code{"timberland_value"} with:
#' \describe{
#'   \item{total_value}{Combined per-acre value ($/acre)}
#'   \item{timber_value}{PV of standing timber component}
#'   \item{land_value}{Bare land value (LEV or market-based)}
#'   \item{method}{Which valuation method was used}
#'   \item{optimal_harvest_age}{From optimal_rotation_mp}
#'   \item{current_age}{Stand age (or NA if bare land)}
#'   \item{components}{Data frame breaking down the valuation}
#' }
#'
#' @details
#' **Income method (Faustmann):**
#' For bare land (\code{current_age = NULL}), returns the Land Expectation
#' Value (LEV) — the PV of perpetual timber management. For mid-rotation
#' stands, uses the anticipated value approach from Klemperer (1996).
#'
#' **Liquidation method:**
#' Adds the immediate stumpage value (harvest now) to the bare land value.
#' Appropriate when a stand is past financial maturity.
#'
#' **Hybrid method:**
#' Takes the maximum of income and liquidation. This follows the
#' highest-and-best-use principle: if a stand is worth more cut today
#' than managed, rational behavior is to cut.
#'
#' @references
#' Klemperer, W.D. (1996). *Forest Resource Economics and Finance*. Ch. 9, 14.
#'
#' Bullard, S.H. & Straka, T.J. (2011). *Basic Concepts in Forest Valuation
#' and Investment Analysis*. 3rd ed. Ch. 8.
#'
#' @examples
#' # Using the pine_yields() example dataset
#' yt <- pine_yields()
#'
#' # Bare land value
#' timberland_value(yt, discount_rate = 0.06, regen_cost = 750, annual_cost = 50)
#'
#' # 30-year-old stand
#' timberland_value(yt, discount_rate = 0.06, regen_cost = 750,
#'                   annual_cost = 50, current_age = 30)
#'
#' # Hybrid with market-based land value
#' timberland_value(yt, discount_rate = 0.06, regen_cost = 750,
#'                   current_age = 30, land_value = 800, method = "hybrid")
#'
#' @seealso \code{\link{mid_rotation_value}}, \code{\link{liquidation_value}},
#'   \code{\link{lev}}, \code{\link{hold_vs_sell}}
#' @family appraisal
#' @export
timberland_value <- function(yield_tbl, discount_rate, regen_cost = 0,
                              annual_cost = 0, current_age = NULL,
                              land_value = NULL,
                              method = c("income", "liquidation", "hybrid")) {
  method <- match.arg(method)

  if (!inherits(yield_tbl, "yield_table")) {
    stop("'yield_tbl' must be a yield_table object", call. = FALSE)
  }
  if (discount_rate <= 0) {
    stop("'discount_rate' must be positive", call. = FALSE)
  }

  # Optimal rotation
  opt <- optimal_rotation_mp(yield_tbl, regen_cost, annual_cost, discount_rate)
  opt_age <- opt$optimal_age

  # LEV (bare land value) — either computed or user-supplied
  if (is.null(land_value)) {
    blv <- opt$value_at_optimum  # LEV from optimal_rotation_mp
  } else {
    blv <- land_value
  }

  is_bare_land <- is.null(current_age)

  # ---- Income method ----
  if (is_bare_land) {
    income_value <- blv
    timber_val <- 0
    land_val <- blv
  } else {
    mrv <- mid_rotation_value(yield_tbl, current_age, discount_rate,
                               regen_cost, annual_cost, harvest_age = opt_age)
    income_value <- mrv$total_value
    timber_val <- mrv$timber_value
    land_val <- mrv$land_value
  }

  # ---- Liquidation method ----
  if (!is_bare_land) {
    liq <- liquidation_value(yield_tbl, current_age)
    liq_value <- liq$net_value + blv
    liq_timber <- liq$net_value
  } else {
    liq_value <- blv
    liq_timber <- 0
  }

  # ---- Select result based on method ----
  if (method == "income") {
    total <- income_value
    tv <- timber_val
    lv <- land_val
  } else if (method == "liquidation") {
    total <- liq_value
    tv <- liq_timber
    lv <- blv
  } else {
    # Hybrid: highest and best use
    if (liq_value > income_value) {
      total <- liq_value
      tv <- liq_timber
      lv <- blv
      method <- "hybrid (liquidation wins)"
    } else {
      total <- income_value
      tv <- timber_val
      lv <- land_val
      method <- "hybrid (income wins)"
    }
  }

  # Components breakdown
  components <- data.frame(
    component = c("Timber (standing)", "Land (bare)", "Total"),
    value = c(tv, lv, total),
    pct_of_total = c(
      if (total > 0) tv / total else 0,
      if (total > 0) lv / total else 0,
      1.0
    ),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      total_value = total,
      timber_value = tv,
      land_value = lv,
      method = method,
      optimal_harvest_age = opt_age,
      current_age = if (is_bare_land) NA_real_ else current_age,
      components = components
    ),
    class = "timberland_value"
  )
}


#' Hold vs. Sell Decision Analysis
#'
#' Compares the value of **holding** a timber asset (harvesting later at the
#' optimal time) against **selling** it now. This is the core divestment
#' decision function. Also computes breakeven values: the minimum sale price
#' that makes selling preferable, and the reinvestment rate that equates
#' holding and selling.
#'
#' @param yield_tbl A \code{yield_table} object.
#' @param discount_rate Numeric. Real discount rate.
#' @param regen_cost Numeric. Regeneration cost. Default 0.
#' @param annual_cost Numeric. Annual carrying cost. Default 0.
#' @param current_age Numeric. Current stand age.
#' @param current_offer Numeric or NULL. Current offer for the timber/land.
#'   If NULL, uses the liquidation value as the sell price. Default NULL.
#' @param reinvest_rate Numeric or NULL. Rate earned on sale proceeds if
#'   you sell. If NULL, assumes equal to discount_rate (proceeds invested
#'   at same return). Default NULL.
#' @param holding_period Numeric or NULL. Time horizon for comparison.
#'   If NULL, uses years from current age to optimal rotation. Default NULL.
#'
#' @return A list of class \code{"hold_vs_sell"} with:
#' \describe{
#'   \item{recommendation}{"hold" or "sell"}
#'   \item{hold_value}{PV of holding (harvest at optimal time + future LEV)}
#'   \item{sell_value}{Value of selling now (offer price)}
#'   \item{advantage}{hold_value - sell_value (positive favors holding)}
#'   \item{advantage_pct}{Advantage as \% of sell value}
#'   \item{breakeven_price}{Minimum offer that makes selling preferable}
#'   \item{breakeven_rate}{Reinvestment rate that equates hold and sell}
#'   \item{years_to_harvest}{Remaining years if holding}
#' }
#'
#' @details
#' **Hold value** is computed as the mid-rotation value: PV of future harvest
#' revenue plus PV of bare land (for perpetual management) minus PV of
#' carrying costs during the wait period.
#'
#' **Sell value** is the current offer (or liquidation value if no offer).
#'
#' **Breakeven price:** The sale price at which \eqn{V_{sell} = V_{hold}}.
#' Anything above this price makes selling the better choice.
#'
#' **Breakeven reinvestment rate:** If you sell, you'll reinvest the proceeds.
#' This is the rate you'd need to earn on those proceeds to match the return
#' from holding. If your alternative investments earn less than this rate,
#' hold the timber.
#'
#' @references
#' Klemperer, W.D. (1996). *Forest Resource Economics and Finance*. Ch. 6
#' (Optimal Holding Period for a Current Asset).
#'
#' @examples
#' # Using the pine_yields() example dataset
#' yt <- pine_yields()
#'
#' # Should you sell a 25-year-old stand for $3,000/acre?
#' hold_vs_sell(yt, discount_rate = 0.06, regen_cost = 750,
#'              annual_cost = 50, current_age = 25,
#'              current_offer = 3000)
#'
#' @seealso \code{\link{timberland_value}}, \code{\link{mid_rotation_value}}
#' @family appraisal
#' @export
hold_vs_sell <- function(yield_tbl, discount_rate, regen_cost = 0,
                          annual_cost = 0, current_age, current_offer = NULL,
                          reinvest_rate = NULL, holding_period = NULL) {
  if (!inherits(yield_tbl, "yield_table")) {
    stop("'yield_tbl' must be a yield_table object", call. = FALSE)
  }
  if (!is.numeric(current_age) || current_age < 0) {
    stop("'current_age' must be a non-negative number", call. = FALSE)
  }

  r <- discount_rate
  if (is.null(reinvest_rate)) reinvest_rate <- r

  # Hold value = mid-rotation value (anticipated harvest + land + carry costs)
  mrv <- mid_rotation_value(yield_tbl, current_age, r, regen_cost,
                             annual_cost, harvest_age = NULL)
  hold_value <- mrv$total_value

  # Sell value = current offer or liquidation
  if (is.null(current_offer)) {
    liq <- liquidation_value(yield_tbl, current_age)
    sell_value <- liq$net_value
  } else {
    sell_value <- current_offer
  }

  # Comparison
  advantage <- hold_value - sell_value
  advantage_pct <- if (sell_value != 0) advantage / sell_value else Inf

  recommendation <- if (hold_value >= sell_value) "hold" else "sell"

  # Breakeven price: the offer that would make selling = holding
  breakeven_price <- hold_value

  # Breakeven reinvestment rate: what rate on proceeds = hold value?
  years <- mrv$years_to_harvest
  if (is.null(holding_period)) holding_period <- years

  # Solve: sell_value * (1 + r_reinvest)^n / (1+r)^n = hold_value
  # => (1 + r_reinvest)^n = hold_value * (1+r)^n / sell_value
  breakeven_reinvest <- if (sell_value > 0 && holding_period > 0) {
    tryCatch({
      target_fv <- hold_value * (1 + r)^holding_period
      (target_fv / sell_value)^(1 / holding_period) - 1
    }, error = function(e) NA_real_)
  } else {
    NA_real_
  }

  structure(
    list(
      recommendation = recommendation,
      hold_value = hold_value,
      sell_value = sell_value,
      advantage = advantage,
      advantage_pct = advantage_pct,
      breakeven_price = breakeven_price,
      breakeven_rate = breakeven_reinvest,
      years_to_harvest = years,
      holding_period = holding_period,
      harvest_revenue = mrv$harvest_revenue
    ),
    class = "hold_vs_sell"
  )
}


#' Aggregate Multiple Tract Valuations
#'
#' Combines multiple \code{\link{timberland_value}} results into a
#' property-level or portfolio-level summary. Handles tracts of different
#' sizes, species, ages, and values.
#'
#' @param ... Named \code{timberland_value} objects, or a single named list
#'   of them.
#' @param acres Named numeric vector of acres per tract. Names must match
#'   the tract names in \code{...}. Required.
#' @param weights Character. How to weight per-acre values:
#'   \code{"acres"} (default, area-weighted), \code{"equal"}, or
#'   \code{"custom"}.
#' @param custom_weights Named numeric vector. Required if
#'   \code{weights = "custom"}.
#'
#' @return A list of class \code{"tract_aggregate"} with:
#' \describe{
#'   \item{total_value}{Total property value ($)}
#'   \item{per_acre_value}{Weighted average $/acre}
#'   \item{total_acres}{Sum of all tract acres}
#'   \item{tract_summary}{Data frame: tract, acres, per_acre_value,
#'     total_value, pct_of_portfolio, timber_pct (timber's share of
#'     tract value)}
#' }
#'
#' @details
#' Timberland properties are rarely homogeneous. A typical 5,000-acre
#' property might contain 15-year-old pine plantations, 60-year-old
#' hardwood stands, recently harvested tracts, and non-productive areas.
#' Each stand is valued independently, then aggregated here.
#'
#' **Area-weighted average** (default) gives larger tracts proportionally
#' more influence on the per-acre result.
#'
#' @examples
#' \dontrun{
#' # Value three different stands on the same property
#' val_pine <- timberland_value(yt_pine, 0.06, 550, 12, current_age = 18)
#' val_hardwood <- timberland_value(yt_hw, 0.05, 200, 8, current_age = 55)
#' val_recent <- timberland_value(yt_pine, 0.06, 550, 12, current_age = 3)
#'
#' tract_aggregate(
#'   pine = val_pine, hardwood = val_hardwood, recent = val_recent,
#'   acres = c(pine = 2000, hardwood = 1500, recent = 800)
#' )
#' }
#'
#' @seealso \code{\link{timberland_value}}
#' @family appraisal
#' @export
tract_aggregate <- function(..., acres, weights = c("acres", "equal", "custom"),
                             custom_weights = NULL) {
  weights <- match.arg(weights)

  # Collect tracts
  dots <- list(...)
  if (length(dots) == 1 && is.list(dots[[1]]) &&
      !inherits(dots[[1]], "timberland_value")) {
    tracts <- dots[[1]]
  } else {
    tracts <- dots
  }

  if (is.null(names(tracts)) || any(names(tracts) == "")) {
    stop("All tracts must be named", call. = FALSE)
  }
  for (tn in names(tracts)) {
    if (!inherits(tracts[[tn]], "timberland_value")) {
      stop("'", tn, "' is not a timberland_value object", call. = FALSE)
    }
  }

  tract_names <- names(tracts)
  if (!all(tract_names %in% names(acres))) {
    stop("'acres' must include an entry for every tract", call. = FALSE)
  }

  # Build summary
  n <- length(tracts)
  summary_df <- data.frame(
    tract = character(n),
    acres = numeric(n),
    per_acre_value = numeric(n),
    total_value = numeric(n),
    timber_pct = numeric(n),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(tracts)) {
    tn <- tract_names[i]
    tv <- tracts[[tn]]
    ac <- acres[[tn]]

    summary_df$tract[i] <- tn
    summary_df$acres[i] <- ac
    summary_df$per_acre_value[i] <- tv$total_value
    summary_df$total_value[i] <- tv$total_value * ac
    summary_df$timber_pct[i] <- if (tv$total_value > 0) {
      tv$timber_value / tv$total_value
    } else 0
  }

  total_value <- sum(summary_df$total_value)
  total_acres <- sum(summary_df$acres)
  summary_df$pct_of_portfolio <- if (total_value > 0) {
    summary_df$total_value / total_value
  } else {
    rep(0, n)
  }

  # Weighted per-acre value
  if (weights == "acres") {
    per_acre <- total_value / total_acres
  } else if (weights == "equal") {
    per_acre <- mean(summary_df$per_acre_value)
  } else {
    if (is.null(custom_weights)) {
      stop("'custom_weights' required when weights = 'custom'", call. = FALSE)
    }
    w <- custom_weights[tract_names]
    per_acre <- sum(summary_df$per_acre_value * w) / sum(w)
  }

  structure(
    list(
      total_value = total_value,
      per_acre_value = per_acre,
      total_acres = total_acres,
      tract_summary = summary_df
    ),
    class = "tract_aggregate"
  )
}
