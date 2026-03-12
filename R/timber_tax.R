# U.S. Timber Taxation Functions
#
# Functions for computing after-tax timber income under IRC Section 631,
# tracking cost basis depletion, comparing tax methods, and modeling
# reforestation tax incentives.
#
# References:
#   IRS Publication 535 (Business Expenses)
#   National Timber Tax Website (timbertax.org)
#   Siegel, W.C. et al. (2009). Federal Income Tax on Timber.
#   Hoover, W.L. (2017). Timber Tax Management for CPAs.
#   Haney, H.L. Jr. et al. (2001). Forest Landowners' Guide to the
#     Federal Income Tax.


#' Timber Tax Computation (IRC Section 631)
#'
#' Computes after-tax timber income under U.S. federal rules. Supports
#' Section 631(a) (standing timber treated as capital gain), Section 631(b)
#' (outright sale of standing timber), and ordinary income treatment.
#'
#' @param gross_revenue Numeric. Gross timber sale proceeds ($).
#' @param cost_basis Numeric. Adjusted cost basis of the timber ($).
#'   This is typically the original purchase price allocated to timber,
#'   or the reforestation cost, adjusted for prior depletion.
#' @param depletion_basis Numeric or NULL. If different from cost_basis,
#'   the separate depletion account. NULL means use cost_basis. Default NULL.
#' @param holding_period_years Numeric. Years the timber has been held.
#'   Must be > 1 year for long-term capital gains under 631(b).
#' @param marginal_tax_rate Numeric. Taxpayer's marginal ordinary income
#'   rate. Default 0.24 (24\% bracket).
#' @param capital_gains_rate Numeric. Long-term capital gains rate.
#'   Default 0.15 (most common bracket).
#' @param method Character. Tax election method:
#'   \describe{
#'     \item{"631a"}{(Default) Section 631(a) — owner cuts or has timber
#'       cut for sale. Gain = FMV at beginning of tax year minus adjusted
#'       basis. Taxed at capital gains rate. Most favorable for owner-operators.}
#'     \item{"631b"}{Section 631(b) — outright disposal of standing timber
#'       with economic interest retained. Gain = amount realized minus
#'       adjusted basis. Capital gains if held > 1 year.}
#'     \item{"ordinary"}{No 631 election. All gain taxed at ordinary rates.
#'       Baseline for comparison.}
#'   }
#' @param state_tax_rate Numeric. Combined state income tax rate. Default 0.
#' @param niit Logical. Apply 3.8\% Net Investment Income Tax (for AGI >
#'   $200K single / $250K married). Default FALSE.
#'
#' @return A list of class \code{"timber_tax"} with:
#' \describe{
#'   \item{net_after_tax}{After-tax proceeds ($)}
#'   \item{tax_liability}{Total federal + state tax ($)}
#'   \item{effective_rate}{Tax as fraction of gross revenue}
#'   \item{depletion_amount}{Basis recovered (not taxed)}
#'   \item{capital_gain}{Amount taxed at capital gains rate}
#'   \item{ordinary_income}{Amount taxed at ordinary rate}
#'   \item{method_used}{Which Section 631 method}
#'   \item{detail}{Data frame with line-by-line breakdown}
#' }
#'
#' @details
#' **Section 631(a) — Cutting of timber:**
#' The taxpayer elects to treat cutting as a sale. Gain = FMV on first day
#' of the tax year minus adjusted depletion basis. The gain qualifies as
#' Section 1231 gain (long-term capital gains rate) if the timber was held
#' > 1 year. Any amount received in excess of FMV is ordinary income.
#'
#' For this function, we approximate FMV as the gross revenue (the typical
#' case where harvest and sale occur in the same year).
#'
#' **Section 631(b) — Disposal with retained economic interest:**
#' Outright sale of standing timber where the owner retains an economic
#' interest (e.g., pay-as-cut contract). Gain = amount realized minus
#' adjusted basis. All gain is Section 1231 if held > 1 year.
#'
#' **Depletion:**
#' Cost depletion recovers the timber's cost basis. The depleted amount
#' reduces taxable gain dollar-for-dollar. Depletion = min(basis, gain).
#'
#' @references
#' Siegel, W.C., Haney, H.L. Jr., & Greene, J.L. (2009). *Federal Income
#' Tax on Timber: A Key to Your Most Frequently Asked Questions*.
#' USDA Forest Service GTR-SRS-112.
#'
#' Hoover, W.L. (2017). *Timber Tax Management for CPAs*. Purdue Extension.
#'
#' @examples
#' # Landowner sells standing timber from a 25-year-old plantation
#' timber_tax(gross_revenue = 45000, cost_basis = 8000,
#'            holding_period_years = 25, method = "631b")
#'
#' # Compare all three methods
#' tax_comparison(45000, 8000, 25)
#'
#' @seealso \code{\link{tax_comparison}}, \code{\link{depletion_schedule}},
#'   \code{\link{after_tax_npv}}
#' @export
timber_tax <- function(gross_revenue, cost_basis, depletion_basis = NULL,
                        holding_period_years,
                        marginal_tax_rate = 0.24,
                        capital_gains_rate = 0.15,
                        method = c("631a", "631b", "ordinary"),
                        state_tax_rate = 0,
                        niit = FALSE) {
  method <- match.arg(method)

  if (gross_revenue < 0) stop("'gross_revenue' cannot be negative", call. = FALSE)
  if (cost_basis < 0) stop("'cost_basis' cannot be negative", call. = FALSE)
  if (marginal_tax_rate < 0 || marginal_tax_rate > 1) {
    stop("'marginal_tax_rate' must be between 0 and 1", call. = FALSE)
  }
  if (capital_gains_rate < 0 || capital_gains_rate > 1) {
    stop("'capital_gains_rate' must be between 0 and 1", call. = FALSE)
  }

  if (is.null(depletion_basis)) depletion_basis <- cost_basis

  # Depletion: recover the cost basis
  depletion <- min(depletion_basis, gross_revenue)

  # Taxable gain
  taxable_gain <- gross_revenue - depletion

  # Classify income based on method
  if (method == "631a") {
    # Section 631(a): all gain from cutting is Section 1231 (cap gains)
    # if held > 1 year
    if (holding_period_years > 1) {
      capital_gain <- taxable_gain
      ordinary_income <- 0
    } else {
      capital_gain <- 0
      ordinary_income <- taxable_gain
    }
  } else if (method == "631b") {
    # Section 631(b): disposal gain is Section 1231 if held > 1 year
    if (holding_period_years > 1) {
      capital_gain <- taxable_gain
      ordinary_income <- 0
    } else {
      capital_gain <- 0
      ordinary_income <- taxable_gain
    }
  } else {
    # Ordinary income treatment
    capital_gain <- 0
    ordinary_income <- taxable_gain
  }

  # Tax computation
  fed_tax_cg <- capital_gain * capital_gains_rate
  fed_tax_oi <- ordinary_income * marginal_tax_rate
  niit_tax <- if (niit) taxable_gain * 0.038 else 0
  state_tax <- taxable_gain * state_tax_rate

  total_tax <- fed_tax_cg + fed_tax_oi + niit_tax + state_tax
  net_after_tax <- gross_revenue - total_tax
  effective_rate <- if (gross_revenue > 0) total_tax / gross_revenue else 0

  # Detail breakdown
  detail <- data.frame(
    item = c("Gross Revenue", "Cost Basis Depletion", "Taxable Gain",
             "Capital Gain Portion", "Ordinary Income Portion",
             "Federal Tax (Capital Gains)", "Federal Tax (Ordinary)",
             "NIIT (3.8%)", "State Tax", "Total Tax", "Net After Tax"),
    amount = c(gross_revenue, depletion, taxable_gain,
               capital_gain, ordinary_income,
               fed_tax_cg, fed_tax_oi, niit_tax, state_tax,
               total_tax, net_after_tax),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      net_after_tax = net_after_tax,
      tax_liability = total_tax,
      effective_rate = effective_rate,
      depletion_amount = depletion,
      capital_gain = capital_gain,
      ordinary_income = ordinary_income,
      method_used = method,
      detail = detail
    ),
    class = "timber_tax"
  )
}


#' Timber Depletion Schedule
#'
#' Tracks cost basis depletion over multiple harvests. Essential for
#' landowners who harvest portions of their timber over several years,
#' as the IRS requires cost depletion to be tracked per harvest.
#'
#' @param initial_basis Numeric. Total cost basis of the timber account ($).
#' @param initial_volume Numeric. Total merchantable volume at time of
#'   acquisition (or most recent timber cruise).
#' @param harvests Data frame with columns:
#'   \describe{
#'     \item{year}{Harvest year (numeric)}
#'     \item{volume}{Volume harvested (same units as initial_volume)}
#'     \item{revenue}{Gross revenue from the harvest ($)}
#'   }
#' @param method Character. Depletion method: \code{"cost"} (default) or
#'   \code{"percentage"}. Cost depletion is the standard for timber.
#'
#' @return A list of class \code{"depletion_schedule"} with:
#' \describe{
#'   \item{schedule}{Data frame: year, volume_harvested, depletion_rate,
#'     depletion_amount, taxable_gain, remaining_basis, remaining_volume}
#'   \item{total_depleted}{Total basis recovered ($)}
#'   \item{remaining_basis}{Basis remaining after all harvests ($)}
#'   \item{initial_basis}{Original basis}
#' }
#'
#' @details
#' **Cost depletion** is computed as:
#'
#' \deqn{\text{Depletion rate} = \frac{\text{Adjusted basis}}{\text{Remaining volume}}}
#' \deqn{\text{Depletion} = \text{Rate} \times \text{Volume harvested}}
#'
#' The depletion rate is recalculated before each harvest because the
#' remaining basis and volume change. This is a "units of production"
#' method specific to natural resources.
#'
#' **Important:** The depletion amount cannot exceed the harvest revenue
#' (you can't create a tax loss from depletion alone).
#'
#' @references
#' Haney, H.L. Jr., Hoover, W.L., Siegel, W.C., & Greene, J.L. (2001).
#' *Forest Landowners' Guide to the Federal Income Tax*. USDA Forest Service
#' Agriculture Handbook 718.
#'
#' @examples
#' # 500-acre tract, 10,000 MBF total, purchased for $600,000 timber basis
#' # Harvest 3 times over 15 years
#' harvests <- data.frame(
#'   year = c(2020, 2025, 2035),
#'   volume = c(2500, 3000, 4500),
#'   revenue = c(600000, 780000, 1200000)
#' )
#' depletion_schedule(initial_basis = 600000, initial_volume = 10000,
#'                     harvests = harvests)
#'
#' @seealso \code{\link{timber_tax}}
#' @export
depletion_schedule <- function(initial_basis, initial_volume, harvests,
                                method = c("cost", "percentage")) {
  method <- match.arg(method)

  if (initial_basis < 0) stop("'initial_basis' cannot be negative", call. = FALSE)
  if (initial_volume <= 0) stop("'initial_volume' must be positive", call. = FALSE)

  required_cols <- c("year", "volume", "revenue")
  if (!all(required_cols %in% names(harvests))) {
    stop("'harvests' must have columns: ", paste(required_cols, collapse = ", "),
         call. = FALSE)
  }

  # Sort by year
  harvests <- harvests[order(harvests$year), ]

  n <- nrow(harvests)
  sched <- data.frame(
    year = harvests$year,
    volume_harvested = harvests$volume,
    revenue = harvests$revenue,
    depletion_rate = numeric(n),
    depletion_amount = numeric(n),
    taxable_gain = numeric(n),
    remaining_basis = numeric(n),
    remaining_volume = numeric(n),
    stringsAsFactors = FALSE
  )

  remaining_basis <- initial_basis
  remaining_volume <- initial_volume

  for (i in seq_len(n)) {
    vol <- harvests$volume[i]
    rev <- harvests$revenue[i]

    if (remaining_volume <= 0) {
      sched$depletion_rate[i] <- 0
      sched$depletion_amount[i] <- 0
      sched$taxable_gain[i] <- rev
    } else {
      # Cost depletion rate
      rate <- remaining_basis / remaining_volume
      depletion <- rate * vol
      # Cap depletion at remaining basis and revenue
      depletion <- min(depletion, remaining_basis, rev)

      sched$depletion_rate[i] <- rate
      sched$depletion_amount[i] <- depletion
      sched$taxable_gain[i] <- rev - depletion

      remaining_basis <- remaining_basis - depletion
      remaining_volume <- remaining_volume - vol
    }

    sched$remaining_basis[i] <- remaining_basis
    sched$remaining_volume[i] <- max(remaining_volume, 0)
  }

  structure(
    list(
      schedule = sched,
      total_depleted = initial_basis - remaining_basis,
      remaining_basis = remaining_basis,
      initial_basis = initial_basis
    ),
    class = "depletion_schedule"
  )
}


#' After-Tax NPV
#'
#' Computes net present value with timber tax treatment applied to each
#' cash flow. Revenue cash flows are taxed according to the specified
#' Section 631 method; cost cash flows are treated as deductible expenses.
#'
#' @param cash_flows Numeric vector. Cash flow amounts (negative = cost,
#'   positive = revenue).
#' @param times Numeric vector. Timing of each cash flow (years).
#' @param discount_rate Numeric. Real discount rate.
#' @param cost_basis Numeric. Timber cost basis to deplete against revenues.
#'   Default 0.
#' @param tax_method Character. Tax treatment for revenues:
#'   \code{"631a"}, \code{"631b"}, or \code{"ordinary"}. Default "631b".
#' @param marginal_rate Numeric. Ordinary income tax rate. Default 0.24.
#' @param cap_gains_rate Numeric. Capital gains rate. Default 0.15.
#' @param state_rate Numeric. State tax rate. Default 0.
#' @param holding_period Numeric. Years timber held (for capital gains
#'   qualification). Default 10.
#'
#' @return A list with:
#' \describe{
#'   \item{npv_pretax}{Pre-tax NPV}
#'   \item{npv_after_tax}{After-tax NPV}
#'   \item{tax_impact}{Difference (pretax - after_tax)}
#'   \item{effective_rate}{Total tax / total gross revenue}
#'   \item{year_detail}{Data frame: year, cash_flow, tax, after_tax_flow,
#'     discounted_pretax, discounted_after_tax}
#' }
#'
#' @details
#' For each revenue cash flow, the function applies \code{\link{timber_tax}}
#' to compute the tax liability. Cost cash flows (negative) are assumed
#' deductible at the marginal rate, reducing ordinary income tax.
#'
#' Cost basis depletion is allocated proportionally across revenue cash
#' flows (by revenue amount).
#'
#' @references
#' Klemperer, W.D. (1996). *Forest Resource Economics and Finance*.
#' McGraw-Hill. Ch. 5 (Taxes and Forest Valuation).
#'
#' @examples
#' # Plantation: plant now, harvest in 30 years
#' cf <- c(-750, -50, -50, rep(-50, 27), 8500)
#' times <- 0:30
#' after_tax_npv(cf, times, discount_rate = 0.06, cost_basis = 750,
#'               tax_method = "631b", holding_period = 30)
#'
#' @seealso \code{\link{timber_tax}}, \code{\link{npv}}
#' @export
after_tax_npv <- function(cash_flows, times, discount_rate,
                           cost_basis = 0, tax_method = c("631b", "631a", "ordinary"),
                           marginal_rate = 0.24, cap_gains_rate = 0.15,
                           state_rate = 0, holding_period = 10) {
  tax_method <- match.arg(tax_method)

  if (length(cash_flows) != length(times)) {
    stop("'cash_flows' and 'times' must have the same length", call. = FALSE)
  }

  n <- length(cash_flows)
  total_revenue <- sum(pmax(cash_flows, 0))
  remaining_basis <- cost_basis

  # Allocate basis proportionally to revenues
  year_detail <- data.frame(
    year = times,
    cash_flow = cash_flows,
    tax = numeric(n),
    after_tax_flow = numeric(n),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(n)) {
    cf <- cash_flows[i]

    if (cf > 0) {
      # Revenue: apply timber tax
      # Allocate basis proportionally
      if (total_revenue > 0) {
        basis_alloc <- cost_basis * (cf / total_revenue)
      } else {
        basis_alloc <- 0
      }
      basis_alloc <- min(basis_alloc, remaining_basis)
      remaining_basis <- remaining_basis - basis_alloc

      tt <- timber_tax(cf, basis_alloc, holding_period_years = holding_period,
                        marginal_tax_rate = marginal_rate,
                        capital_gains_rate = cap_gains_rate,
                        method = tax_method, state_tax_rate = state_rate)
      year_detail$tax[i] <- tt$tax_liability
      year_detail$after_tax_flow[i] <- tt$net_after_tax
    } else if (cf < 0) {
      # Cost: deductible at marginal rate (tax savings)
      tax_savings <- abs(cf) * (marginal_rate + state_rate)
      year_detail$tax[i] <- -tax_savings  # negative = tax savings
      year_detail$after_tax_flow[i] <- cf + tax_savings  # less costly after deduction
    } else {
      year_detail$tax[i] <- 0
      year_detail$after_tax_flow[i] <- 0
    }
  }

  # Compute NPVs
  year_detail$discounted_pretax <- cash_flows / (1 + discount_rate)^times
  year_detail$discounted_after_tax <- year_detail$after_tax_flow /
    (1 + discount_rate)^times

  npv_pre <- sum(year_detail$discounted_pretax)
  npv_post <- sum(year_detail$discounted_after_tax)

  total_tax <- sum(pmax(year_detail$tax, 0))
  eff_rate <- if (total_revenue > 0) total_tax / total_revenue else 0

  list(
    npv_pretax = npv_pre,
    npv_after_tax = npv_post,
    tax_impact = npv_pre - npv_post,
    effective_rate = eff_rate,
    year_detail = year_detail
  )
}


#' Compare Timber Tax Methods Side-by-Side
#'
#' Computes tax liability under all three methods — Section 631(a),
#' Section 631(b), and ordinary income — for the same harvest. Identifies
#' the best method and reports savings vs. ordinary income treatment.
#'
#' @param gross_revenue Numeric. Gross timber sale proceeds.
#' @param cost_basis Numeric. Adjusted cost basis.
#' @param holding_period_years Numeric. Years timber held.
#' @param marginal_rate Numeric. Ordinary income tax rate. Default 0.24.
#' @param cap_gains_rate Numeric. Capital gains rate. Default 0.15.
#' @param state_rate Numeric. State tax rate. Default 0.
#' @param depletion_basis Numeric or NULL. Default NULL.
#' @param niit Logical. Apply NIIT. Default FALSE.
#'
#' @return A list of class \code{"tax_comparison"} with:
#' \describe{
#'   \item{methods}{Data frame comparing all three methods: method, gross,
#'     depletion, taxable_gain, capital_gain, ordinary_income, tax_liability,
#'     net_after_tax, effective_rate}
#'   \item{best_method}{Name of the lowest-tax method}
#'   \item{savings_vs_ordinary}{Dollars saved by using best method vs.
#'     ordinary income}
#'   \item{recommendation}{Plain-English recommendation}
#' }
#'
#' @details
#' This is the function a consulting forester uses to show a landowner
#' why Section 631 matters. For a typical timber sale of $50,000 with an
#' $8,000 basis, the difference between 631(b) and ordinary income can be
#' $3,000-5,000 in tax savings.
#'
#' @examples
#' # $45,000 timber sale, $8,000 basis, held 25 years
#' tc <- tax_comparison(45000, 8000, 25)
#' tc$best_method
#' tc$savings_vs_ordinary
#' tc$methods
#'
#' @seealso \code{\link{timber_tax}}
#' @export
tax_comparison <- function(gross_revenue, cost_basis, holding_period_years,
                            marginal_rate = 0.24, cap_gains_rate = 0.15,
                            state_rate = 0, depletion_basis = NULL,
                            niit = FALSE) {
  methods <- c("631a", "631b", "ordinary")
  results <- vector("list", 3)

  for (i in seq_along(methods)) {
    results[[i]] <- timber_tax(
      gross_revenue = gross_revenue,
      cost_basis = cost_basis,
      depletion_basis = depletion_basis,
      holding_period_years = holding_period_years,
      marginal_tax_rate = marginal_rate,
      capital_gains_rate = cap_gains_rate,
      method = methods[i],
      state_tax_rate = state_rate,
      niit = niit
    )
  }

  comparison <- data.frame(
    method = methods,
    gross_revenue = gross_revenue,
    depletion = sapply(results, function(x) x$depletion_amount),
    taxable_gain = sapply(results, function(x) x$capital_gain + x$ordinary_income),
    capital_gain = sapply(results, function(x) x$capital_gain),
    ordinary_income = sapply(results, function(x) x$ordinary_income),
    tax_liability = sapply(results, function(x) x$tax_liability),
    net_after_tax = sapply(results, function(x) x$net_after_tax),
    effective_rate = sapply(results, function(x) x$effective_rate),
    stringsAsFactors = FALSE
  )

  best_idx <- which.min(comparison$tax_liability)
  best_method <- comparison$method[best_idx]
  ordinary_tax <- comparison$tax_liability[comparison$method == "ordinary"]
  savings <- ordinary_tax - comparison$tax_liability[best_idx]

  if (best_method == "ordinary") {
    recommendation <- "No Section 631 benefit in this case."
  } else {
    recommendation <- paste0(
      "Use Section ", sub("631", "631(", best_method), ") to save $",
      formatC(savings, format = "f", digits = 2, big.mark = ","),
      " compared to ordinary income treatment (effective rate: ",
      round(comparison$effective_rate[best_idx] * 100, 1), "% vs. ",
      round(comparison$effective_rate[comparison$method == "ordinary"] * 100, 1),
      "%)."
    )
  }

  structure(
    list(
      methods = comparison,
      best_method = best_method,
      savings_vs_ordinary = savings,
      recommendation = recommendation
    ),
    class = "tax_comparison"
  )
}


#' Reforestation Tax Deduction (IRC Section 194)
#'
#' Models the Section 194 reforestation tax incentive, which allows up to
#' $10,000 per year in direct deduction of reforestation expenses, with
#' the remainder amortized over 84 months (7 years).
#'
#' @param total_cost Numeric. Total reforestation expenditure ($).
#' @param tax_year Numeric. Year the expense was incurred. Used for
#'   the amortization schedule timeline.
#' @param marginal_rate Numeric. Marginal ordinary income tax rate.
#'   Default 0.24.
#' @param amortize_excess Logical. If TRUE (default), amounts over $10,000
#'   are amortized over 84 months per Section 194. If FALSE, only the
#'   direct deduction is computed.
#'
#' @return A list of class \code{"reforestation_deduction"} with:
#' \describe{
#'   \item{year_1_deduction}{Immediate deduction (up to $10,000)}
#'   \item{year_1_savings}{Tax savings from immediate deduction}
#'   \item{amortization_schedule}{Data frame: year, deduction, tax_savings,
#'     cumulative_savings}
#'   \item{total_tax_savings}{Total tax savings over the full period}
#'   \item{effective_subsidy_rate}{Total savings / total cost}
#'   \item{total_cost}{Original reforestation cost}
#' }
#'
#' @details
#' **How Section 194 works:**
#' \enumerate{
#'   \item The first $10,000 of qualifying reforestation expenses is
#'     deductible in the year incurred.
#'   \item Any amount over $10,000 is amortized ratably over an 84-month
#'     period beginning on the first day of the month the expense was paid.
#'     For simplicity, this function assumes the expense is paid January 1.
#'   \item The amortization uses a 1/14 deduction for the first and last
#'     half-years, and 1/7 for each full year in between.
#' }
#'
#' **Qualifying expenses** include site preparation, seed, seedlings,
#' planting labor, and direct supervision. They do not include land clearing
#' for non-timber purposes or expenses deducted under other provisions.
#'
#' @references
#' IRS Publication 535, Chapter 7 (Reforestation Costs).
#'
#' Haney, H.L. Jr., Hoover, W.L., Siegel, W.C., & Greene, J.L. (2001).
#' *Forest Landowners' Guide to the Federal Income Tax*. USDA Forest
#' Service Agriculture Handbook 718. Ch. 5.
#'
#' @examples
#' # $45,000 reforestation cost in 2025
#' rd <- reforestation_deduction(total_cost = 45000, tax_year = 2025)
#' rd$year_1_deduction         # $10,000
#' rd$total_tax_savings        # all savings over 8 tax years
#' rd$effective_subsidy_rate   # effective subsidy as % of cost
#' rd$amortization_schedule
#'
#' @seealso \code{\link{timber_tax}}, \code{\link{after_tax_npv}}
#' @export
reforestation_deduction <- function(total_cost, tax_year = 2025,
                                     marginal_rate = 0.24,
                                     amortize_excess = TRUE) {
  if (total_cost < 0) stop("'total_cost' cannot be negative", call. = FALSE)

  # Direct deduction: up to $10,000
  direct_deduction <- min(total_cost, 10000)
  direct_savings <- direct_deduction * marginal_rate

  excess <- total_cost - direct_deduction

  if (!amortize_excess || excess <= 0) {
    schedule <- data.frame(
      year = tax_year,
      deduction = direct_deduction,
      tax_savings = direct_savings,
      cumulative_savings = direct_savings,
      stringsAsFactors = FALSE
    )

    return(structure(
      list(
        year_1_deduction = direct_deduction,
        year_1_savings = direct_savings,
        amortization_schedule = schedule,
        total_tax_savings = direct_savings,
        effective_subsidy_rate = if (total_cost > 0) direct_savings / total_cost else 0,
        total_cost = total_cost
      ),
      class = "reforestation_deduction"
    ))
  }

  # 84-month amortization schedule
  # Half-year convention: 1/14 for first and last half-year,
  # 1/7 for 6 full years in between
  annual_amount <- excess / 7  # full year
  half_year_amount <- excess / 14  # half year

  # Build schedule: year 1 gets direct + half-year amort
  # Years 2-7 get full-year amort, year 8 gets final half-year
  years <- seq(tax_year, tax_year + 7)
  deductions <- numeric(8)
  deductions[1] <- direct_deduction + half_year_amount
  deductions[2:7] <- annual_amount
  deductions[8] <- half_year_amount

  savings <- deductions * marginal_rate
  cum_savings <- cumsum(savings)

  schedule <- data.frame(
    year = years,
    deduction = round(deductions, 2),
    tax_savings = round(savings, 2),
    cumulative_savings = round(cum_savings, 2),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      year_1_deduction = direct_deduction,
      year_1_savings = direct_savings,
      amortization_schedule = schedule,
      total_tax_savings = sum(savings),
      effective_subsidy_rate = if (total_cost > 0) sum(savings) / total_cost else 0,
      total_cost = total_cost
    ),
    class = "reforestation_deduction"
  )
}
