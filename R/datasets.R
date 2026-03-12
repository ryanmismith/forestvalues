# Example datasets for forestvalues package
#
# These functions create pre-built example datasets for use in examples,
# vignettes, and testing. They return data objects ready for use with
# package functions.


#' Southern Pine Plantation Yield Data
#'
#' Creates a \code{\link{yield_table}} object for a typical loblolly pine
#' plantation in the U.S. Southeast (Site Index 60, base age 25, planted
#' at 600 trees/acre). Two products: sawlog and pulpwood.
#'
#' @return A \code{yield_table} object with ages 10-50 (by 5), two products
#'   (sawlog at $254/MBF, pulpwood at $9/ton).
#'
#' @details
#' This is the most commonly used example dataset in this package. Volumes
#' approximate Clutter & Jones (1980) growth and yield predictions for
#' well-managed loblolly pine in the coastal plain.
#'
#' @examples
#' yt <- pine_yields()
#' print(yt)
#' plot(yt)
#'
#' # Use in rotation analysis
#' optimal_rotation_mp(yt, regen_cost = 750, annual_cost = 50,
#'                      discount_rate = 0.06)
#'
#' @references
#' Clutter, J.L. & Jones, E.P. (1980). Prediction of growth after thinning
#' in old-field slash pine plantations. USDA Forest Service Research Paper
#' SE-217.
#'
#' @family datasets
#' @family yield-tables
#' @export
pine_yields <- function() {
  yield_table(
    ages = seq(10, 50, by = 5),
    products = list(
      sawlog = data.frame(
        volume = c(0, 0, 1.5, 4.0, 8.0, 13.0, 17.5, 21.0, 23.0),
        price = 254
      ),
      pulpwood = data.frame(
        volume = c(8, 16, 22, 26, 28, 29, 29, 28, 27),
        price = 9
      )
    ),
    product_units = c(sawlog = "mbf", pulpwood = "ton")
  )
}


#' Northern Hardwood Selection Yield Data
#'
#' Creates a \code{\link{yield_table}} object for uneven-aged northern
#' hardwoods (sugar maple, yellow birch, beech) typical of the U.S.
#' Northeast. Two products: sawtimber and cordwood. Suitable for modeling
#' selection cutting systems.
#'
#' @return A \code{yield_table} object with ages 10-80 (by 10), two products
#'   (sawtimber at $380/MBF, cordwood at $12/cord).
#'
#' @details
#' These volumes approximate standing inventory accumulation for a
#' well-stocked northern hardwood stand on a Class II site. In practice,
#' selection systems harvest periodically (15-20 year cutting cycle)
#' rather than clearcutting, but the yield table format is used here
#' for rotation analysis comparison.
#'
#' @examples
#' yt <- hardwood_yields()
#'
#' # Compare rotation ages
#' rotation_comparison_mp(yt, regen_cost = 500, annual_cost = 25,
#'                         discount_rate = 0.04)
#'
#' @references
#' Leak, W.B., Solomon, D.S., & DeBald, P.S. (1987). Silvicultural Guide
#' for Northern Hardwood Types in the Northeast. USDA Forest Service
#' Research Paper NE-603.
#'
#' @family datasets
#' @family yield-tables
#' @export
hardwood_yields <- function() {
  yield_table(
    ages = seq(10, 80, by = 10),
    products = list(
      sawtimber = data.frame(
        volume = c(0, 0.5, 1.5, 3.5, 6.0, 8.5, 10.0, 11.0),
        price = 380
      ),
      cordwood = data.frame(
        volume = c(3, 6, 10, 14, 17, 19, 20, 20),
        price = 12
      )
    ),
    product_units = c(sawtimber = "mbf", cordwood = "cord")
  )
}


#' Southern Pine Management Schedule
#'
#' Returns a management schedule data frame for a typical loblolly pine
#' plantation. Includes site prep, planting, herbicide, annual costs,
#' thinning, and final harvest. Ready for use with
#' \code{\link{cash_flow_schedule}}, \code{\link{npv_schedule}},
#' \code{\link{export_excel}}, and \code{\link{after_tax_npv}}.
#'
#' @return A data frame with columns: \code{name}, \code{amount}, \code{year},
#'   \code{frequency}, \code{period_length}.
#'
#' @details
#' Cost and revenue assumptions:
#' \describe{
#'   \item{Site prep & planting}{$750/acre (year 0)}
#'   \item{Herbicide release}{$125/acre (year 1)}
#'   \item{Property tax}{$12/acre/year (annual)}
#'   \item{Insurance}{$5/acre/year (annual)}
#'   \item{Pre-commercial thin}{$200/acre (year 8, cost)}
#'   \item{First thinning}{$2,500/acre revenue (year 20)}
#'   \item{Final harvest}{$8,500/acre revenue (year 35)}
#' }
#'
#' @examples
#' sched <- pine_schedule()
#'
#' # NPV of the full management plan
#' npv_schedule(sched, discount_rate = 0.06, time_horizon = 35)
#'
#' # Visualize cash flows
#' cf <- cash_flow_schedule(sched, time_horizon = 35)
#' plot(cf)
#'
#' @family datasets
#' @family cash-flow
#' @export
pine_schedule <- function() {
  data.frame(
    name = c("Site Prep & Planting", "Herbicide Release", "Property Tax",
             "Insurance", "Pre-commercial Thin", "First Thinning",
             "Final Harvest"),
    amount = c(-750, -125, -12, -5, -200, 2500, 8500),
    year = c(0, 1, 0, 0, 8, 20, 35),
    frequency = c("once", "once", "annual", "annual", "once", "once", "once"),
    period_length = c(NA, NA, NA, NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )
}


#' Example Timber Sales for Tax Analysis
#'
#' Returns a data frame of three representative timber sales for use with
#' \code{\link{timber_tax}}, \code{\link{tax_comparison}}, and
#' \code{\link{after_tax_npv}}.
#'
#' @return A data frame with columns: \code{sale_id}, \code{year},
#'   \code{gross_revenue}, \code{cost_basis}, \code{volume_mbf},
#'   \code{holding_period_years}, \code{species}, \code{state}.
#'
#' @details
#' Three sales representing different timber types and scales:
#' \describe{
#'   \item{TS-2020-001}{$45,000 loblolly pine sale in Georgia, 25-year hold}
#'   \item{TS-2023-001}{$78,000 mixed pine sale in Alabama, 30-year hold}
#'   \item{TS-2025-001}{$125,000 longleaf pine sale in Mississippi, 18-year hold}
#' }
#'
#' @examples
#' sales <- timber_sales()
#'
#' # Tax comparison for the first sale
#' tax_comparison(
#'   gross_revenue = sales$gross_revenue[1],
#'   cost_basis = sales$cost_basis[1],
#'   holding_period_years = sales$holding_period_years[1]
#' )
#'
#' @family datasets
#' @family timber-tax
#' @export
timber_sales <- function() {
  data.frame(
    sale_id = c("TS-2020-001", "TS-2023-001", "TS-2025-001"),
    year = c(2020, 2023, 2025),
    gross_revenue = c(45000, 78000, 125000),
    cost_basis = c(8000, 15000, 22000),
    volume_mbf = c(180, 310, 500),
    holding_period_years = c(25, 30, 18),
    species = c("Loblolly Pine", "Mixed Pine", "Longleaf Pine"),
    state = c("Georgia", "Alabama", "Mississippi"),
    stringsAsFactors = FALSE
  )
}


#' Multi-Harvest Depletion Example Data
#'
#' Returns a data frame of five harvest entries for a 1,000-acre pine tract
#' harvested over 15 years. Ready for use with \code{\link{depletion_schedule}}.
#'
#' @return A list with:
#' \describe{
#'   \item{harvests}{Data frame with columns: \code{year}, \code{volume},
#'     \code{revenue}, \code{acres}}
#'   \item{initial_basis}{Initial timber cost basis ($800,000)}
#'   \item{initial_volume}{Initial merchantable volume (13,000 MBF)}
#' }
#'
#' @examples
#' data <- pine_harvests()
#'
#' ds <- depletion_schedule(
#'   initial_basis = data$initial_basis,
#'   initial_volume = data$initial_volume,
#'   harvests = data$harvests
#' )
#' ds$schedule
#' ds$remaining_basis
#'
#' @family datasets
#' @family timber-tax
#' @export
pine_harvests <- function() {
  list(
    harvests = data.frame(
      year = c(2018, 2021, 2024, 2027, 2033),
      volume = c(2000, 2500, 3000, 1500, 4000),
      revenue = c(480000, 650000, 810000, 420000, 1200000),
      acres = c(200, 250, 300, 150, 100),
      stringsAsFactors = FALSE
    ),
    initial_basis = 800000,
    initial_volume = 13000
  )
}


#' Comparable Timberland Sales Data
#'
#' Returns a data frame of four recent timberland sales for use with
#' \code{\link{comparable_sale}} and the sales comparison approach to
#' timberland appraisal.
#'
#' @return A data frame with columns: \code{sale_id}, \code{sale_date},
#'   \code{price_per_acre}, \code{acres}, \code{timber_vol_mbf},
#'   \code{site_index}, \code{road_access}, \code{county}.
#'
#' @examples
#' comps <- comparable_sales_data()
#'
#' # Adjust the first comparable for the subject property
#' comparable_sale(
#'   sale_price_per_acre = comps$price_per_acre[1],
#'   adjustments = list(
#'     timber_volume = 350,
#'     access = -125,
#'     site_quality = 50
#'   )
#' )
#'
#' @family datasets
#' @family appraisal
#' @export
comparable_sales_data <- function() {
  data.frame(
    sale_id = c("CS-001", "CS-002", "CS-003", "CS-004"),
    sale_date = as.Date(c("2024-03-15", "2024-07-22", "2023-11-01", "2025-01-10")),
    price_per_acre = c(2100, 1850, 2400, 2250),
    acres = c(450, 1200, 280, 650),
    timber_vol_mbf = c(4.2, 2.8, 5.5, 3.9),
    site_index = c(65, 55, 70, 62),
    road_access = c("good", "poor", "excellent", "good"),
    county = c("Baldwin, AL", "Ware, GA", "Berkeley, SC", "Jones, MS"),
    stringsAsFactors = FALSE
  )
}


#' Discount Rate Component Benchmarks
#'
#' Returns a reference table of typical discount rate component ranges
#' for use with \code{\link{discount_rate_buildup}}. Values represent
#' historical ranges observed in U.S. timberland transactions and
#' appraisals.
#'
#' @return A data frame with columns: \code{component}, \code{typical_low},
#'   \code{typical_mid}, \code{typical_high}, \code{description}.
#'
#' @details
#' Components:
#' \describe{
#'   \item{Risk-free rate}{10-year U.S. Treasury yield (3-5\%)}
#'   \item{Inflation premium}{Expected CPI (1.5-3.5\%)}
#'   \item{Illiquidity premium}{Non-traded asset premium (0.5-2\%)}
#'   \item{Management risk}{Professional management cost/risk (0.3-1\%)}
#'   \item{Market/timber risk}{Price volatility (0.5-2\%)}
#'   \item{Property-specific}{Title, access, environmental (0-1.5\%)}
#' }
#'
#' @examples
#' rc <- rate_components()
#' rc
#'
#' # Build a rate using the midpoint of each range
#' discount_rate_buildup(
#'   risk_free = rc$typical_mid[1],
#'   inflation_premium = rc$typical_mid[2],
#'   illiquidity_premium = rc$typical_mid[3],
#'   management_risk = rc$typical_mid[4],
#'   market_risk = rc$typical_mid[5],
#'   property_specific = rc$typical_mid[6]
#' )
#'
#' @references
#' Cascio, A.J. & Clutter, M.L. (2008). Risk and required return
#' assessments of equity timberland investments in the United States.
#' *Forest Products Journal* 58(10): 61-70.
#'
#' @family datasets
#' @family appraisal
#' @export
rate_components <- function() {
  data.frame(
    component = c("Risk-free rate", "Inflation premium", "Illiquidity premium",
                   "Management risk", "Market/timber risk", "Property-specific"),
    typical_low = c(0.03, 0.015, 0.005, 0.003, 0.005, 0.00),
    typical_mid = c(0.04, 0.025, 0.010, 0.005, 0.010, 0.005),
    typical_high = c(0.05, 0.035, 0.020, 0.010, 0.020, 0.015),
    description = c(
      "10-year U.S. Treasury yield",
      "Expected CPI inflation rate",
      "Premium for non-traded, hard-to-sell asset",
      "Cost/risk of professional forest management",
      "Timber price volatility and market risk",
      "Property-level factors (title, access, environmental)"),
    stringsAsFactors = FALSE
  )
}
