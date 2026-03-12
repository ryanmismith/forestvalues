# Appraisal helper functions: liquidation_value, mid_rotation_value, comparable_sale
#
# These are building blocks used by timberland_value() and other appraisal functions.
# All are exported for standalone use.


#' Liquidation Value of Standing Timber
#'
#' Computes the immediate harvest value of all merchantable timber at the
#' current stand age — the "floor value" of the timber asset. This is what
#' the timber is worth if you cut everything today, regardless of whether
#' waiting would be more profitable.
#'
#' @param yield_tbl A \code{yield_table} object from \code{\link{yield_table}}.
#' @param current_age Numeric. Current stand age in years.
#' @param logging_cost Numeric. Logging and hauling cost per acre. Default 0.
#' @param sale_cost_pct Numeric. Transaction costs as a fraction of gross
#'   value (e.g., 0.05 for 5\% broker/administration fees). Default 0.
#'
#' @return A list with components:
#' \describe{
#'   \item{gross_value}{Total stumpage value at current age ($/acre)}
#'   \item{logging_cost}{Logging/hauling deduction}
#'   \item{sale_costs}{Transaction cost deduction (percentage-based)}
#'   \item{net_value}{After all deductions}
#'   \item{product_detail}{Data frame with product-level breakdown: product,
#'     volume, price, gross_value}
#' }
#'
#' @details
#' Liquidation value represents the minimum the timber is worth to an owner
#' who can harvest. It is the appropriate value when:
#' \itemize{
#'   \item The owner needs cash immediately
#'   \item The stand is past financial maturity
#'   \item Comparing against an offer to purchase standing timber
#'   \item Establishing a floor for the \code{\link{timberland_value}} hybrid method
#' }
#'
#' Logging costs are deducted as a flat $/acre amount. Transaction costs
#' (broker fees, sale administration) are deducted as a percentage of gross
#' value after logging costs.
#'
#' @references
#' Bullard, S.H. & Straka, T.J. (2011). *Basic Concepts in Forest Valuation
#' and Investment Analysis*. 3rd ed. Ch. 8.
#'
#' @examples
#' yt <- yield_table(
#'   ages = seq(10, 60, by = 10),
#'   products = list(
#'     sawlog = data.frame(volume = c(0, 2, 5, 10, 16, 20), price = 250),
#'     pulp = data.frame(volume = c(5, 12, 18, 22, 24, 25), price = 9)
#'   ),
#'   product_units = c(sawlog = "mbf", pulp = "ton")
#' )
#' liquidation_value(yt, current_age = 35, logging_cost = 150, sale_cost_pct = 0.05)
#'
#' @seealso \code{\link{timberland_value}}, \code{\link{mid_rotation_value}}
#' @export
liquidation_value <- function(yield_tbl, current_age, logging_cost = 0,
                               sale_cost_pct = 0) {
  if (!inherits(yield_tbl, "yield_table")) {
    stop("'yield_tbl' must be a yield_table object", call. = FALSE)
  }
  if (!is.numeric(current_age) || length(current_age) != 1 || current_age < 0) {
    stop("'current_age' must be a non-negative number", call. = FALSE)
  }
  if (sale_cost_pct < 0 || sale_cost_pct >= 1) {
    stop("'sale_cost_pct' must be between 0 and 1", call. = FALSE)
  }

  # Product-level breakdown
  product_detail <- data.frame(
    product = character(),
    volume = numeric(),
    price = numeric(),
    gross_value = numeric(),
    stringsAsFactors = FALSE
  )

  for (pn in yield_tbl$product_names) {
    vol <- yield_tbl$product_fns[[pn]](current_age)
    vol <- max(vol, 0)  # no negative volumes

    pdata <- yield_tbl$products[[pn]]
    price <- if ("price" %in% names(pdata)) pdata$price[1] else 0

    product_detail <- rbind(product_detail, data.frame(
      product = pn,
      volume = vol,
      price = price,
      gross_value = vol * price,
      stringsAsFactors = FALSE
    ))
  }

  gross_value <- sum(product_detail$gross_value)
  after_logging <- gross_value - logging_cost
  sale_costs <- after_logging * sale_cost_pct
  net_value <- after_logging - sale_costs

  list(
    gross_value = gross_value,
    logging_cost = logging_cost,
    sale_costs = sale_costs,
    net_value = max(net_value, 0),  # can't be negative (just don't harvest)
    product_detail = product_detail
  )
}


#' Mid-Rotation Stand Value
#'
#' Values a stand that is partway through a rotation — the most common
#' real-world situation. Decomposes the value into the present value of
#' the anticipated timber harvest plus the present value of the bare land
#' after harvest.
#'
#' @param yield_tbl A \code{yield_table} object.
#' @param current_age Numeric. Current stand age in years.
#' @param discount_rate Numeric. Real discount rate (e.g., 0.06 for 6\%).
#' @param regen_cost Numeric. Regeneration cost at start of next rotation.
#'   Default 0.
#' @param annual_cost Numeric. Annual carrying cost (taxes, insurance).
#'   Default 0.
#' @param harvest_age Numeric or NULL. Planned harvest age. If NULL, uses
#'   the optimal rotation from \code{\link{optimal_rotation_mp}}.
#'
#' @return A list with components:
#' \describe{
#'   \item{timber_value}{PV of future harvest discounted to today}
#'   \item{land_value}{PV of bare land (LEV) discounted from harvest to today}
#'   \item{total_value}{timber_value + land_value}
#'   \item{years_to_harvest}{harvest_age - current_age}
#'   \item{harvest_revenue}{Undiscounted revenue at harvest age}
#'   \item{harvest_age}{The harvest age used}
#'   \item{annual_cost_pv}{PV of annual costs over the remaining wait period}
#' }
#'
#' @details
#' The mid-rotation value is computed as:
#'
#' \deqn{V_{mid} = \frac{R(T)}{(1+r)^{T-t}} + \frac{LEV}{(1+r)^{T-t}} -
#'   PV(\text{annual costs from } t \text{ to } T)}
#'
#' where \eqn{t} is current age, \eqn{T} is harvest age, \eqn{R(T)} is
#' harvest revenue, and \eqn{LEV} is the bare land value assuming perpetual
#' management after harvest.
#'
#' This is the **anticipated value** (or expectation value) approach, used
#' when valuing standing timber that will be harvested in the future. It
#' answers: "What would a buyer pay today for this stand, given that they
#' plan to harvest at age T and then manage the land perpetually?"
#'
#' @references
#' Klemperer, W.D. (1996). *Forest Resource Economics and Finance*.
#' McGraw-Hill. Ch. 9 (Valuing Mid-Rotation Stands).
#'
#' @examples
#' yt <- yield_table(
#'   ages = seq(10, 60, by = 10),
#'   products = list(
#'     sawlog = data.frame(volume = c(0, 2, 5, 10, 16, 20), price = 250),
#'     pulp = data.frame(volume = c(5, 12, 18, 22, 24, 25), price = 9)
#'   )
#' )
#' mid_rotation_value(yt, current_age = 25, discount_rate = 0.06,
#'                     regen_cost = 750, annual_cost = 50)
#'
#' @seealso \code{\link{timberland_value}}, \code{\link{liquidation_value}},
#'   \code{\link{lev}}
#' @export
mid_rotation_value <- function(yield_tbl, current_age, discount_rate,
                                regen_cost = 0, annual_cost = 0,
                                harvest_age = NULL) {
  if (!inherits(yield_tbl, "yield_table")) {
    stop("'yield_tbl' must be a yield_table object", call. = FALSE)
  }
  if (discount_rate <= 0) {
    stop("'discount_rate' must be positive", call. = FALSE)
  }

  # Determine harvest age
  if (is.null(harvest_age)) {
    opt <- optimal_rotation_mp(yield_tbl, regen_cost, annual_cost,
                                discount_rate)
    harvest_age <- opt$optimal_age
  }

  if (current_age >= harvest_age) {
    warning("current_age >= harvest_age; returning liquidation value.",
            call. = FALSE)
    liq <- liquidation_value(yield_tbl, current_age)
    return(list(
      timber_value = liq$net_value,
      land_value = 0,
      total_value = liq$net_value,
      years_to_harvest = 0,
      harvest_revenue = liq$gross_value,
      harvest_age = current_age,
      annual_cost_pv = 0
    ))
  }

  years_to_harvest <- harvest_age - current_age
  r <- discount_rate

  # Revenue at harvest age
  harvest_revenue <- yield_tbl$total_value_fn(harvest_age)

  # PV of harvest revenue discounted to today
  timber_value <- harvest_revenue / (1 + r)^years_to_harvest

  # LEV at harvest age (bare land value for perpetual management)
  # We need the single-rotation NPV at optimal age to compute LEV
  pv_rev_at_zero <- harvest_revenue / (1 + r)^harvest_age
  pv_annual_at_zero <- if (annual_cost > 0 && r > 0) {
    annual_cost * ((1 + r)^harvest_age - 1) / (r * (1 + r)^harvest_age)
  } else {
    annual_cost * harvest_age
  }
  rotation_npv <- pv_rev_at_zero - regen_cost - pv_annual_at_zero
  lev_at_harvest <- lev(rotation_npv, harvest_age, r, is_npv = TRUE)

  # PV of LEV discounted from harvest to today
  land_value <- lev_at_harvest / (1 + r)^years_to_harvest

  # PV of annual costs from now to harvest
  annual_cost_pv <- if (annual_cost > 0 && r > 0) {
    annual_cost * ((1 + r)^years_to_harvest - 1) / (r * (1 + r)^years_to_harvest)
  } else {
    annual_cost * years_to_harvest
  }

  total_value <- timber_value + land_value - annual_cost_pv

  list(
    timber_value = timber_value,
    land_value = land_value,
    total_value = total_value,
    years_to_harvest = years_to_harvest,
    harvest_revenue = harvest_revenue,
    harvest_age = harvest_age,
    annual_cost_pv = annual_cost_pv
  )
}


#' Comparable Sale Adjustment
#'
#' Adjusts a comparable timberland sale price for differences between the
#' comparable property and the subject property. This implements the **sales
#' comparison approach** to appraisal, where recent arm's-length transactions
#' are adjusted for differences in characteristics.
#'
#' @param sale_price_per_acre Numeric. The comparable's sale price per acre.
#' @param adjustments Named list of adjustments. Each element should be a
#'   numeric value representing the dollar-per-acre adjustment (positive if
#'   the subject is superior, negative if inferior). Common adjustment
#'   categories:
#'   \describe{
#'     \item{timber_volume}{Difference in standing timber value}
#'     \item{access}{Road quality, distance to mill}
#'     \item{site_quality}{Site index or productivity difference}
#'     \item{location}{Regional market premium/discount}
#'     \item{time}{Market appreciation since sale date}
#'     \item{size}{Large tract discount or small tract premium}
#'     \item{topography}{Terrain operability adjustment}
#'     \item{water}{Water frontage or wetland impact}
#'   }
#' @param sale_acres Numeric. Acres in the comparable sale. Default 1.
#'   Used only for total value computation.
#' @param method Character. \code{"additive"} (default) adds adjustments
#'   to the per-acre price. \code{"multiplicative"} applies adjustments as
#'   percentage multipliers (e.g., 1.05 = 5\% premium, 0.90 = 10\% discount).
#'
#' @return A list with components:
#' \describe{
#'   \item{adjusted_price}{Adjusted $/acre for the subject property}
#'   \item{raw_price}{Original sale price per acre}
#'   \item{total_adjustment}{Sum (additive) or product (multiplicative) of
#'     all adjustments}
#'   \item{adjustment_pct}{Total adjustment as \% of raw price}
#'   \item{adjustment_detail}{Data frame: category, adjustment, cumulative}
#' }
#'
#' @details
#' The sales comparison approach is one of three standard appraisal methods
#' (alongside income and cost approaches). For timberland, adjustments
#' typically cover:
#'
#' \enumerate{
#'   \item **Timber volume/value** — most important; difference in standing
#'     inventory between comparable and subject
#'   \item **Time** — market appreciation/depreciation since the comp sold
#'   \item **Location** — proximity to mills, roads, markets
#'   \item **Site quality** — site index, soil productivity
#'   \item **Access** — road network, terrain operability
#' }
#'
#' Appraisers generally apply adjustments additively ($/acre) for most
#' categories, but time adjustments are sometimes multiplicative.
#'
#' **USPAP Note:** Per the Uniform Standards of Professional Appraisal
#' Practice, total net adjustments exceeding 25\% of the comparable's
#' price should be viewed with caution, and the function warns accordingly.
#'
#' @references
#' Bullard, S.H. & Straka, T.J. (2011). *Basic Concepts in Forest Valuation
#' and Investment Analysis*. 3rd ed. Ch. 8 (Timberland Appraisal).
#'
#' Wagner, J.E. (2012). *Forestry Economics: A Managerial Approach*. Ch. 11.
#'
#' @examples
#' # Comparable sold at $2,100/acre. Subject has more timber, worse access.
#' comparable_sale(
#'   sale_price_per_acre = 2100,
#'   adjustments = list(
#'     timber_volume = 350,    # subject has more standing timber
#'     access = -125,          # subject has worse road access
#'     site_quality = 50,      # slightly better site index
#'     time = 100              # market appreciated since sale
#'   )
#' )
#'
#' @seealso \code{\link{timberland_value}}
#' @export
comparable_sale <- function(sale_price_per_acre, adjustments = list(),
                             sale_acres = 1,
                             method = c("additive", "multiplicative")) {
  method <- match.arg(method)

  if (!is.numeric(sale_price_per_acre) || length(sale_price_per_acre) != 1) {
    stop("'sale_price_per_acre' must be a single number", call. = FALSE)
  }
  if (!is.list(adjustments)) {
    stop("'adjustments' must be a named list", call. = FALSE)
  }

  if (length(adjustments) == 0) {
    return(list(
      adjusted_price = sale_price_per_acre,
      raw_price = sale_price_per_acre,
      total_adjustment = 0,
      adjustment_pct = 0,
      adjustment_detail = data.frame(
        category = character(), adjustment = numeric(),
        cumulative = numeric(), stringsAsFactors = FALSE
      )
    ))
  }

  # Build detail table
  categories <- names(adjustments)
  values <- as.numeric(adjustments)

  if (method == "additive") {
    cumulative <- cumsum(values)
    total_adj <- sum(values)
    adjusted_price <- sale_price_per_acre + total_adj
  } else {
    # Multiplicative: values are multipliers (e.g., 1.05 for +5%)
    cumulative <- cumprod(values) * sale_price_per_acre
    total_adj <- prod(values)
    adjusted_price <- sale_price_per_acre * total_adj
  }

  adjustment_pct <- (adjusted_price - sale_price_per_acre) / sale_price_per_acre

  # USPAP warning for large adjustments
  if (abs(adjustment_pct) > 0.25) {
    warning("Total net adjustment exceeds 25% of comparable price. ",
            "Per USPAP, this comparable may not be sufficiently similar.",
            call. = FALSE)
  }

  detail <- data.frame(
    category = categories,
    adjustment = values,
    cumulative = if (method == "additive") {
      sale_price_per_acre + cumulative
    } else {
      cumulative
    },
    stringsAsFactors = FALSE
  )

  list(
    adjusted_price = adjusted_price,
    raw_price = sale_price_per_acre,
    total_adjustment = if (method == "additive") total_adj else adjusted_price - sale_price_per_acre,
    adjustment_pct = adjustment_pct,
    adjustment_detail = detail
  )
}
