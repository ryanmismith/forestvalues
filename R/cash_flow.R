#' Build a Cash Flow Schedule
#'
#' Expands a set of management activities into a year-by-year cash flow table
#' with cumulative NPV tracking. This is the primary tool for building and
#' visualizing a complete forest management financial plan.
#'
#' @param activities A data.frame with columns:
#'   \describe{
#'     \item{name}{Character. Activity description (e.g., "Planting", "Harvest").}
#'     \item{amount}{Numeric. Cash flow amount (positive = revenue, negative = cost).}
#'     \item{year}{Numeric. Year the activity first occurs.}
#'     \item{frequency}{Character. "once", "annual", or "periodic".}
#'     \item{period_length}{Numeric. Years between periodic occurrences (NA otherwise).}
#'   }
#' @param time_horizon Numeric scalar. Total planning horizon in years.
#' @param discount_rate Numeric scalar. Discount rate for NPV calculations.
#'
#' @return A data.frame with one row per year containing:
#'   \describe{
#'     \item{year}{Year number (0 to time_horizon).}
#'     \item{cash_flow}{Net cash flow in that year.}
#'     \item{cumulative_cash}{Cumulative undiscounted cash flow.}
#'     \item{discounted_flow}{Discounted cash flow for that year.}
#'     \item{cumulative_npv}{Cumulative NPV through that year.}
#'   }
#'
#' @references
#' Klemperer, W.D. (1996). *Forest Resource Economics and Finance*. McGraw-Hill. Ch. 9.
#'
#' Bettinger, P., Boston, K., Siry, J.P., & Grebner, D.L. (2017).
#' *Forest Management and Planning*. 2nd ed. Academic Press. Ch. 8.
#'
#' @examples
#' activities <- data.frame(
#'   name = c("Planting", "Property Tax", "PCT", "Thinning", "Final Harvest"),
#'   amount = c(-750, -50, -800, 2500, 6600),
#'   year = c(0, 0, 12, 20, 40),
#'   frequency = c("once", "annual", "once", "once", "once"),
#'   period_length = c(NA, NA, NA, NA, NA)
#' )
#' schedule <- cash_flow_schedule(activities, time_horizon = 40, discount_rate = 0.06)
#' head(schedule)
#' tail(schedule)
#'
#' @seealso \code{\link{npv_schedule}}, \code{\link{project_income}}
#'
#' @family cash-flow
#' @export
cash_flow_schedule <- function(activities, time_horizon, discount_rate) {
  # Input validation
  if (!is.data.frame(activities)) stop("'activities' must be a data.frame", call. = FALSE)
  required_cols <- c("name", "amount", "year", "frequency")
  missing_cols <- setdiff(required_cols, names(activities))
  if (length(missing_cols) > 0) {
    stop("'activities' is missing columns: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  if (!"period_length" %in% names(activities)) {
    activities$period_length <- NA
  }

  years <- 0:time_horizon
  annual_flows <- rep(0, length(years))

  for (i in seq_len(nrow(activities))) {
    amount <- activities$amount[i]
    start_year <- activities$year[i]
    frequency <- activities$frequency[i]

    if (frequency == "once") {
      if (start_year >= 0 && start_year <= time_horizon) {
        year_index <- start_year + 1  # 0-indexed year -> 1-indexed position
        annual_flows[year_index] <- annual_flows[year_index] + amount
      }

    } else if (frequency == "annual") {
      for (year in start_year:time_horizon) {
        year_index <- year + 1
        annual_flows[year_index] <- annual_flows[year_index] + amount
      }

    } else if (frequency == "periodic") {
      period_length <- activities$period_length[i]
      if (is.na(period_length)) stop("'period_length' required for periodic activities", call. = FALSE)
      year <- start_year
      while (year <= time_horizon) {
        year_index <- year + 1
        annual_flows[year_index] <- annual_flows[year_index] + amount
        year <- year + period_length
      }
    }
  }

  # Build result
  discounted <- annual_flows / (1 + discount_rate)^years

  result <- data.frame(
    year = years,
    cash_flow = annual_flows,
    cumulative_cash = cumsum(annual_flows),
    discounted_flow = discounted,
    cumulative_npv = cumsum(discounted)
  )

  class(result) <- c("cash_flow_schedule", "data.frame")
  result
}


#' Project Timber Income
#'
#' Projects timber income from a yield function, stumpage price, and
#' harvest schedule. Combines growth modeling with financial analysis.
#'
#' @param yield_fn A function or data.frame (see \code{\link{optimal_rotation}}).
#' @param stumpage_price Numeric scalar. Price per unit volume.
#' @param harvest_ages Numeric vector. Ages at which harvests occur.
#' @param harvest_fractions Numeric vector. Fraction of standing volume
#'   removed at each harvest (0 to 1). Default: 1 (clearcut) for last harvest,
#'   0.3 for earlier harvests.
#' @param regen_cost Numeric scalar. Regeneration cost at year 0. Default 0.
#' @param annual_cost Numeric scalar. Annual cost (e.g., taxes). Default 0.
#' @param discount_rate Numeric scalar. Discount rate.
#' @param time_horizon Numeric scalar. Total planning period. Default: max of
#'   harvest_ages.
#'
#' @return A list with:
#'   \describe{
#'     \item{harvests}{Data.frame of harvest details (age, volume, revenue).}
#'     \item{npv}{NPV of the projected income stream.}
#'     \item{lev}{LEV assuming perpetual rotations.}
#'     \item{irr}{IRR of the projected income stream (if calculable).}
#'   }
#'
#' @references
#' Gregory, G.R. (1987). *Resource Economics for Foresters*. Wiley. Ch. 12.
#'
#' Klemperer, W.D. (1996). *Forest Resource Economics and Finance*. McGraw-Hill. Ch. 9.
#'
#' @examples
#' yield <- function(age) 200 * (1 - exp(-0.03 * age))^3
#' result <- project_income(yield, stumpage_price = 50,
#'                           harvest_ages = c(25, 40),
#'                           regen_cost = 750, annual_cost = 5,
#'                           discount_rate = 0.06)
#' result$harvests
#' result$npv
#'
#' @family cash-flow
#' @export
project_income <- function(yield_fn, stumpage_price, harvest_ages,
                            harvest_fractions = NULL,
                            regen_cost = 0, annual_cost = 0,
                            discount_rate, time_horizon = NULL) {
  # Convert data.frame yield to function
  if (is.data.frame(yield_fn)) {
    if (!all(c("age", "volume") %in% names(yield_fn))) {
      stop("yield data.frame must have 'age' and 'volume' columns", call. = FALSE)
    }
    yield_data <- yield_fn
    yield_fn <- stats::splinefun(yield_data$age, yield_data$volume, method = "natural")
  }

  if (is.null(time_horizon)) time_horizon <- max(harvest_ages)
  harvest_ages <- sort(harvest_ages)

  # Default fractions: clearcut at final, 30% thinning for earlier

  if (is.null(harvest_fractions)) {
    harvest_fractions <- rep(0.3, length(harvest_ages))
    harvest_fractions[length(harvest_fractions)] <- 1.0
  }

  if (length(harvest_fractions) != length(harvest_ages)) {
    stop("'harvest_fractions' must match length of 'harvest_ages'", call. = FALSE)
  }

  # Calculate harvest volumes and revenues
  remaining_frac <- 1.0
  harvests <- data.frame(
    age = harvest_ages,
    volume = NA_real_,
    fraction = harvest_fractions,
    revenue = NA_real_
  )

  for (i in seq_along(harvest_ages)) {
    total_vol <- yield_fn(harvest_ages[i])
    available_vol <- total_vol * remaining_frac
    harvested_vol <- available_vol * harvest_fractions[i]
    harvests$volume[i] <- harvested_vol
    harvests$revenue[i] <- harvested_vol * stumpage_price
    remaining_frac <- remaining_frac * (1 - harvest_fractions[i])
  }

  # Build cash flows for NPV/IRR
  cf <- c(-regen_cost, harvests$revenue)
  times <- c(0, harvest_ages)

  # Add annual costs
  if (annual_cost > 0) {
    for (y in 0:time_horizon) {
      cf <- c(cf, -annual_cost)
      times <- c(times, y)
    }
  }

  result_npv <- npv(cf, times, discount_rate)
  result_lev <- lev(result_npv, time_horizon, discount_rate, is_npv = TRUE)

  # Try IRR
  result_irr <- tryCatch(
    irr(cf, times),
    error = function(e) NA_real_
  )

  list(
    harvests = harvests,
    npv = result_npv,
    lev = result_lev,
    irr = result_irr
  )
}


#' Annualize a Present Value
#'
#' Converts a lump-sum present value to an equivalent annual annuity (EAA).
#' Useful for comparing investments with different time horizons on an
#' annual basis.
#'
#' AE = PV * r / (1 - (1+r)^(-n))
#'
#' @param present_value Numeric scalar. The present value to annualize.
#' @param discount_rate Numeric scalar. The discount rate.
#' @param n_years Numeric scalar. Number of years for the annuity.
#'
#' @return Numeric scalar. The equivalent annual payment.
#'
#' @references
#' Klemperer, W.D. (1996). *Forest Resource Economics and Finance*. McGraw-Hill. Ch. 6.
#'
#' @examples
#' # What annual income equals an NPV of $5,000 over 30 years at 6%?
#' annualize(5000, 0.06, 30)
#'
#' @family cash-flow
#' @export
annualize <- function(present_value, discount_rate, n_years) {
  if (!is.numeric(present_value)) stop("'present_value' must be numeric", call. = FALSE)
  if (!is.numeric(discount_rate) || length(discount_rate) != 1) {
    stop("'discount_rate' must be a single numeric value", call. = FALSE)
  }
  if (!is.numeric(n_years) || length(n_years) != 1 || n_years <= 0) {
    stop("'n_years' must be a single positive number", call. = FALSE)
  }

  if (discount_rate == 0) return(present_value / n_years)

  present_value * discount_rate / (1 - (1 + discount_rate)^(-n_years))
}
