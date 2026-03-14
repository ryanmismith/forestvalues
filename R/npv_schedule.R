#' Net Present Value from a Management Schedule
#'
#' Calculates NPV for complex management scenarios with mixed payment types
#' (one-time, annual, periodic). Takes a data.frame describing all cash flows
#' in a management plan and computes the net present value.
#'
#' This function expands annual and periodic payments into their equivalent
#' present values using standard annuity formulas, then sums all discounted
#' cash flows.
#'
#' @param schedule A data.frame with columns:
#'   \describe{
#'     \item{amount}{Numeric. Cash flow amount (positive for revenue, negative for cost).}
#'     \item{year}{Numeric. Year the flow first occurs (0 = today).}
#'     \item{frequency}{Character. One of "once", "annual", or "periodic".}
#'     \item{period_length}{Numeric. Years between periodic payments. Required when
#'       frequency = "periodic"; ignored otherwise. Set to NA for non-periodic flows.}
#'   }
#' @param discount_rate Numeric scalar. The discount rate.
#' @param time_horizon Numeric scalar. Total analysis period in years.
#'
#' @return Numeric scalar. The net present value of the schedule.
#'
#' @details
#' For one-time ("once") cash flows, the standard discounting formula is used.
#' For annual cash flows, the present value of an annuity starting at the
#' specified year is computed. For periodic cash flows, the periodic series
#' present value formula is applied.
#'
#' **Annuity formulas (Klemperer 1996)**:
#' - Annual PV = A * ((1+r)^n - 1) / (r * (1+r)^n)
#' - Periodic PV = A * ((1+r)^n - 1) / (((1+r)^p - 1) * (1+r)^n)
#'
#' where A = payment, r = discount rate, n = number of years, p = period length.
#'
#' @references
#' Klemperer, W.D. (1996). *Forest Resource Economics and Finance*. McGraw-Hill. Ch. 5-6.
#'
#' Bettinger, P., Boston, K., Siry, J.P., & Grebner, D.L. (2017).
#' *Forest Management and Planning*. 2nd ed. Academic Press. Ch. 8.
#'
#' @examples
#' # Carbon credit scenario
#' schedule <- data.frame(
#'   amount = c(-750, 4000, -50, -500, -750, 2000),
#'   year = c(0, 0, 0, 10, 15, 40),
#'   frequency = c("once", "once", "annual", "periodic", "once", "once"),
#'   period_length = c(NA, NA, NA, 10, NA, NA)
#' )
#' npv_schedule(schedule, discount_rate = 0.06, time_horizon = 40)
#'
#' @family npv
#' @export
npv_schedule <- function(schedule, discount_rate, time_horizon) {
  # Input validation
  if (!is.data.frame(schedule)) stop("'schedule' must be a data.frame", call. = FALSE)
  required_cols <- c("amount", "year", "frequency")
  missing_cols <- setdiff(required_cols, names(schedule))
  if (length(missing_cols) > 0) {
    stop("'schedule' is missing columns: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }
  if (!is.numeric(discount_rate) || length(discount_rate) != 1) {
    stop("'discount_rate' must be a single numeric value", call. = FALSE)
  }
  if (!is.numeric(time_horizon) || length(time_horizon) != 1) {
    stop("'time_horizon' must be a single numeric value", call. = FALSE)
  }

  valid_freq <- c("once", "annual", "periodic")
  bad_freq <- !schedule$frequency %in% valid_freq
  if (any(bad_freq)) {
    stop("Invalid frequency values: ", paste(unique(schedule$frequency[bad_freq]),
         collapse = ", "), ". Must be 'once', 'annual', or 'periodic'.", call. = FALSE)
  }

  # Add period_length column if missing
  if (!"period_length" %in% names(schedule)) {
    schedule$period_length <- NA
  }

  # Check periodic flows have period_length
  periodic_rows <- schedule$frequency == "periodic"
  if (any(periodic_rows) && any(is.na(schedule$period_length[periodic_rows]))) {
    stop("'period_length' must be specified for all periodic cash flows", call. = FALSE)
  }

  rate <- discount_rate
  total_pv <- 0

  for (i in seq_len(nrow(schedule))) {
    amount <- schedule$amount[i]
    start_year <- schedule$year[i]
    frequency <- schedule$frequency[i]
    years_remaining <- time_horizon - start_year

    if (years_remaining < 0) next  # skip flows beyond time horizon

    if (frequency == "once") {
      # Simple discounting
      total_pv <- total_pv + amount / (1 + rate)^start_year

    } else if (frequency == "annual") {
      # Present value of annual annuity starting at 'start_year'
      # PV at start of annuity = A * ((1+r)^n - 1) / (r * (1+r)^n)
      # Then discount back to year 0
      if (rate == 0) {
        pv_at_start <- amount * (years_remaining + 1)
      } else {
        n_payments <- years_remaining + 1  # includes the start year
        pv_at_start <- amount * ((1 + rate)^n_payments - 1) / (rate * (1 + rate)^n_payments)
      }
      total_pv <- total_pv + pv_at_start / (1 + rate)^start_year

    } else if (frequency == "periodic") {
      period_length <- schedule$period_length[i]
      # Number of years within remaining time
      n_years <- years_remaining
      if (rate == 0) {
        pv_at_start <- amount * (floor(n_years / period_length) + 1)
      } else {
        # PV of periodic series: A * ((1+r)^n - 1) / (((1+r)^p - 1) * (1+r)^n)
        pv_at_start <- amount * ((1 + rate)^n_years - 1) /
          (((1 + rate)^period_length - 1) * (1 + rate)^n_years)
      }
      total_pv <- total_pv + pv_at_start / (1 + rate)^start_year
    }
  }

  total_pv
}
