#' Net Present Value
#'
#' Calculates the net present value of a series of cash flows occurring at
#' specified times, discounted at a given rate. Cash flows should be signed:
#' positive for revenues, negative for costs.
#'
#' NPV = sum(CF_t / (1 + r)^t) for all cash flows CF at times t.
#'
#' This is the standard discounted cash flow criterion used throughout
#' forest economics. A positive NPV indicates the investment earns more
#' than the discount rate; a negative NPV indicates it does not.
#'
#' @param cash_flows Numeric vector. Signed cash flows (negative = cost,
#'   positive = revenue).
#' @param times Numeric vector. Year each cash flow occurs (0 = today).
#'   Must be same length as \code{cash_flows}.
#' @param discount_rate Numeric scalar. The discount rate (e.g., 0.06 for 6\%).
#'
#' @return Numeric scalar. The net present value.
#'
#' @references
#' Klemperer, W.D. (1996). *Forest Resource Economics and Finance*. McGraw-Hill. Ch. 5-6.
#'
#' Bettinger, P., Boston, K., Siry, J.P., & Grebner, D.L. (2017).
#' *Forest Management and Planning*. 2nd ed. Academic Press. Ch. 8.
#'
#' Bullard, S.H. & Straka, T.J. (2011). *Basic Concepts in Forest Valuation
#' and Investment Analysis*. 3rd ed. Ch. 4-5.
#'
#' @examples
#' # Simple plantation: plant at year 0, harvest at year 30
#' npv(
#'   cash_flows = c(-1500, 45000),
#'   times = c(0, 30),
#'   discount_rate = 0.06
#' )
#'
#' # Using the pine management schedule dataset
#' sched <- pine_schedule()
#' npv(
#'   cash_flows = sched$amount[sched$frequency == "once"],
#'   times = sched$year[sched$frequency == "once"],
#'   discount_rate = 0.06
#' )
#'
#' # Multiple activities: planting, PCT, thinning, final harvest
#' npv(
#'   cash_flows = c(-750, -800, 2500, 6600),
#'   times = c(0, 12, 20, 40),
#'   discount_rate = 0.06
#' )
#'
#' @family npv
#' @export
npv <- function(cash_flows, times, discount_rate) {
  # Input validation
  if (!is.numeric(cash_flows)) stop("'cash_flows' must be numeric", call. = FALSE)
  if (!is.numeric(times)) stop("'times' must be numeric", call. = FALSE)
  if (!is.numeric(discount_rate) || length(discount_rate) != 1) {
    stop("'discount_rate' must be a single numeric value", call. = FALSE)
  }
  if (length(cash_flows) != length(times)) {
    stop("'cash_flows' and 'times' must have the same length", call. = FALSE)
  }
  if (any(times < 0)) warning("Negative times detected; cash flows will be compounded")
  if (discount_rate < 0) warning("Negative discount rate")
  if (discount_rate > 0.25) warning("Discount rate exceeds 25%; verify this is intentional")

  sum(cash_flows / (1 + discount_rate)^times)
}


#' Incremental Net Present Value
#'
#' Compares two mutually exclusive alternatives by computing
#' NPV(alternative) - NPV(baseline). A positive incremental NPV means the
#' alternative is preferred; negative means the baseline is preferred.
#'
#' When comparing mutually exclusive projects (e.g., carbon credits vs.
#' business-as-usual), the project with the higher NPV should be chosen.
#' The incremental NPV quantifies the advantage.
#'
#' @param alt_flows Numeric vector. Cash flows for the alternative.
#' @param alt_times Numeric vector. Times for the alternative.
#' @param base_flows Numeric vector. Cash flows for the baseline.
#' @param base_times Numeric vector. Times for the baseline.
#' @param discount_rate Numeric scalar. Discount rate.
#'
#' @return A list with components:
#'   \describe{
#'     \item{npv_alternative}{NPV of the alternative}
#'     \item{npv_baseline}{NPV of the baseline}
#'     \item{incremental_npv}{NPV(alternative) - NPV(baseline)}
#'     \item{preferred}{Character: "alternative" or "baseline"}
#'   }
#'
#' @references
#' Klemperer, W.D. (1996). *Forest Resource Economics and Finance*. McGraw-Hill. Ch. 6.
#'
#' @examples
#' # Carbon credits vs. business-as-usual
#' carbon <- incremental_npv(
#'   alt_flows = c(-750, 4000, -500, -750, 2000),
#'   alt_times = c(0, 0, 0, 15, 40),
#'   base_flows = c(-750, -275, -800, 2500, 2500, 2500),
#'   base_times = c(0, 5, 12, 20, 30, 40),
#'   discount_rate = 0.06
#' )
#' carbon$incremental_npv
#'
#' @family npv
#' @export
incremental_npv <- function(alt_flows, alt_times, base_flows, base_times,
                             discount_rate) {
  npv_alt <- npv(alt_flows, alt_times, discount_rate)
  npv_base <- npv(base_flows, base_times, discount_rate)
  inc <- npv_alt - npv_base

  list(
    npv_alternative = npv_alt,
    npv_baseline = npv_base,
    incremental_npv = inc,
    preferred = if (inc >= 0) "alternative" else "baseline"
  )
}
