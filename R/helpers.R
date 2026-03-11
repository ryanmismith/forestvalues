#' Discount a future value to present value
#'
#' Calculates the present value of a future cash flow using the standard
#' discounting formula: PV = FV / (1 + r)^t.
#'
#' @param future_value Numeric. The future cash flow amount.
#' @param rate Numeric. The discount rate (e.g., 0.06 for 6\%).
#' @param time Numeric. Years until the cash flow occurs.
#'
#' @return Numeric. The present value.
#'
#' @references
#' Klemperer, W.D. (1996). *Forest Resource Economics and Finance*. McGraw-Hill. Ch. 4.
#'
#' @examples
#' # PV of $100,000 harvest revenue in 30 years at 6%
#' discount(100000, 0.06, 30)
#'
#' # Vectorized: multiple cash flows
#' discount(c(50000, 100000), 0.06, c(20, 40))
#'
#' @export
discount <- function(future_value, rate, time) {
  if (!is.numeric(future_value)) stop("'future_value' must be numeric", call. = FALSE)
  if (!is.numeric(rate)) stop("'rate' must be numeric", call. = FALSE)
  if (!is.numeric(time)) stop("'time' must be numeric", call. = FALSE)
  if (any(time < 0)) warning("Negative 'time' values will compound rather than discount")
  future_value / (1 + rate)^time
}


#' Compound a present value to future value
#'
#' Calculates the future value of a present cash flow using the standard
#' compounding formula: FV = PV * (1 + r)^t.
#'
#' @param present_value Numeric. The present cash flow amount.
#' @param rate Numeric. The interest/growth rate (e.g., 0.06 for 6\%).
#' @param time Numeric. Years to compound.
#'
#' @return Numeric. The future value.
#'
#' @references
#' Klemperer, W.D. (1996). *Forest Resource Economics and Finance*. McGraw-Hill. Ch. 4.
#'
#' @examples
#' # FV of $5,000 planting cost after 30 years at 6%
#' compound(5000, 0.06, 30)
#'
#' @export
compound <- function(present_value, rate, time) {
  if (!is.numeric(present_value)) stop("'present_value' must be numeric", call. = FALSE)
  if (!is.numeric(rate)) stop("'rate' must be numeric", call. = FALSE)
  if (!is.numeric(time)) stop("'time' must be numeric", call. = FALSE)
  present_value * (1 + rate)^time
}
