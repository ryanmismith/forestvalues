#' Internal Rate of Return
#'
#' Finds the discount rate that makes NPV equal to zero using numerical
#' root-finding (\code{stats::uniroot}). The IRR has no closed-form solution.
#'
#' **Important**: IRR can give misleading results for non-conventional cash
#' flows (multiple sign changes). When cash flows change sign more than once,
#' multiple IRRs may exist (Descartes' rule of signs). In these cases, consider
#' using \code{\link{mirr}} instead.
#'
#' **IRR vs. NPV for ranking**: IRR and NPV can give conflicting rankings for
#' mutually exclusive projects of different scales or durations (the Fisher
#' intersection problem). NPV is generally preferred for ranking mutually
#' exclusive alternatives.
#'
#' @param cash_flows Numeric vector. Signed cash flows (negative = cost,
#'   positive = revenue). Must contain at least one positive and one negative value.
#' @param times Numeric vector. Year each cash flow occurs. Same length as
#'   \code{cash_flows}.
#' @param interval Numeric vector of length 2. Search interval for the IRR.
#'   Default \code{c(-0.5, 10)} covers -50\% to 1000\%.
#'
#' @return Numeric scalar. The internal rate of return as a decimal
#'   (e.g., 0.08 for 8\%).
#'
#' @references
#' Boulding, K.E. (1935). "The theory of a single investment."
#' *Quarterly Journal of Economics* 49(3): 475-494.
#'
#' Klemperer, W.D. (1996). *Forest Resource Economics and Finance*.
#' McGraw-Hill. Ch. 7.
#'
#' Hazen, G.B. (2003). "A new perspective on multiple internal rates of return."
#' *The Engineering Economist* 48(1): 31-51.
#'
#' @examples
#' # Simple plantation: invest $1,500, harvest $45,000 in 30 years
#' irr(c(-1500, 45000), c(0, 30))
#'
#' # Multiple activities
#' irr(
#'   cash_flows = c(-750, -800, 2500, 6600),
#'   times = c(0, 12, 20, 40)
#' )
#'
#' @seealso \code{\link{mirr}} for Modified IRR, \code{\link{npv}}
#'
#' @family irr
#' @export
irr <- function(cash_flows, times, interval = c(-0.5, 10)) {
  # Input validation
  if (!is.numeric(cash_flows)) stop("'cash_flows' must be numeric", call. = FALSE)
  if (!is.numeric(times)) stop("'times' must be numeric", call. = FALSE)
  if (length(cash_flows) != length(times)) {
    stop("'cash_flows' and 'times' must have the same length", call. = FALSE)
  }
  if (length(cash_flows) < 2) {
    stop("At least two cash flows are required", call. = FALSE)
  }
  if (all(cash_flows >= 0) || all(cash_flows <= 0)) {
    stop("Cash flows must contain at least one positive and one negative value",
         call. = FALSE)
  }

  # Check for multiple sign changes (Descartes' rule)
  nonzero <- cash_flows[cash_flows != 0]
  signs <- sign(nonzero)
  n_sign_changes <- sum(diff(signs) != 0)
  if (n_sign_changes > 1) {
    warning("Multiple sign changes in cash flows (", n_sign_changes,
            "): multiple IRRs may exist. Consider using mirr() instead.",
            call. = FALSE)
  }

  # NPV as a function of rate
  npv_at_rate <- function(r) {
    sum(cash_flows / (1 + r)^times)
  }

  # Solve for IRR
  result <- tryCatch(
    stats::uniroot(npv_at_rate, interval = interval, tol = 1e-10),
    error = function(e) {
      stop("Could not find IRR in interval [", interval[1], ", ", interval[2],
           "]. Try widening the 'interval' parameter.", call. = FALSE)
    }
  )

  result$root
}


#' Modified Internal Rate of Return
#'
#' Computes the Modified IRR, which resolves two problems with standard IRR:
#' (1) the multiple-IRR problem for non-conventional cash flows, and
#' (2) the implicit reinvestment rate assumption.
#'
#' MIRR separates the financing rate (applied to negative cash flows) from
#' the reinvestment rate (applied to positive cash flows). It always yields
#' a unique solution.
#'
#' MIRR = (FV_positives / |PV_negatives|)^(1/n) - 1
#'
#' where FV_positives = sum of positive flows compounded to period n at the
#' reinvestment rate, and PV_negatives = sum of negative flows discounted
#' to period 0 at the finance rate.
#'
#' @param cash_flows Numeric vector. Signed cash flows.
#' @param times Numeric vector. Year each cash flow occurs.
#' @param finance_rate Numeric scalar. Rate for discounting negative cash flows
#'   (cost of capital).
#' @param reinvest_rate Numeric scalar. Rate for compounding positive cash flows
#'   (reinvestment opportunity rate).
#'
#' @return Numeric scalar. The modified internal rate of return.
#'
#' @references
#' Hazen, G.B. (2003). "A new perspective on multiple internal rates of return."
#' *The Engineering Economist* 48(1): 31-51.
#'
#' Klemperer, W.D. (1996). *Forest Resource Economics and Finance*.
#' McGraw-Hill. Ch. 7.
#'
#' @examples
#' # Plantation with reinvestment at 4%, financed at 6%
#' mirr(
#'   cash_flows = c(-1500, -800, 2500, 6600),
#'   times = c(0, 12, 20, 40),
#'   finance_rate = 0.06,
#'   reinvest_rate = 0.04
#' )
#'
#' @seealso \code{\link{irr}} for standard IRR
#'
#' @family irr
#' @export
mirr <- function(cash_flows, times, finance_rate, reinvest_rate) {
  # Input validation
  if (!is.numeric(cash_flows)) stop("'cash_flows' must be numeric", call. = FALSE)
  if (!is.numeric(times)) stop("'times' must be numeric", call. = FALSE)
  if (length(cash_flows) != length(times)) {
    stop("'cash_flows' and 'times' must have the same length", call. = FALSE)
  }
  if (!is.numeric(finance_rate) || length(finance_rate) != 1) {
    stop("'finance_rate' must be a single numeric value", call. = FALSE)
  }
  if (!is.numeric(reinvest_rate) || length(reinvest_rate) != 1) {
    stop("'reinvest_rate' must be a single numeric value", call. = FALSE)
  }
  if (all(cash_flows >= 0) || all(cash_flows <= 0)) {
    stop("Cash flows must contain at least one positive and one negative value",
         call. = FALSE)
  }

  n <- max(times)  # total time horizon

  # FV of positive cash flows at reinvestment rate
  positives <- cash_flows > 0
  fv_pos <- sum(cash_flows[positives] * (1 + reinvest_rate)^(n - times[positives]))

  # PV of negative cash flows at finance rate
  negatives <- cash_flows < 0
  pv_neg <- sum(abs(cash_flows[negatives]) / (1 + finance_rate)^times[negatives])

  # MIRR formula
  (fv_pos / pv_neg)^(1 / n) - 1
}
