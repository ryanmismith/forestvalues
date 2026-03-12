#' Land Expectation Value (Faustmann Formula)
#'
#' Calculates the Land Expectation Value (LEV), also known as Bare Land Value
#' (BLV) or Soil Expectation Value (SEV). This is the present value of bare
#' forestland assuming an infinite series of identical rotations.
#'
#' The Faustmann formula is the most important equation in forest economics.
#' It answers: "What is this bare land worth if managed optimally in perpetuity?"
#'
#' LEV = NPV_rotation * (1+r)^T / ((1+r)^T - 1)
#'
#' For the simple case where net_revenue is the lump sum at rotation age T:
#' LEV = net_revenue / ((1+r)^T - 1)
#'
#' @param net_revenue Numeric scalar. Net revenue (revenues minus costs) at
#'   rotation age. If this is already a present value (NPV of the rotation),
#'   set \code{is_npv = TRUE}.
#' @param rotation_age Numeric scalar. Rotation length in years.
#' @param discount_rate Numeric scalar. Real discount rate.
#' @param is_npv Logical. If \code{TRUE}, treats \code{net_revenue} as the NPV
#'   of a single rotation (already discounted to year 0) and applies the
#'   perpetuity multiplier. If \code{FALSE} (default), treats \code{net_revenue}
#'   as a future value at the rotation age. Default: \code{FALSE}.
#'
#' @return Numeric scalar. The land expectation value.
#'
#' @references
#' Faustmann, M. (1849). "Berechnung des Werthes welchen Waldboden sowie noch
#' nicht haubare Holzbestaende fuer die Waldwirthschaft besitzen."
#' *Allgemeine Forst- und Jagd-Zeitung* 25: 441-455.
#'
#' Samuelson, P.A. (1976). "Economics of forestry in an evolving society."
#' *Economic Inquiry* 14(4): 466-492.
#'
#' Chang, S.J. (1998). "A generalized Faustmann model for the determination of
#' optimal harvest age." *Canadian Journal of Forest Research* 28(5): 652-659.
#'
#' Brazee, R.J. (2001). "The Faustmann Formula: Fundamental to Forest Economics
#' 150 Years After Publication." *Forest Science* 47(4): 441-442.
#'
#' @examples
#' # Simple case: $52,000 net revenue at age 40, 6% discount rate
#' lev(52000, rotation_age = 40, discount_rate = 0.06)
#'
#' # From an NPV: rotation NPV = $5,000
#' lev(5000, rotation_age = 30, discount_rate = 0.06, is_npv = TRUE)
#'
#' # Compare LEV across rotation ages
#' sapply(seq(20, 60, by = 5), function(age) {
#'   lev(age * 1200, rotation_age = age, discount_rate = 0.06)
#' })
#'
#' @seealso \code{\link{bare_land_value}} (alias), \code{\link{lev_schedule}},
#'   \code{\link{optimal_rotation}}
#'
#' @family lev
#' @export
lev <- function(net_revenue, rotation_age, discount_rate, is_npv = FALSE) {
  # Input validation
  if (!is.numeric(net_revenue) || length(net_revenue) != 1) {
    stop("'net_revenue' must be a single numeric value", call. = FALSE)
  }
  if (!is.numeric(rotation_age) || length(rotation_age) != 1) {
    stop("'rotation_age' must be a single numeric value", call. = FALSE)
  }
  if (!is.numeric(discount_rate) || length(discount_rate) != 1) {
    stop("'discount_rate' must be a single numeric value", call. = FALSE)
  }
  if (rotation_age <= 0) stop("'rotation_age' must be positive", call. = FALSE)
  if (discount_rate <= 0) stop("'discount_rate' must be positive for LEV", call. = FALSE)
  if (discount_rate > 0.25) {
    warning("Discount rate exceeds 25%; verify this is intentional", call. = FALSE)
  }

  if (is_npv) {
    # net_revenue is already NPV of the rotation
    # LEV = NPV * (1+r)^T / ((1+r)^T - 1)
    net_revenue * (1 + discount_rate)^rotation_age /
      ((1 + discount_rate)^rotation_age - 1)
  } else {
    # net_revenue is a future value at the rotation age
    # LEV = NR / ((1+r)^T - 1)
    net_revenue / ((1 + discount_rate)^rotation_age - 1)
  }
}


#' Bare Land Value
#'
#' Alias for \code{\link{lev}}. "Bare Land Value" (BLV) is the practitioner
#' term for what forest economists call "Land Expectation Value" (LEV) or
#' "Soil Expectation Value" (SEV). They are mathematically identical.
#'
#' @inheritParams lev
#'
#' @return Numeric scalar. The bare land value.
#'
#' @references
#' Faustmann, M. (1849). "Berechnung des Werthes welchen Waldboden sowie noch
#' nicht haubare Holzbestaende fuer die Waldwirthschaft besitzen."
#' *Allgemeine Forst- und Jagd-Zeitung* 25: 441-455.
#'
#' @examples
#' bare_land_value(52000, rotation_age = 40, discount_rate = 0.06)
#'
#' @seealso \code{\link{lev}}
#'
#' @family lev
#' @export
bare_land_value <- function(net_revenue, rotation_age, discount_rate,
                             is_npv = FALSE) {
  lev(net_revenue, rotation_age, discount_rate, is_npv)
}
