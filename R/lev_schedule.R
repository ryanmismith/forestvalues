#' Land Expectation Value from a Management Schedule
#'
#' Calculates LEV for a complex management schedule with mixed payment types.
#' First computes the single-rotation NPV using \code{\link{npv_schedule}},
#' then converts to LEV assuming identical rotations in perpetuity.
#'
#' This is the generalized Faustmann formula for realistic management
#' scenarios that include planting costs, annual taxes, periodic thinnings,
#' and final harvest revenue.
#'
#' @param schedule A data.frame with columns: \code{amount}, \code{year},
#'   \code{frequency}, \code{period_length}. See \code{\link{npv_schedule}} for
#'   format details.
#' @param discount_rate Numeric scalar. The discount rate.
#' @param rotation_age Numeric scalar. Rotation length in years (used as
#'   time_horizon for NPV and as T in the LEV formula).
#'
#' @return Numeric scalar. The land expectation value.
#'
#' @references
#' Faustmann, M. (1849). "Berechnung des Werthes welchen Waldboden sowie noch
#' nicht haubare Holzbestaende fuer die Waldwirthschaft besitzen."
#' *Allgemeine Forst- und Jagd-Zeitung* 25: 441-455.
#'
#' Chang, S.J. (1998). "A generalized Faustmann model for the determination of
#' optimal harvest age." *Canadian Journal of Forest Research* 28(5): 652-659.
#'
#' @examples
#' # Business-as-usual scenario
#' bau <- data.frame(
#'   amount = c(-750, -50, -275, -800, 2500, 2500, 2500),
#'   year = c(0, 0, 5, 12, 20, 30, 40),
#'   frequency = c("once", "annual", "once", "once", "once", "once", "once"),
#'   period_length = c(NA, NA, NA, NA, NA, NA, NA)
#' )
#' lev_schedule(bau, discount_rate = 0.06, rotation_age = 40)
#'
#' @seealso \code{\link{lev}}, \code{\link{npv_schedule}}
#'
#' @family lev
#' @export
lev_schedule <- function(schedule, discount_rate, rotation_age) {
  rotation_npv <- npv_schedule(schedule, discount_rate, rotation_age)
  lev(rotation_npv, rotation_age, discount_rate, is_npv = TRUE)
}
