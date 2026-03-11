#' One-Way Sensitivity Analysis
#'
#' Varies a single parameter across a specified range while holding all other
#' parameters at their base case values. Returns the parameter-outcome mapping.
#'
#' One-way sensitivity analysis is the simplest form of uncertainty analysis.
#' It shows how sensitive an outcome (e.g., NPV) is to changes in a single
#' input assumption while assuming all other inputs are known with certainty.
#'
#' @param eval_fn A function that takes named parameters and returns a single
#'   numeric value.
#' @param param_name Character. Name of the parameter to vary.
#' @param param_range Numeric vector. Values to evaluate.
#' @param base_params A named list of base case parameter values.
#'
#' @return An object of class \code{"sensitivity_1way"} (a data.frame with
#'   columns \code{param_value} and \code{outcome}).
#'
#' @references
#' Klemperer, W.D. (1996). *Forest Resource Economics and Finance*.
#' McGraw-Hill. Ch. 10.
#'
#' Bettinger, P., Boston, K., Siry, J.P., & Grebner, D.L. (2017).
#' *Forest Management and Planning*. 2nd ed. Academic Press. Ch. 8, Sec. 8.4.
#'
#' @examples
#' my_npv <- function(price, cost, discount_rate) {
#'   npv(c(-cost, price * 150), c(0, 30), discount_rate)
#' }
#' sens <- sensitivity_1way(
#'   my_npv, "discount_rate", seq(0.02, 0.10, by = 0.01),
#'   base_params = list(price = 50, cost = 750, discount_rate = 0.06)
#' )
#' sens
#'
#' @seealso \code{\link{sensitivity_2way}}, \code{\link{tornado_plot}},
#'   \code{\link{breakeven_analysis}}
#'
#' @export
sensitivity_1way <- function(eval_fn, param_name, param_range, base_params) {
  if (!is.function(eval_fn)) stop("'eval_fn' must be a function", call. = FALSE)
  if (!is.character(param_name) || length(param_name) != 1) {
    stop("'param_name' must be a single character string", call. = FALSE)
  }
  if (!param_name %in% names(base_params)) {
    stop("'", param_name, "' not found in base_params", call. = FALSE)
  }

  results <- data.frame(
    param_value = param_range,
    outcome = NA_real_
  )

  for (i in seq_along(param_range)) {
    params <- base_params
    params[[param_name]] <- param_range[i]
    results$outcome[i] <- do.call(eval_fn, params)
  }

  structure(results,
            class = c("sensitivity_1way", "data.frame"),
            param_name = param_name,
            base_value = base_params[[param_name]])
}


#' Two-Way Sensitivity Analysis
#'
#' Varies two parameters simultaneously across a grid while holding all others
#' at base case values. Returns a matrix of outcomes.
#'
#' @param eval_fn A function that takes named parameters and returns a numeric.
#' @param param_x Character. Name of the first parameter (x-axis).
#' @param range_x Numeric vector. Values for param_x.
#' @param param_y Character. Name of the second parameter (y-axis).
#' @param range_y Numeric vector. Values for param_y.
#' @param base_params A named list of base case parameter values.
#'
#' @return An object of class \code{"sensitivity_2way"} (a list with
#'   \code{x_values}, \code{y_values}, \code{matrix} of outcomes).
#'
#' @references
#' Wagner, J.E. (2012). *Forestry Economics: A Managerial Approach*. Routledge.
#' Ch. 10.
#'
#' @examples
#' my_npv <- function(price, cost, discount_rate) {
#'   npv(c(-cost, price * 150), c(0, 30), discount_rate)
#' }
#' sens2 <- sensitivity_2way(
#'   my_npv, "price", seq(30, 70, by = 10),
#'   "discount_rate", seq(0.03, 0.09, by = 0.02),
#'   base_params = list(price = 50, cost = 750, discount_rate = 0.06)
#' )
#'
#' @seealso \code{\link{sensitivity_1way}}
#'
#' @export
sensitivity_2way <- function(eval_fn, param_x, range_x, param_y, range_y,
                              base_params) {
  if (!is.function(eval_fn)) stop("'eval_fn' must be a function", call. = FALSE)

  outcome_matrix <- matrix(NA_real_, nrow = length(range_x), ncol = length(range_y))
  rownames(outcome_matrix) <- range_x
  colnames(outcome_matrix) <- range_y

  for (i in seq_along(range_x)) {
    for (j in seq_along(range_y)) {
      params <- base_params
      params[[param_x]] <- range_x[i]
      params[[param_y]] <- range_y[j]
      outcome_matrix[i, j] <- do.call(eval_fn, params)
    }
  }

  structure(
    list(
      x_values = range_x,
      y_values = range_y,
      matrix = outcome_matrix,
      param_x = param_x,
      param_y = param_y
    ),
    class = "sensitivity_2way"
  )
}


#' Tornado Plot Data
#'
#' Computes the data for a tornado diagram: for each parameter, evaluates the
#' outcome at its low and high values while holding all others at base case.
#' Parameters are ranked by the width of their impact range.
#'
#' Tornado diagrams visually rank parameters by their influence on the outcome.
#' The widest bar = most influential parameter. This is a standard tool in
#' forest investment analysis for identifying which assumptions matter most.
#'
#' @param eval_fn A function that takes named parameters and returns a numeric.
#' @param param_ranges A named list of numeric vectors of length 2 (low, high)
#'   for each parameter to include in the tornado.
#' @param base_params A named list of base case parameter values.
#'
#' @return An object of class \code{"tornado"} (a data.frame with columns:
#'   \code{parameter}, \code{low_value}, \code{high_value}, \code{low_outcome},
#'   \code{high_outcome}, \code{range}, sorted by descending range).
#'
#' @references
#' Cubbage, F.W., O'Laughlin, J., & Bullock, C.S. (1993). *Forest Resource
#' Policy*. Wiley. Ch. 15.
#'
#' Klemperer, W.D. (1996). *Forest Resource Economics and Finance*.
#' McGraw-Hill. Ch. 10.
#'
#' @examples
#' my_npv <- function(price, cost, discount_rate) {
#'   npv(c(-cost, price * 150), c(0, 30), discount_rate)
#' }
#' tornado <- tornado_plot(
#'   my_npv,
#'   param_ranges = list(
#'     price = c(30, 70),
#'     cost = c(500, 1500),
#'     discount_rate = c(0.03, 0.09)
#'   ),
#'   base_params = list(price = 50, cost = 750, discount_rate = 0.06)
#' )
#' tornado
#'
#' @seealso \code{\link{sensitivity_1way}}, \code{\link{breakeven_analysis}}
#'
#' @export
tornado_plot <- function(eval_fn, param_ranges, base_params) {
  if (!is.function(eval_fn)) stop("'eval_fn' must be a function", call. = FALSE)
  if (!is.list(param_ranges)) stop("'param_ranges' must be a named list", call. = FALSE)

  base_outcome <- do.call(eval_fn, base_params)

  results <- data.frame(
    parameter = character(),
    low_value = numeric(),
    high_value = numeric(),
    low_outcome = numeric(),
    high_outcome = numeric(),
    range = numeric(),
    stringsAsFactors = FALSE
  )

  for (pname in names(param_ranges)) {
    rng <- param_ranges[[pname]]
    if (length(rng) != 2) {
      stop("Each param_range must have exactly 2 values (low, high)", call. = FALSE)
    }

    # Low value
    params_low <- base_params
    params_low[[pname]] <- rng[1]
    outcome_low <- do.call(eval_fn, params_low)

    # High value
    params_high <- base_params
    params_high[[pname]] <- rng[2]
    outcome_high <- do.call(eval_fn, params_high)

    results <- rbind(results, data.frame(
      parameter = pname,
      low_value = rng[1],
      high_value = rng[2],
      low_outcome = outcome_low,
      high_outcome = outcome_high,
      range = abs(outcome_high - outcome_low),
      stringsAsFactors = FALSE
    ))
  }

  # Sort by range (most impactful first)
  results <- results[order(-results$range), ]
  rownames(results) <- NULL

  structure(results,
            class = c("tornado", "data.frame"),
            base_outcome = base_outcome)
}


#' Breakeven Analysis
#'
#' Finds the value of a parameter that makes the outcome equal to a target
#' (default: NPV = 0). Uses numerical root-finding.
#'
#' Breakeven analysis answers questions like: "What stumpage price makes this
#' investment break even?" or "What discount rate makes the carbon project
#' equivalent to business-as-usual?"
#'
#' @param eval_fn A function that takes named parameters and returns a numeric.
#' @param param_name Character. Parameter to solve for.
#' @param base_params Named list of base case parameter values.
#' @param target Numeric. Target outcome value. Default 0 (breakeven).
#' @param interval Numeric vector of length 2. Search interval. Default uses
#'   the base value +/- 90\%.
#'
#' @return A list with:
#'   \describe{
#'     \item{parameter}{Name of the parameter.}
#'     \item{breakeven_value}{The parameter value where outcome = target.}
#'     \item{target}{The target value.}
#'   }
#'
#' @references
#' Bettinger, P., Boston, K., Siry, J.P., & Grebner, D.L. (2017).
#' *Forest Management and Planning*. 2nd ed. Academic Press. Ch. 8.
#'
#' @examples
#' my_npv <- function(price, cost, discount_rate) {
#'   npv(c(-cost, price * 150), c(0, 30), discount_rate)
#' }
#' be <- breakeven_analysis(
#'   my_npv, "price",
#'   base_params = list(price = 50, cost = 750, discount_rate = 0.06),
#'   target = 0
#' )
#' cat("Breakeven price:", round(be$breakeven_value, 2), "$/unit\n")
#'
#' @export
breakeven_analysis <- function(eval_fn, param_name, base_params,
                                target = 0, interval = NULL) {
  if (!is.function(eval_fn)) stop("'eval_fn' must be a function", call. = FALSE)
  if (!param_name %in% names(base_params)) {
    stop("'", param_name, "' not found in base_params", call. = FALSE)
  }

  if (is.null(interval)) {
    base_val <- base_params[[param_name]]
    if (base_val == 0) {
      interval <- c(-1, 1)
    } else {
      interval <- c(base_val * 0.1, base_val * 10)
    }
  }

  obj <- function(x) {
    params <- base_params
    params[[param_name]] <- x
    do.call(eval_fn, params) - target
  }

  result <- tryCatch(
    stats::uniroot(obj, interval = interval, tol = 1e-8),
    error = function(e) {
      stop("Could not find breakeven in interval [", interval[1], ", ",
           interval[2], "]. Try widening the 'interval' parameter.",
           call. = FALSE)
    }
  )

  list(
    parameter = param_name,
    breakeven_value = result$root,
    target = target
  )
}
