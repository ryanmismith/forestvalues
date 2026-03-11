#' Optimal Rotation Age
#'
#' Finds the rotation age that maximizes either Land Expectation Value
#' (Faustmann rotation), single-rotation NPV, or Mean Annual Increment (MAI).
#'
#' The **Faustmann rotation** (criterion = "lev") finds the age that maximizes
#' the value of bare land managed in perpetuity. This is the economically
#' correct criterion when land has an opportunity cost (i.e., you could start
#' a new rotation). The Faustmann rotation is always shorter than the
#' biological rotation (MAI).
#'
#' The **financial maturity** criterion (criterion = "npv") maximizes
#' single-rotation NPV. This is appropriate only when there is no opportunity
#' cost of land (e.g., the land will not be replanted).
#'
#' The **MAI** criterion finds the age of maximum Mean Annual Increment
#' (volume/age). This is the biological rotation and is not an economic criterion.
#'
#' The marginal condition for the Faustmann rotation is: harvest when the
#' marginal value growth rate equals r + r/((1+r)^T - 1), i.e., the interest
#' rate plus a land rent charge (Pressler 1860).
#'
#' @param yield_fn A function that takes age (numeric) and returns volume,
#'   OR a data.frame with columns \code{age} and \code{volume}. If a data.frame,
#'   a cubic spline is fitted for interpolation.
#' @param stumpage_price Numeric scalar. Price per unit volume at harvest.
#' @param regen_cost Numeric scalar. Regeneration/planting cost at year 0.
#' @param annual_cost Numeric scalar. Annual costs (e.g., property tax).
#'   Default 0.
#' @param discount_rate Numeric scalar. Real discount rate.
#' @param age_range Numeric vector of length 2. Min and max ages to search.
#'   Default \code{c(10, 150)}.
#' @param criterion Character. One of \code{"lev"} (Faustmann rotation, default),
#'   \code{"npv"} (single rotation), or \code{"mai"} (biological rotation).
#'
#' @return A list with components:
#'   \describe{
#'     \item{optimal_age}{Numeric. The optimal rotation age.}
#'     \item{value_at_optimum}{Numeric. The LEV, NPV, or MAI at the optimum.}
#'     \item{criterion}{Character. The criterion used.}
#'   }
#'
#' @references
#' Faustmann, M. (1849). "Berechnung des Werthes welchen Waldboden sowie noch
#' nicht haubare Holzbestaende fuer die Waldwirthschaft besitzen."
#' *Allgemeine Forst- und Jagd-Zeitung* 25: 441-455.
#'
#' Pressler, M.R. (1860). "Aus der Holzzuwachlehre."
#' *Allgemeine Forst- und Jagd-Zeitung* 36: 173-191.
#'
#' Newman, D.H. (2002). "Forestry's golden rule and the development of the
#' optimal forest rotation literature." *Journal of Forest Economics* 8(1): 5-27.
#'
#' Amacher, G.S., Ollikainen, M., & Koskela, E. (2009). *Economics of Forest
#' Resources*. MIT Press. Ch. 2.
#'
#' @examples
#' # Yield function: volume = 200 * (1 - exp(-0.03 * age))^3
#' yield <- function(age) 200 * (1 - exp(-0.03 * age))^3
#'
#' # Faustmann rotation
#' optimal_rotation(yield, stumpage_price = 50, regen_cost = 750,
#'                   annual_cost = 5, discount_rate = 0.06)
#'
#' # Compare all three criteria
#' for (crit in c("lev", "npv", "mai")) {
#'   result <- optimal_rotation(yield, 50, 750, 5, 0.06, criterion = crit)
#'   cat(crit, ": age =", round(result$optimal_age), "\n")
#' }
#'
#' @seealso \code{\link{rotation_comparison}}, \code{\link{lev}}
#'
#' @export
optimal_rotation <- function(yield_fn, stumpage_price, regen_cost,
                              annual_cost = 0, discount_rate,
                              age_range = c(10, 150),
                              criterion = c("lev", "npv", "mai")) {
  criterion <- match.arg(criterion)

  # Convert data.frame to function via spline

  if (is.data.frame(yield_fn)) {
    if (!all(c("age", "volume") %in% names(yield_fn))) {
      stop("yield data.frame must have 'age' and 'volume' columns", call. = FALSE)
    }
    yield_data <- yield_fn
    yield_fn <- stats::splinefun(yield_data$age, yield_data$volume,
                                  method = "natural")
  }

  if (!is.function(yield_fn)) {
    stop("'yield_fn' must be a function or a data.frame with 'age' and 'volume'",
         call. = FALSE)
  }

  if (!is.numeric(stumpage_price) || length(stumpage_price) != 1) {
    stop("'stumpage_price' must be a single numeric value", call. = FALSE)
  }
  if (!is.numeric(discount_rate) || length(discount_rate) != 1) {
    stop("'discount_rate' must be a single numeric value", call. = FALSE)
  }

  r <- discount_rate

  objective <- function(age) {
    vol <- yield_fn(age)
    revenue <- vol * stumpage_price

    if (criterion == "mai") {
      return(vol / age)
    } else if (criterion == "npv") {
      # Single rotation NPV
      pv_revenue <- revenue / (1 + r)^age
      pv_regen <- regen_cost  # at year 0
      if (annual_cost > 0 && r > 0) {
        pv_annual <- annual_cost * ((1 + r)^age - 1) / (r * (1 + r)^age)
      } else {
        pv_annual <- annual_cost * age
      }
      return(pv_revenue - pv_regen - pv_annual)
    } else {
      # LEV criterion
      # NPV of single rotation
      pv_revenue <- revenue / (1 + r)^age
      pv_regen <- regen_cost
      if (annual_cost > 0 && r > 0) {
        pv_annual <- annual_cost * ((1 + r)^age - 1) / (r * (1 + r)^age)
      } else {
        pv_annual <- annual_cost * age
      }
      rotation_npv <- pv_revenue - pv_regen - pv_annual
      # Convert to LEV
      return(rotation_npv * (1 + r)^age / ((1 + r)^age - 1))
    }
  }

  result <- stats::optimize(objective, interval = age_range, maximum = TRUE)

  list(
    optimal_age = result$maximum,
    value_at_optimum = result$objective,
    criterion = criterion
  )
}


#' Compare Rotation Ages
#'
#' Tabulates NPV, LEV, MAI, and volume across a range of rotation ages for
#' visual comparison and decision support.
#'
#' @param yield_fn A function or data.frame (see \code{\link{optimal_rotation}}).
#' @param stumpage_price Numeric scalar. Price per unit volume.
#' @param regen_cost Numeric scalar. Regeneration cost.
#' @param annual_cost Numeric scalar. Annual costs. Default 0.
#' @param discount_rate Numeric scalar. Discount rate.
#' @param ages Numeric vector. Ages to evaluate. Default \code{seq(10, 100, by = 5)}.
#'
#' @return A data.frame with columns: \code{age}, \code{volume}, \code{mai},
#'   \code{revenue}, \code{npv}, \code{lev}.
#'
#' @references
#' Newman, D.H. (2002). "Forestry's golden rule and the development of the
#' optimal forest rotation literature." *Journal of Forest Economics* 8(1): 5-27.
#'
#' @examples
#' yield <- function(age) 200 * (1 - exp(-0.03 * age))^3
#' comp <- rotation_comparison(yield, 50, 750, 5, 0.06)
#' head(comp)
#'
#' @seealso \code{\link{optimal_rotation}}
#'
#' @export
rotation_comparison <- function(yield_fn, stumpage_price, regen_cost,
                                 annual_cost = 0, discount_rate,
                                 ages = seq(10, 100, by = 5)) {
  # Convert data.frame to function if needed
  if (is.data.frame(yield_fn)) {
    if (!all(c("age", "volume") %in% names(yield_fn))) {
      stop("yield data.frame must have 'age' and 'volume' columns", call. = FALSE)
    }
    yield_data <- yield_fn
    yield_fn <- stats::splinefun(yield_data$age, yield_data$volume,
                                  method = "natural")
  }

  r <- discount_rate

  results <- data.frame(
    age = ages,
    volume = NA_real_,
    mai = NA_real_,
    revenue = NA_real_,
    npv = NA_real_,
    lev = NA_real_
  )

  for (i in seq_along(ages)) {
    age <- ages[i]
    vol <- yield_fn(age)
    rev <- vol * stumpage_price

    results$volume[i] <- vol
    results$mai[i] <- vol / age
    results$revenue[i] <- rev

    # NPV
    pv_rev <- rev / (1 + r)^age
    pv_regen <- regen_cost
    if (annual_cost > 0 && r > 0) {
      pv_annual <- annual_cost * ((1 + r)^age - 1) / (r * (1 + r)^age)
    } else {
      pv_annual <- annual_cost * age
    }
    rot_npv <- pv_rev - pv_regen - pv_annual
    results$npv[i] <- rot_npv

    # LEV
    results$lev[i] <- rot_npv * (1 + r)^age / ((1 + r)^age - 1)
  }

  class(results) <- c("rotation_comparison", "data.frame")
  results
}
