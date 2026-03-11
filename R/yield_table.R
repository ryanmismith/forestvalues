#' Create a Multi-Product Yield Table
#'
#' Builds a yield table with multiple timber products (sawlog, pulpwood, veneer,
#' etc.), each with its own volume trajectory and stumpage price. The resulting
#' object can be passed directly to \code{\link{optimal_rotation_mp}} or used
#' for scenario analysis.
#'
#' In practice, a stand produces multiple products simultaneously: sawlog volume
#' increases with age as trees reach merchantable diameters, pulpwood comes from
#' tops and smaller stems, and veneer-quality logs only appear at older ages.
#' Each product has a different stumpage price. This function captures that
#' reality.
#'
#' @param ages Numeric vector. Stand ages (years) at which volumes are observed
#'   or projected.
#' @param products A named list of data.frames, one per product class. Each
#'   data.frame must have a column \code{volume} (volume at each age).
#'   Optionally include a \code{price} column; if missing, price must be
#'   supplied separately to analysis functions.
#' @param product_units Named character vector. Units for each product
#'   (e.g., \code{c(sawlog = "mbf", pulp = "cord")}). Optional but recommended
#'   for documentation.
#'
#' @return An object of class \code{"yield_table"} containing:
#'   \describe{
#'     \item{ages}{Numeric vector of ages.}
#'     \item{products}{Named list of product data (volume, price, unit).}
#'     \item{total_value_fn}{Function that returns total $/acre at any age (spline-interpolated).}
#'     \item{product_fns}{Named list of spline functions, one per product.}
#'     \item{data}{Long-format data.frame of all product volumes by age.}
#'   }
#'
#' @references
#' Bettinger, P., Boston, K., Siry, J.P., & Grebner, D.L. (2017).
#' *Forest Management and Planning*. 2nd ed. Academic Press. Ch. 3, 8.
#'
#' Klemperer, W.D. (1996). *Forest Resource Economics and Finance*.
#' McGraw-Hill. Ch. 12.
#'
#' @examples
#' # Typical northern hardwood stand: sawlog + pulp
#' ages <- seq(10, 80, by = 10)
#'
#' yt <- yield_table(
#'   ages = ages,
#'   products = list(
#'     sawlog = data.frame(
#'       volume = c(0, 0, 2, 5, 10, 16, 20, 22),
#'       price = rep(254, 8)  # $/MBF sugar maple
#'     ),
#'     pulp = data.frame(
#'       volume = c(5, 12, 18, 22, 24, 25, 25, 24),
#'       price = rep(9, 8)  # $/ton hardwood pulp
#'     )
#'   ),
#'   product_units = c(sawlog = "mbf", pulp = "ton")
#' )
#' yt
#'
#' # Total value at age 50
#' yt$total_value_fn(50)
#'
#' # Use with optimal rotation
#' optimal_rotation_mp(yt, regen_cost = 750, annual_cost = 50,
#'                      discount_rate = 0.06)
#'
#' @seealso \code{\link{optimal_rotation_mp}}, \code{\link{optimal_rotation}}
#'
#' @export
yield_table <- function(ages, products, product_units = NULL) {
  # Validation
  if (!is.numeric(ages)) stop("'ages' must be a numeric vector", call. = FALSE)
  if (!is.list(products) || is.null(names(products))) {
    stop("'products' must be a named list of data.frames", call. = FALSE)
  }
  if (length(ages) < 3) {
    stop("Need at least 3 age observations for spline interpolation", call. = FALSE)
  }

  product_names <- names(products)
  product_fns <- list()       # spline functions for volume
  value_fns <- list()         # spline functions for value (vol * price)
  all_data <- data.frame()

  for (pname in product_names) {
    pdata <- products[[pname]]

    if (!is.data.frame(pdata)) {
      stop("products$", pname, " must be a data.frame", call. = FALSE)
    }
    if (!"volume" %in% names(pdata)) {
      stop("products$", pname, " must have a 'volume' column", call. = FALSE)
    }
    if (nrow(pdata) != length(ages)) {
      stop("products$", pname, " must have ", length(ages),
           " rows (one per age)", call. = FALSE)
    }

    vol <- pdata$volume

    # Price: from data or NA
    if ("price" %in% names(pdata)) {
      price <- pdata$price
      if (length(unique(price)) == 1) {
        # Constant price — simple
        price_val <- price[1]
      } else {
        price_val <- price  # age-varying prices
      }
    } else {
      price_val <- NA
    }

    # Build spline for volume
    vol_fn <- stats::splinefun(ages, vol, method = "natural")
    product_fns[[pname]] <- vol_fn

    # Build value spline if price available
    if (!all(is.na(price_val))) {
      if (length(price_val) == 1) {
        value_at_ages <- vol * price_val
      } else {
        value_at_ages <- vol * price_val
      }
      value_fns[[pname]] <- stats::splinefun(ages, value_at_ages, method = "natural")
    }

    # Unit
    unit <- if (!is.null(product_units) && pname %in% names(product_units)) {
      product_units[[pname]]
    } else {
      "units"
    }

    # Long-format data
    row_data <- data.frame(
      age = ages,
      product = pname,
      volume = vol,
      unit = unit,
      stringsAsFactors = FALSE
    )
    if (!all(is.na(price_val))) {
      if (length(price_val) == 1) {
        row_data$price <- price_val
        row_data$value <- vol * price_val
      } else {
        row_data$price <- price_val
        row_data$value <- vol * price_val
      }
    } else {
      row_data$price <- NA_real_
      row_data$value <- NA_real_
    }
    all_data <- rbind(all_data, row_data)
  }

  # Total value function (sum across products)
  total_value_fn <- function(age) {
    total <- 0
    for (vfn in value_fns) {
      val <- vfn(age)
      val[val < 0] <- 0  # spline can go negative at extremes
      total <- total + val
    }
    total
  }

  result <- list(
    ages = ages,
    product_names = product_names,
    products = products,
    product_units = product_units,
    product_fns = product_fns,
    value_fns = value_fns,
    total_value_fn = total_value_fn,
    data = all_data
  )
  class(result) <- "yield_table"
  result
}


#' @export
print.yield_table <- function(x, ...) {
  cat("Multi-Product Yield Table\n")
  cat("Products:", paste(x$product_names, collapse = ", "), "\n")
  cat("Ages:", min(x$ages), "to", max(x$ages), "years",
      "(", length(x$ages), "observations)\n\n")

  # Print summary by product
  for (pname in x$product_names) {
    pdata <- x$data[x$data$product == pname, ]
    unit <- pdata$unit[1]
    cat(pname, " (", unit, "):\n", sep = "")
    cat("  Volume range:", round(min(pdata$volume), 1), "-",
        round(max(pdata$volume), 1), "\n")
    if (!all(is.na(pdata$price))) {
      cat("  Price:", round(unique(stats::na.omit(pdata$price))[1], 2), "\n")
      cat("  Value range: $", round(min(pdata$value, na.rm = TRUE), 2), " - $",
          round(max(pdata$value, na.rm = TRUE), 2), "\n")
    }
  }

  # Total value at each age
  total_vals <- sapply(x$ages, x$total_value_fn)
  cat("\nTotal value/acre range: $", round(min(total_vals), 2), " - $",
      round(max(total_vals), 2), "\n")

  invisible(x)
}


#' Plot a Yield Table
#'
#' @param x A \code{yield_table} object.
#' @param type Character. "volume" to plot volumes, "value" to plot dollar values.
#' @param ... Additional arguments (unused).
#'
#' @return A ggplot2 object.
#' @export
plot.yield_table <- function(x, type = c("value", "volume"), ...) {
  type <- match.arg(type)
  y_col <- if (type == "volume") "volume" else "value"
  y_lab <- if (type == "volume") "Volume" else "Value ($)"

  if (type == "value" && all(is.na(x$data$value))) {
    stop("No prices available. Use type = 'volume' or supply prices.", call. = FALSE)
  }

  p <- ggplot2::ggplot(x$data, ggplot2::aes(x = .data$age, y = .data[[y_col]],
                                              color = .data$product)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(x = "Stand Age (years)", y = y_lab, color = "Product",
                  title = paste("Yield Table:", type)) +
    ggplot2::theme_minimal()

  p
}


#' Optimal Rotation for Multi-Product Yield Tables
#'
#' Finds the optimal rotation age for a stand that produces multiple timber
#' products (sawlog, pulp, veneer, etc.) with different volumes and prices
#' at each age. This is the multi-product generalization of
#' \code{\link{optimal_rotation}}.
#'
#' Revenue at any age is the sum of (volume_i * price_i) across all products.
#' The function then optimizes LEV, NPV, or MAI over age.
#'
#' @param yield_tbl A \code{yield_table} object created by
#'   \code{\link{yield_table}}.
#' @param regen_cost Numeric scalar. Regeneration/planting cost at year 0.
#' @param annual_cost Numeric scalar. Annual costs (taxes, management). Default 0.
#' @param discount_rate Numeric scalar. Real discount rate.
#' @param age_range Numeric vector of length 2. Search range for optimal age.
#'   Default: range of ages in the yield table.
#' @param criterion Character. "lev" (default), "npv", or "mai".
#'
#' @return A list with:
#'   \describe{
#'     \item{optimal_age}{Optimal rotation age.}
#'     \item{value_at_optimum}{LEV, NPV, or MAI at the optimum.}
#'     \item{criterion}{Criterion used.}
#'     \item{revenue_at_optimum}{Total revenue (sum across products) at optimal age.}
#'     \item{product_detail}{Data.frame showing each product's volume and value at optimal age.}
#'   }
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
#' ages <- seq(10, 80, by = 10)
#' yt <- yield_table(
#'   ages = ages,
#'   products = list(
#'     sawlog = data.frame(volume = c(0, 0, 2, 5, 10, 16, 20, 22), price = 254),
#'     pulp = data.frame(volume = c(5, 12, 18, 22, 24, 25, 25, 24), price = 9)
#'   ),
#'   product_units = c(sawlog = "mbf", pulp = "ton")
#' )
#'
#' result <- optimal_rotation_mp(yt, regen_cost = 750, annual_cost = 50,
#'                                discount_rate = 0.06)
#' result
#'
#' @seealso \code{\link{yield_table}}, \code{\link{optimal_rotation}},
#'   \code{\link{rotation_comparison_mp}}
#'
#' @export
optimal_rotation_mp <- function(yield_tbl, regen_cost, annual_cost = 0,
                                 discount_rate,
                                 age_range = NULL,
                                 criterion = c("lev", "npv", "mai")) {
  if (!inherits(yield_tbl, "yield_table")) {
    stop("'yield_tbl' must be a yield_table object from yield_table()", call. = FALSE)
  }
  if (length(yield_tbl$value_fns) == 0) {
    stop("yield_table has no prices. Supply prices in the products data.frames.",
         call. = FALSE)
  }

  criterion <- match.arg(criterion)

  if (is.null(age_range)) {
    age_range <- range(yield_tbl$ages)
  }

  r <- discount_rate
  total_value_fn <- yield_tbl$total_value_fn

  objective <- function(age) {
    revenue <- total_value_fn(age)

    if (criterion == "mai") {
      # Total volume across products / age
      total_vol <- 0
      for (vfn in yield_tbl$product_fns) {
        v <- vfn(age)
        if (v > 0) total_vol <- total_vol + v
      }
      return(total_vol / age)

    } else if (criterion == "npv") {
      pv_revenue <- revenue / (1 + r)^age
      pv_regen <- regen_cost
      if (annual_cost > 0 && r > 0) {
        pv_annual <- annual_cost * ((1 + r)^age - 1) / (r * (1 + r)^age)
      } else {
        pv_annual <- annual_cost * age
      }
      return(pv_revenue - pv_regen - pv_annual)

    } else {
      # LEV
      pv_revenue <- revenue / (1 + r)^age
      pv_regen <- regen_cost
      if (annual_cost > 0 && r > 0) {
        pv_annual <- annual_cost * ((1 + r)^age - 1) / (r * (1 + r)^age)
      } else {
        pv_annual <- annual_cost * age
      }
      rot_npv <- pv_revenue - pv_regen - pv_annual
      return(rot_npv * (1 + r)^age / ((1 + r)^age - 1))
    }
  }

  result <- stats::optimize(objective, interval = age_range, maximum = TRUE)
  opt_age <- result$maximum

  # Product detail at optimal age
  detail <- data.frame(
    product = yield_tbl$product_names,
    volume = NA_real_,
    value = NA_real_,
    stringsAsFactors = FALSE
  )
  for (i in seq_along(yield_tbl$product_names)) {
    pname <- yield_tbl$product_names[i]
    vol <- yield_tbl$product_fns[[pname]](opt_age)
    vol <- max(vol, 0)
    detail$volume[i] <- vol
    if (pname %in% names(yield_tbl$value_fns)) {
      detail$value[i] <- yield_tbl$value_fns[[pname]](opt_age)
    }
  }

  list(
    optimal_age = opt_age,
    value_at_optimum = result$objective,
    criterion = criterion,
    revenue_at_optimum = total_value_fn(opt_age),
    product_detail = detail
  )
}


#' Compare Rotation Ages for Multi-Product Yield Tables
#'
#' Tabulates NPV, LEV, and product-level detail across a range of ages.
#'
#' @param yield_tbl A \code{yield_table} object.
#' @param regen_cost Numeric scalar. Regeneration cost.
#' @param annual_cost Numeric scalar. Annual costs. Default 0.
#' @param discount_rate Numeric scalar. Discount rate.
#' @param ages Numeric vector. Ages to evaluate.
#'
#' @return A data.frame with columns for age, total revenue, NPV, LEV,
#'   and per-product volumes and values.
#'
#' @examples
#' ages <- seq(10, 80, by = 10)
#' yt <- yield_table(
#'   ages = ages,
#'   products = list(
#'     sawlog = data.frame(volume = c(0, 0, 2, 5, 10, 16, 20, 22), price = 254),
#'     pulp = data.frame(volume = c(5, 12, 18, 22, 24, 25, 25, 24), price = 9)
#'   )
#' )
#' comp <- rotation_comparison_mp(yt, 750, 50, 0.06)
#' head(comp)
#'
#' @export
rotation_comparison_mp <- function(yield_tbl, regen_cost, annual_cost = 0,
                                    discount_rate, ages = NULL) {
  if (!inherits(yield_tbl, "yield_table")) {
    stop("'yield_tbl' must be a yield_table object", call. = FALSE)
  }

  if (is.null(ages)) {
    ages <- seq(min(yield_tbl$ages), max(yield_tbl$ages), by = 1)
  }

  r <- discount_rate
  pnames <- yield_tbl$product_names

  # Base columns
  results <- data.frame(age = ages, total_revenue = NA_real_,
                         npv = NA_real_, lev = NA_real_)

  # Product columns
  for (pn in pnames) {
    results[[paste0(pn, "_vol")]] <- NA_real_
    results[[paste0(pn, "_val")]] <- NA_real_
  }

  for (i in seq_along(ages)) {
    age <- ages[i]
    rev <- yield_tbl$total_value_fn(age)
    results$total_revenue[i] <- rev

    # Product detail
    for (pn in pnames) {
      vol <- yield_tbl$product_fns[[pn]](age)
      vol <- max(vol, 0)
      results[[paste0(pn, "_vol")]][i] <- vol
      if (pn %in% names(yield_tbl$value_fns)) {
        val <- yield_tbl$value_fns[[pn]](age)
        results[[paste0(pn, "_val")]][i] <- max(val, 0)
      }
    }

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
    results$lev[i] <- rot_npv * (1 + r)^age / ((1 + r)^age - 1)
  }

  results
}
