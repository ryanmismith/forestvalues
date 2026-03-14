#' Estimate Stumpage Value
#'
#' Calculates the total stumpage value for timber given species, volume,
#' product class, and a price table. If no price table is supplied, uses
#' default 2019 Maine statewide average stumpage prices.
#'
#' Stumpage is the price paid to the landowner for standing timber, as
#' distinguished from delivered log prices. Stumpage prices vary by species,
#' product class (sawlog, pulpwood, veneer), region, and market conditions.
#'
#' @param species Character vector. Species codes (FVS codes, e.g., "SM" for
#'   sugar maple, "WP" for white pine). See \code{maine_stumpage_2019} for
#'   supported codes.
#' @param volume Numeric vector. Volume of timber. Same length as \code{species}.
#' @param product_class Character vector. Product class for each entry:
#'   "sawlog" or "pulp". Default "sawlog".
#' @param price_table A data.frame with columns \code{species}, \code{product_class},
#'   \code{price}, and \code{unit}. If \code{NULL} (default), uses built-in
#'   2019 Maine stumpage prices with a message.
#' @param unit Character. Volume unit: "mbf" (thousand board feet, default for
#'   sawlog) or "ton" (for pulpwood).
#'
#' @return A data.frame with columns: \code{species}, \code{volume},
#'   \code{product_class}, \code{unit_price}, \code{total_value}.
#'
#' @references
#' Wagner, J.E. (2012). *Forestry Economics: A Managerial Approach*. Routledge. Ch. 5.
#'
#' Maine Forest Service (2019). Stumpage price report.
#'
#' @examples
#' # Single species
#' stumpage_value("SM", 10, "sawlog")
#'
#' # Multiple species with custom prices
#' my_prices <- create_price_table(
#'   species = c("SM", "SM", "RO", "RO"),
#'   product_class = c("sawlog", "pulp", "sawlog", "pulp"),
#'   price = c(300, 10, 350, 12),
#'   unit = c("mbf", "ton", "mbf", "ton")
#' )
#' stumpage_value(c("SM", "RO"), c(15, 20), "sawlog", price_table = my_prices)
#'
#' @seealso \code{\link{create_price_table}}, \code{\link{update_prices}}
#'
#' @family stumpage
#' @export
stumpage_value <- function(species, volume, product_class = "sawlog",
                            price_table = NULL, unit = "mbf") {
  # Input validation
  if (!is.character(species)) stop("'species' must be character", call. = FALSE)
  if (!is.numeric(volume)) stop("'volume' must be numeric", call. = FALSE)
  if (length(species) != length(volume)) {
    stop("'species' and 'volume' must have the same length", call. = FALSE)
  }

  # Recycle product_class if needed
  product_class <- rep_len(product_class, length(species))

  valid_pc <- c("sawlog", "pulp")
  if (!all(product_class %in% valid_pc)) {
    stop("'product_class' must be 'sawlog' or 'pulp'", call. = FALSE)
  }

  # Use default prices if none supplied
  if (is.null(price_table)) {
    message("Using default 2019 Maine statewide stumpage prices. ",
            "Supply a 'price_table' for current or regional prices.")
    price_table <- default_price_table()
  }

  # Validate price table
  required_cols <- c("species", "product_class", "price", "unit")
  missing_cols <- setdiff(required_cols, names(price_table))
  if (length(missing_cols) > 0) {
    stop("price_table missing columns: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  # Look up prices
  results <- data.frame(
    species = species,
    volume = volume,
    product_class = product_class,
    unit_price = NA_real_,
    total_value = NA_real_,
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(results))) {
    match_rows <- price_table$species == species[i] &
      price_table$product_class == product_class[i]
    if (any(match_rows)) {
      price <- price_table$price[which(match_rows)[1]]
      if (price == 0) {
        warning("Price for species '", species[i], "' (product class: '",
                product_class[i], "') is $0 in the price table. ",
                "This may indicate missing market data. Verify this is intentional.",
                call. = FALSE)
      }
      results$unit_price[i] <- price
      # Unit conversion is handled by the price table: sawlog prices are $/MBF,
      # pulp prices are $/ton. Volume must match the price table's unit.
      results$total_value[i] <- volume[i] * price
    } else {
      warning("No price found for species '", species[i], "' (product class: '",
              product_class[i], "'). Check species code — e.g., 'SM' for sugar maple, ",
              "'RO' for red oak. See default_price_table() for supported codes. ",
              "Setting value to $0.", call. = FALSE)
      results$unit_price[i] <- 0
      results$total_value[i] <- 0
    }
  }

  results
}


#' Create a Stumpage Price Table
#'
#' Helper function to build a properly formatted price table for use with
#' \code{\link{stumpage_value}}.
#'
#' @param species Character vector. Species codes.
#' @param product_class Character vector. "sawlog" or "pulp".
#' @param price Numeric vector. Price per unit.
#' @param unit Character vector. Unit of measure ("mbf", "ton", "cord", "m3").
#'
#' @return A data.frame suitable for the \code{price_table} argument of
#'   \code{\link{stumpage_value}}.
#'
#' @examples
#' prices <- create_price_table(
#'   species = c("SM", "RO", "WP"),
#'   product_class = c("sawlog", "sawlog", "sawlog"),
#'   price = c(254, 265, 175),
#'   unit = c("mbf", "mbf", "mbf")
#' )
#'
#' @family stumpage
#' @export
create_price_table <- function(species, product_class, price, unit = "mbf") {
  if (!is.character(species)) stop("'species' must be character", call. = FALSE)
  if (!is.numeric(price)) stop("'price' must be numeric", call. = FALSE)
  if (any(price < 0)) warning("Negative prices detected", call. = FALSE)

  n <- length(species)
  product_class <- rep_len(product_class, n)
  unit <- rep_len(unit, n)

  data.frame(
    species = species,
    product_class = product_class,
    price = price,
    unit = unit,
    stringsAsFactors = FALSE
  )
}


#' Update Stumpage Prices for Inflation
#'
#' Adjusts a price table for inflation using a simple compound growth formula.
#' The package does NOT fetch external economic data; the user must supply
#' the inflation rate.
#'
#' @param price_table A data.frame from \code{\link{create_price_table}} or
#'   the default table.
#' @param inflation_rate Numeric scalar. Annual inflation rate (e.g., 0.03 for 3\%).
#' @param base_year Numeric scalar. Year the prices are from.
#' @param target_year Numeric scalar. Year to adjust prices to.
#'
#' @return A data.frame with the same structure as the input, with adjusted prices.
#'
#' @examples
#' prices <- create_price_table(
#'   species = c("SM", "RO"),
#'   product_class = c("sawlog", "sawlog"),
#'   price = c(254, 265),
#'   unit = c("mbf", "mbf")
#' )
#' # Adjust 2019 prices to 2024 at 3% annual inflation
#' updated <- update_prices(prices, inflation_rate = 0.03,
#'                           base_year = 2019, target_year = 2024)
#'
#' @family stumpage
#' @export
update_prices <- function(price_table, inflation_rate, base_year, target_year) {
  if (!is.data.frame(price_table)) {
    stop("'price_table' must be a data.frame", call. = FALSE)
  }
  if (!is.numeric(inflation_rate) || length(inflation_rate) != 1) {
    stop("'inflation_rate' must be a single numeric value as a decimal (e.g., 0.03 for 3%)",
         call. = FALSE)
  }

  years <- target_year - base_year
  price_table$price <- price_table$price * (1 + inflation_rate)^years
  price_table
}


#' Default 2019 Maine Stumpage Price Table
#'
#' Internal function returning the built-in price data extracted from the
#' original ValueEstimate function. Prices are 2019 Maine statewide averages.
#'
#' @return A data.frame with columns: species, product_class, price, unit.
#'
#' @keywords internal
default_price_table <- function() {
  # Extracted from the original ValueEstimate.R species lookup table

  # Source: Maine Forest Service 2019 Stumpage Price Report
  # Sawlog prices: $/MBF (thousand board feet). Volume input should be in MBF.
  # Pulp prices: $/ton. Volume input should be in tons.

  sawlog <- data.frame(
    species = c("AB", "AE", "AS", "BA", "BC", "BF", "BO", "BP", "BS", "BT",
                "BW", "EC", "EH", "GA", "GB", "HH", "JP", "NS", "OC", "OH",
                "OS", "PB", "PC", "PR", "QA", "RB", "RM", "RP", "RN", "RO",
                "RS", "SB", "SH", "SM", "ST", "TA", "WA", "WC", "WO", "WP",
                "WS", "YB"),
    product_class = "sawlog",
    price = c(133, 0, 197, 197, 0, 128, 265, 98, 128, 98,
              0, 0, 67, 197, 0, 0, 0, 128, 104, 0,
              0, 158, 0, 0, 98, 0, 157, 72, 72, 265,
              128, 198, 0, 254, 0, 0, 197, 104, 135, 175,
              128, 198),
    unit = "mbf",
    stringsAsFactors = FALSE
  )

  pulp <- data.frame(
    species = c("AB", "AE", "AS", "BA", "BC", "BF", "BO", "BP", "BS", "BT",
                "BW", "EC", "EH", "GA", "GB", "HH", "JP", "NS", "OC", "OH",
                "OS", "PB", "PC", "PR", "QA", "RB", "RM", "RP", "RN", "RO",
                "RS", "SB", "SH", "SM", "ST", "TA", "WA", "WC", "WO", "WP",
                "WS", "YB"),
    product_class = "pulp",
    price = c(5, 0, 9, 0, 9, 5, 9, 11, 5, 11,
              9, 9, 5, 9, 9, 9, 0, 5, 15, 9,
              0, 5, 9, 9, 11, 9, 9, 4, 4, 9,
              5, 9, 9, 9, 9, 0, 9, 15, 9, 3,
              5, 9),
    unit = "ton",
    stringsAsFactors = FALSE
  )

  rbind(sawlog, pulp)
}
