#' Export Forest Economics Analysis to Excel
#'
#' Creates a formatted Excel workbook with live formulas, locked result cells,
#' and unlocked parameter cells that coworkers can edit. Designed to accompany
#' reports for people who are comfortable with Excel but not R.
#'
#' The workbook contains up to 10 sheets depending on what inputs are provided:
#' \itemize{
#'   \item **Parameters** — editable discount rate, costs, prices (yellow cells).
#'     All other sheets reference these cells with live formulas.
#'   \item **Yield Table** — multi-product volumes (editable) with formula-computed
#'     values referencing Parameters prices.
#'   \item **Rotation Analysis** — NPV and LEV at each age with full formula chain.
#'   \item **Cash Flow** — year-by-year discounted cash flows with cumulative NPV.
#'   \item **Financial Metrics** — IRR (Excel's native IRR), benefit-cost ratio,
#'     payback period, equivalent annual annuity — all as live formulas.
#'   \item **Sensitivity Analysis** — two-way table of LEV across discount rate x
#'     stumpage price with conditional formatting (color scale).
#'   \item **Break-Even** — break-even stumpage price, volume, and max cost at
#'     each rotation age via Excel Goal Seek-ready formulas.
#'   \item **Scenario Comparison** — side-by-side base vs. alternative scenarios
#'     with editable parameters and formula-computed results.
#'   \item **Charts** — embedded ggplot2 visualizations (LEV curve, product mix,
#'     cash flow timeline, sensitivity tornado).
#'   \item **Summary** — key results and parameter links.
#' }
#'
#' @param yield_tbl A \code{yield_table} object from \code{\link{yield_table}},
#'   or \code{NULL} if providing a simple schedule.
#' @param schedule A data.frame of management activities (see
#'   \code{\link{cash_flow_schedule}}), or \code{NULL} if only doing rotation
#'   analysis.
#' @param discount_rate Numeric. Default discount rate for the Parameters sheet.
#' @param regen_cost Numeric. Default regeneration cost. Default 0.
#' @param annual_cost Numeric. Default annual cost (taxes, etc.). Default 0.
#' @param time_horizon Numeric. Default analysis horizon.
#' @param file Character. Output file path (must end in .xlsx).
#' @param title Character. Title for the workbook header.
#' @param author Character. Author name for the header. Default "".
#' @param sensitivity_rates Numeric vector. Discount rates for the sensitivity
#'   table. Default \code{seq(0.02, 0.12, by = 0.01)}.
#' @param sensitivity_prices Numeric vector or \code{NULL}. Price multipliers
#'   for the sensitivity table columns (e.g., \code{seq(0.5, 1.5, by = 0.1)}).
#'   Default generates 11 steps from 50\% to 150\% of base price.
#' @param include_charts Logical. Whether to embed ggplot2 charts. Default TRUE.
#'   Requires an active graphics device.
#'
#' @return Invisibly returns the file path. The workbook is written to disk.
#'
#' @details
#' Requires the \code{openxlsx} package (\code{install.packages("openxlsx")}).
#'
#' **Architecture:** The Parameters sheet is the single source of truth.
#' Every formula on every other sheet references Parameters cells, so changing
#' one value (e.g., discount rate) cascades through rotation analysis, cash flows,
#' financial metrics, and break-even calculations automatically.
#'
#' **Cell conventions:**
#' \itemize{
#'   \item Yellow cells = editable (unlocked)
#'   \item Green headers = data tables
#'   \item Blue headers = computed results
#'   \item All formula/result cells are locked via worksheet protection
#' }
#'
#' @references
#' Klemperer, W.D. (1996). *Forest Resource Economics and Finance*. McGraw-Hill.
#'
#' Bullard, S.H. & Straka, T.J. (2011). *Basic Concepts in Forest Valuation
#' and Investment Analysis*. 3rd ed.
#'
#' @examples
#' \dontrun{
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
#' activities <- data.frame(
#'   name = c("Planting", "Tax", "Thinning", "Harvest"),
#'   amount = c(-750, -50, 2500, 8500),
#'   year = c(0, 0, 25, 50),
#'   frequency = c("once", "annual", "once", "once"),
#'   period_length = c(NA, NA, NA, NA)
#' )
#'
#' export_excel(yt, activities, discount_rate = 0.06, regen_cost = 750,
#'              annual_cost = 50, time_horizon = 80,
#'              file = "full_analysis.xlsx",
#'              title = "Township 7 Range 10 - Stand Analysis",
#'              author = "Ryan Smith")
#' }
#'
#' @seealso \code{\link{yield_table}}, \code{\link{cash_flow_schedule}},
#'   \code{\link{optimal_rotation_mp}}
#'
#' @export
export_excel <- function(yield_tbl = NULL, schedule = NULL,
                          discount_rate = 0.06, regen_cost = 0,
                          annual_cost = 0, time_horizon = 80,
                          file = "forest_analysis.xlsx",
                          title = "Forest Economics Analysis",
                          author = "",
                          sensitivity_rates = seq(0.02, 0.12, by = 0.01),
                          sensitivity_prices = NULL,
                          include_charts = TRUE) {

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required for Excel export. ",
         "Install it with: install.packages('openxlsx')", call. = FALSE)
  }
  if (!grepl("\\.xlsx$", file)) {
    stop("'file' must end in .xlsx", call. = FALSE)
  }

  has_yield <- !is.null(yield_tbl) && inherits(yield_tbl, "yield_table")
  has_schedule <- !is.null(schedule) && is.data.frame(schedule)

  wb <- openxlsx::createWorkbook()
  s <- .excel_styles()

  # Track yield table layout info for cross-sheet references
  yt_info <- list()

  # ===== 1. PARAMETERS SHEET =====
  .build_parameters_sheet(wb, s, title, author, discount_rate, regen_cost,
                           annual_cost, time_horizon, yield_tbl, has_yield)

  # ===== 2. YIELD TABLE SHEET =====
  if (has_yield) {
    yt_info <- .build_yield_table_sheet(wb, s, yield_tbl)
  }

  # ===== 3. ROTATION ANALYSIS SHEET =====
  if (has_yield) {
    .build_rotation_sheet(wb, s, yield_tbl, yt_info)
  }

  # ===== 4. CASH FLOW SHEET =====
  if (has_schedule) {
    .build_cash_flow_sheet(wb, s, schedule, time_horizon, discount_rate)
  }

  # ===== 5. FINANCIAL METRICS SHEET =====
  if (has_schedule) {
    .build_financial_metrics_sheet(wb, s, schedule, time_horizon,
                                    discount_rate, regen_cost)
  }

  # ===== 6. SENSITIVITY ANALYSIS SHEET =====
  if (has_yield) {
    .build_sensitivity_sheet(wb, s, yield_tbl, discount_rate, regen_cost,
                              annual_cost, sensitivity_rates, sensitivity_prices)
  }

  # ===== 7. BREAK-EVEN ANALYSIS SHEET =====
  if (has_yield) {
    .build_breakeven_sheet(wb, s, yield_tbl, discount_rate, regen_cost,
                            annual_cost)
  }

  # ===== 8. SCENARIO COMPARISON SHEET =====
  if (has_yield) {
    .build_scenario_sheet(wb, s, yield_tbl, discount_rate, regen_cost,
                           annual_cost)
  }

  # ===== 9. CHARTS SHEET =====
  if (include_charts && (has_yield || has_schedule)) {
    .build_charts_sheet(wb, s, yield_tbl, schedule, discount_rate,
                         regen_cost, annual_cost, time_horizon,
                         has_yield, has_schedule)
  }

  # ===== 10. SUMMARY SHEET =====
  .build_summary_sheet(wb, s, title, author, yield_tbl, schedule,
                        discount_rate, regen_cost, annual_cost, time_horizon,
                        has_yield, has_schedule)

  # ===== SAVE =====
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  message("Workbook saved to: ", file)
  message("Yellow cells are editable. All other cells are formula-locked.")

  invisible(file)
}


# =============================================================================
# SHEET 1: PARAMETERS
# =============================================================================
.build_parameters_sheet <- function(wb, s, title, author, discount_rate,
                                     regen_cost, annual_cost, time_horizon,
                                     yield_tbl, has_yield) {
  sn <- "Parameters"
  openxlsx::addWorksheet(wb, sn)

  # Title block
  openxlsx::writeData(wb, sn, title, startCol = 1, startRow = 1)
  openxlsx::addStyle(wb, sn, s$header, rows = 1, cols = 1)
  if (nchar(author) > 0) {
    openxlsx::writeData(wb, sn, paste("Author:", author),
                         startCol = 1, startRow = 2)
  }
  openxlsx::writeData(wb, sn, paste("Generated:", format(Sys.Date(), "%B %d, %Y")),
                       startCol = 1, startRow = 3)

  # Section: Editable Parameters
  openxlsx::writeData(wb, sn, "Editable Parameters", startCol = 1, startRow = 5)
  openxlsx::addStyle(wb, sn, s$subheader, rows = 5, cols = 1:3)
  openxlsx::writeData(wb, sn,
    "Change the yellow cells below. All sheets update automatically.",
    startCol = 1, startRow = 6)
  openxlsx::addStyle(wb, sn, s$note, rows = 6, cols = 1)

  # Parameter cells: B8=rate, B9=regen, B10=annual, B11=horizon
  param_labels <- c("Discount Rate", "Regeneration Cost ($/acre)",
                     "Annual Cost ($/acre/yr)", "Time Horizon (years)")
  param_values <- c(discount_rate, regen_cost, annual_cost, time_horizon)
  param_rows <- 8:11

  for (i in seq_along(param_labels)) {
    openxlsx::writeData(wb, sn, param_labels[i],
                         startCol = 1, startRow = param_rows[i])
    openxlsx::addStyle(wb, sn, s$param_label,
                        rows = param_rows[i], cols = 1)
    openxlsx::writeData(wb, sn, param_values[i],
                         startCol = 2, startRow = param_rows[i])
  }
  openxlsx::addStyle(wb, sn, s$editable_pct, rows = 8, cols = 2)
  openxlsx::addStyle(wb, sn, s$editable, rows = 9, cols = 2)
  openxlsx::addStyle(wb, sn, s$editable, rows = 10, cols = 2)
  openxlsx::addStyle(wb, sn, s$editable_int, rows = 11, cols = 2)

  # Product prices
  if (has_yield) {
    openxlsx::writeData(wb, sn, "Product Prices", startCol = 1, startRow = 13)
    openxlsx::addStyle(wb, sn, s$subheader, rows = 13, cols = 1:3)

    price_row <- 14
    for (pn in yield_tbl$product_names) {
      pdata <- yield_tbl$products[[pn]]
      unit <- if (!is.null(yield_tbl$product_units) &&
                  pn %in% names(yield_tbl$product_units)) {
        yield_tbl$product_units[[pn]]
      } else "unit"

      price_val <- if ("price" %in% names(pdata)) pdata$price[1] else 0

      openxlsx::writeData(wb, sn, paste0(pn, " ($/", unit, ")"),
                           startCol = 1, startRow = price_row)
      openxlsx::addStyle(wb, sn, s$param_label,
                          rows = price_row, cols = 1)
      openxlsx::writeData(wb, sn, price_val,
                           startCol = 2, startRow = price_row)
      openxlsx::addStyle(wb, sn, s$editable,
                          rows = price_row, cols = 2)
      price_row <- price_row + 1
    }
  }

  # Reference guide
  ref_row <- if (has_yield) 14 + length(yield_tbl$product_names) + 2 else 14
  openxlsx::writeData(wb, sn, "Cell Reference Guide", startCol = 1,
                       startRow = ref_row)
  openxlsx::addStyle(wb, sn, s$subheader, rows = ref_row, cols = 1:3)
  refs <- data.frame(
    Cell = c("B8", "B9", "B10", "B11"),
    Contains = c("Discount Rate", "Regeneration Cost", "Annual Cost",
                  "Time Horizon"),
    stringsAsFactors = FALSE
  )
  if (has_yield) {
    for (j in seq_along(yield_tbl$product_names)) {
      refs <- rbind(refs, data.frame(
        Cell = paste0("B", 13 + j),
        Contains = paste0(yield_tbl$product_names[j], " price"),
        stringsAsFactors = FALSE
      ))
    }
  }
  openxlsx::writeData(wb, sn, refs, startCol = 1, startRow = ref_row + 1,
                       headerStyle = s$col_header)

  openxlsx::setColWidths(wb, sn, cols = 1:3, widths = c(32, 18, 20))
  openxlsx::protectWorksheet(wb, sn, protect = TRUE,
                              lockFormattingCells = FALSE,
                              lockFormattingColumns = FALSE)
}


# =============================================================================
# SHEET 2: YIELD TABLE
# =============================================================================
.build_yield_table_sheet <- function(wb, s, yield_tbl) {
  sn <- "Yield Table"
  openxlsx::addWorksheet(wb, sn)

  openxlsx::writeData(wb, sn, "Yield Table (editable volumes)",
                       startCol = 1, startRow = 1)
  openxlsx::addStyle(wb, sn, s$header, rows = 1, cols = 1)
  openxlsx::writeData(wb, sn,
    "Edit volume cells (yellow) to match your stand data. Values auto-compute from Parameters prices.",
    startCol = 1, startRow = 2)
  openxlsx::addStyle(wb, sn, s$note, rows = 2, cols = 1)

  # Headers: Age | Prod1 Vol | Prod1 Val | Prod2 Vol | ... | Total Value
  start_row <- 4
  col <- 1
  openxlsx::writeData(wb, sn, "Age", startCol = col, startRow = start_row)
  openxlsx::addStyle(wb, sn, s$col_header, rows = start_row, cols = col)

  prod_vol_cols <- list()
  prod_val_cols <- list()

  for (pn in yield_tbl$product_names) {
    col <- col + 1
    openxlsx::writeData(wb, sn, paste0(pn, " Volume"),
                         startCol = col, startRow = start_row)
    openxlsx::addStyle(wb, sn, s$col_header, rows = start_row, cols = col)
    prod_vol_cols[[pn]] <- col

    col <- col + 1
    openxlsx::writeData(wb, sn, paste0(pn, " Value ($)"),
                         startCol = col, startRow = start_row)
    openxlsx::addStyle(wb, sn, s$col_header_blue, rows = start_row, cols = col)
    prod_val_cols[[pn]] <- col
  }

  col <- col + 1
  openxlsx::writeData(wb, sn, "Total Value ($)",
                       startCol = col, startRow = start_row)
  openxlsx::addStyle(wb, sn, s$col_header_blue, rows = start_row, cols = col)
  total_val_col <- col

  # Percent of total column
  col <- col + 1
  pct_col <- col
  for (pn in yield_tbl$product_names) {
    openxlsx::writeData(wb, sn, paste0(pn, " %"),
                         startCol = col, startRow = start_row)
    openxlsx::addStyle(wb, sn, s$col_header_orange, rows = start_row, cols = col)
    col <- col + 1
  }

  # Data rows
  price_start_row <- 14  # product prices on Parameters sheet
  for (i in seq_along(yield_tbl$ages)) {
    data_row <- start_row + i
    age <- yield_tbl$ages[i]

    openxlsx::writeData(wb, sn, age, startCol = 1, startRow = data_row)

    sum_parts <- c()
    for (j in seq_along(yield_tbl$product_names)) {
      pn <- yield_tbl$product_names[j]
      pdata <- yield_tbl$products[[pn]]
      vol_col <- prod_vol_cols[[pn]]
      val_col <- prod_val_cols[[pn]]

      # Volume: editable yellow cell
      openxlsx::writeData(wb, sn, pdata$volume[i],
                           startCol = vol_col, startRow = data_row)
      openxlsx::addStyle(wb, sn, s$editable,
                          rows = data_row, cols = vol_col)

      # Value: formula = volume * price from Parameters
      vol_cell <- paste0(openxlsx::int2col(vol_col), data_row)
      price_cell <- paste0("Parameters!$B$", price_start_row + j - 1)
      formula <- paste0(vol_cell, "*", price_cell)
      openxlsx::writeFormula(wb, sn, formula,
                              startCol = val_col, startRow = data_row)
      openxlsx::addStyle(wb, sn, s$result_currency,
                          rows = data_row, cols = val_col)

      sum_parts <- c(sum_parts, paste0(openxlsx::int2col(val_col), data_row))
    }

    # Total value: sum of product values
    total_formula <- paste(sum_parts, collapse = "+")
    openxlsx::writeFormula(wb, sn, total_formula,
                            startCol = total_val_col, startRow = data_row)
    openxlsx::addStyle(wb, sn, s$result_currency,
                        rows = data_row, cols = total_val_col)

    # Product percentage columns
    for (j in seq_along(yield_tbl$product_names)) {
      pn <- yield_tbl$product_names[j]
      val_col <- prod_val_cols[[pn]]
      pct_c <- pct_col + j - 1
      val_ref <- paste0(openxlsx::int2col(val_col), data_row)
      total_ref <- paste0(openxlsx::int2col(total_val_col), data_row)
      # Guard against division by zero
      pct_f <- paste0("IF(", total_ref, "=0,0,", val_ref, "/", total_ref, ")")
      openxlsx::writeFormula(wb, sn, pct_f,
                              startCol = pct_c, startRow = data_row)
      openxlsx::addStyle(wb, sn, s$result_pct,
                          rows = data_row, cols = pct_c)
    }
  }

  n_cols <- pct_col + length(yield_tbl$product_names) - 1
  openxlsx::setColWidths(wb, sn, cols = 1:n_cols,
                          widths = c(8, rep(14, n_cols - 1)))
  openxlsx::protectWorksheet(wb, sn, protect = TRUE,
                              lockFormattingCells = FALSE)

  # Return layout info for other sheets
  list(
    start_row = start_row,
    total_val_col = total_val_col,
    prod_vol_cols = prod_vol_cols,
    prod_val_cols = prod_val_cols,
    data_start_row = start_row + 1,
    n_data_rows = length(yield_tbl$ages)
  )
}


# =============================================================================
# SHEET 3: ROTATION ANALYSIS
# =============================================================================
.build_rotation_sheet <- function(wb, s, yield_tbl, yt_info) {
  sn <- "Rotation Analysis"
  openxlsx::addWorksheet(wb, sn)

  openxlsx::writeData(wb, sn, "Rotation Age Analysis", startCol = 1, startRow = 1)
  openxlsx::addStyle(wb, sn, s$header, rows = 1, cols = 1)
  openxlsx::writeData(wb, sn,
    "All values computed via Excel formulas referencing Parameters sheet.",
    startCol = 1, startRow = 2)
  openxlsx::addStyle(wb, sn, s$note, rows = 2, cols = 1)

  # Headers
  ra_headers <- c("Age", "Total Revenue", "PV Revenue", "PV Annual Cost",
                   "Rotation NPV", "LEV", "NPV Rank", "LEV Rank")
  ra_start <- 4
  for (hc in seq_along(ra_headers)) {
    openxlsx::writeData(wb, sn, ra_headers[hc],
                         startCol = hc, startRow = ra_start)
    header_sty <- if (hc <= 1) s$col_header else s$col_header_blue
    openxlsx::addStyle(wb, sn, header_sty, rows = ra_start, cols = hc)
  }

  # Cell references to Parameters
  rate_ref <- "Parameters!$B$8"
  regen_ref <- "Parameters!$B$9"
  annual_ref <- "Parameters!$B$10"

  # One row per age
  n_ages <- length(yield_tbl$ages)
  for (i in seq_along(yield_tbl$ages)) {
    row <- ra_start + i
    age <- yield_tbl$ages[i]

    # A: Age
    openxlsx::writeData(wb, sn, age, startCol = 1, startRow = row)

    # B: Total Revenue = from Yield Table total value column
    tv_col_letter <- openxlsx::int2col(yt_info$total_val_col)
    rev_ref <- paste0("'Yield Table'!", tv_col_letter,
                       yt_info$data_start_row + i - 1)
    openxlsx::writeFormula(wb, sn, rev_ref, startCol = 2, startRow = row)
    openxlsx::addStyle(wb, sn, s$result_currency, rows = row, cols = 2)

    # C: PV Revenue = revenue / (1+r)^age
    pv_rev_f <- paste0("B", row, "/(1+", rate_ref, ")^A", row)
    openxlsx::writeFormula(wb, sn, pv_rev_f, startCol = 3, startRow = row)
    openxlsx::addStyle(wb, sn, s$result_currency, rows = row, cols = 3)

    # D: PV Annual Cost = annual * ((1+r)^n - 1) / (r*(1+r)^n)
    pv_annual_f <- paste0("IF(", annual_ref, "=0,0,",
                           annual_ref, "*((1+", rate_ref, ")^A", row,
                           "-1)/(", rate_ref, "*(1+", rate_ref, ")^A", row, "))")
    openxlsx::writeFormula(wb, sn, pv_annual_f, startCol = 4, startRow = row)
    openxlsx::addStyle(wb, sn, s$result_currency, rows = row, cols = 4)

    # E: Rotation NPV = PV Revenue - Regen - PV Annual
    npv_f <- paste0("C", row, "-", regen_ref, "-D", row)
    openxlsx::writeFormula(wb, sn, npv_f, startCol = 5, startRow = row)
    openxlsx::addStyle(wb, sn, s$result_currency, rows = row, cols = 5)

    # F: LEV = NPV * (1+r)^T / ((1+r)^T - 1)
    lev_f <- paste0("E", row, "*(1+", rate_ref, ")^A", row,
                     "/((1+", rate_ref, ")^A", row, "-1)")
    openxlsx::writeFormula(wb, sn, lev_f, startCol = 6, startRow = row)
    openxlsx::addStyle(wb, sn, s$result_currency, rows = row, cols = 6)

    # G: NPV Rank (1 = best)
    first_data <- ra_start + 1
    last_data <- ra_start + n_ages
    npv_range <- paste0("$E$", first_data, ":$E$", last_data)
    rank_npv_f <- paste0("RANK(E", row, ",", npv_range, ")")
    openxlsx::writeFormula(wb, sn, rank_npv_f, startCol = 7, startRow = row)

    # H: LEV Rank (1 = best)
    lev_range <- paste0("$F$", first_data, ":$F$", last_data)
    rank_lev_f <- paste0("RANK(F", row, ",", lev_range, ")")
    openxlsx::writeFormula(wb, sn, rank_lev_f, startCol = 8, startRow = row)
  }

  # Conditional formatting: color scale on LEV column (green=high, red=low)
  first_row <- ra_start + 1
  last_row <- ra_start + n_ages
  openxlsx::conditionalFormatting(
    wb, sn, cols = 6, rows = first_row:last_row,
    style = c("#F8696B", "#FFEB84", "#63BE7B"),
    type = "colourScale"
  )

  # Conditional formatting: color scale on NPV column
  openxlsx::conditionalFormatting(
    wb, sn, cols = 5, rows = first_row:last_row,
    style = c("#F8696B", "#FFEB84", "#63BE7B"),
    type = "colourScale"
  )

  # Highlight the best LEV row with icon sets on rank column
  openxlsx::conditionalFormatting(
    wb, sn, cols = 8, rows = first_row:last_row,
    type = "dataBar", style = c("#63BE7B")
  )

  # Summary row at bottom
  sum_row <- last_row + 2
  openxlsx::writeData(wb, sn, "Best Rotation (Max LEV):",
                       startCol = 1, startRow = sum_row)
  openxlsx::addStyle(wb, sn, s$bold, rows = sum_row, cols = 1)

  # Formula to find age with highest LEV using INDEX/MATCH
  age_range <- paste0("$A$", first_row, ":$A$", last_row)
  lev_range <- paste0("$F$", first_row, ":$F$", last_row)
  best_age_f <- paste0("INDEX(", age_range, ",MATCH(MAX(", lev_range, "),",
                        lev_range, ",0))")
  openxlsx::writeFormula(wb, sn, best_age_f, startCol = 2, startRow = sum_row)

  openxlsx::writeData(wb, sn, "years", startCol = 3, startRow = sum_row)

  openxlsx::writeData(wb, sn, "Max LEV:", startCol = 1, startRow = sum_row + 1)
  openxlsx::addStyle(wb, sn, s$bold, rows = sum_row + 1, cols = 1)
  max_lev_f <- paste0("MAX(", lev_range, ")")
  openxlsx::writeFormula(wb, sn, max_lev_f, startCol = 2,
                          startRow = sum_row + 1)
  openxlsx::addStyle(wb, sn, s$result_currency, rows = sum_row + 1, cols = 2)

  openxlsx::writeData(wb, sn, "Max NPV:", startCol = 1, startRow = sum_row + 2)
  openxlsx::addStyle(wb, sn, s$bold, rows = sum_row + 2, cols = 1)
  npv_range_ref <- paste0("$E$", first_row, ":$E$", last_row)
  max_npv_f <- paste0("MAX(", npv_range_ref, ")")
  openxlsx::writeFormula(wb, sn, max_npv_f, startCol = 2,
                          startRow = sum_row + 2)
  openxlsx::addStyle(wb, sn, s$result_currency, rows = sum_row + 2, cols = 2)

  openxlsx::setColWidths(wb, sn, cols = 1:8,
                          widths = c(8, 16, 16, 16, 16, 16, 10, 10))
  openxlsx::protectWorksheet(wb, sn, protect = TRUE)
}


# =============================================================================
# SHEET 4: CASH FLOW
# =============================================================================
.build_cash_flow_sheet <- function(wb, s, schedule, time_horizon, discount_rate) {
  sn <- "Cash Flow"
  openxlsx::addWorksheet(wb, sn)

  openxlsx::writeData(wb, sn, "Cash Flow Schedule", startCol = 1, startRow = 1)
  openxlsx::addStyle(wb, sn, s$header, rows = 1, cols = 1)

  # ---- Activities table (editable amounts) ----
  openxlsx::writeData(wb, sn, "Management Activities (edit amounts in yellow)",
                       startCol = 1, startRow = 3)
  openxlsx::addStyle(wb, sn, s$subheader, rows = 3, cols = 1:5)

  act_headers <- intersect(c("name", "amount", "year", "frequency", "period_length"),
                            names(schedule))
  for (hc in seq_along(act_headers)) {
    openxlsx::writeData(wb, sn, act_headers[hc],
                         startCol = hc, startRow = 4)
    openxlsx::addStyle(wb, sn, s$col_header, rows = 4, cols = hc)
  }
  for (i in seq_len(nrow(schedule))) {
    for (hc in seq_along(act_headers)) {
      val <- schedule[[act_headers[hc]]][i]
      openxlsx::writeData(wb, sn, val,
                           startCol = hc, startRow = 4 + i)
      if (act_headers[hc] == "amount") {
        openxlsx::addStyle(wb, sn, s$editable,
                            rows = 4 + i, cols = hc)
      }
    }
  }

  # ---- Expanded year-by-year cash flows ----
  cf <- cash_flow_schedule(schedule, time_horizon, discount_rate)
  cf_start_row <- 4 + nrow(schedule) + 3

  openxlsx::writeData(wb, sn, "Year-by-Year Cash Flows",
                       startCol = 1, startRow = cf_start_row - 1)
  openxlsx::addStyle(wb, sn, s$subheader,
                      rows = cf_start_row - 1, cols = 1:7)

  cf_headers <- c("Year", "Cash Flow", "Cumulative", "Discount Factor",
                   "Discounted CF", "Cumulative NPV", "% of Total NPV")
  for (hc in seq_along(cf_headers)) {
    openxlsx::writeData(wb, sn, cf_headers[hc],
                         startCol = hc, startRow = cf_start_row)
    openxlsx::addStyle(wb, sn, s$col_header_blue,
                        rows = cf_start_row, cols = hc)
  }

  rate_ref <- "Parameters!$B$8"
  last_cf_row <- cf_start_row + nrow(cf)

  for (i in seq_len(nrow(cf))) {
    row <- cf_start_row + i

    # A: Year
    openxlsx::writeData(wb, sn, cf$year[i], startCol = 1, startRow = row)

    # B: Cash flow (nominal)
    openxlsx::writeData(wb, sn, cf$cash_flow[i], startCol = 2, startRow = row)
    openxlsx::addStyle(wb, sn, s$result_currency, rows = row, cols = 2)

    # C: Cumulative nominal
    if (i == 1) {
      cum_f <- paste0("B", row)
    } else {
      cum_f <- paste0("C", row - 1, "+B", row)
    }
    openxlsx::writeFormula(wb, sn, cum_f, startCol = 3, startRow = row)
    openxlsx::addStyle(wb, sn, s$result_currency, rows = row, cols = 3)

    # D: Discount factor = 1/(1+r)^t
    df_f <- paste0("1/(1+", rate_ref, ")^A", row)
    openxlsx::writeFormula(wb, sn, df_f, startCol = 4, startRow = row)
    openxlsx::addStyle(wb, sn, s$result, rows = row, cols = 4)

    # E: Discounted = CF * discount factor
    disc_f <- paste0("B", row, "*D", row)
    openxlsx::writeFormula(wb, sn, disc_f, startCol = 5, startRow = row)
    openxlsx::addStyle(wb, sn, s$result_currency, rows = row, cols = 5)

    # F: Cumulative NPV
    if (i == 1) {
      cnpv_f <- paste0("E", row)
    } else {
      cnpv_f <- paste0("F", row - 1, "+E", row)
    }
    openxlsx::writeFormula(wb, sn, cnpv_f, startCol = 6, startRow = row)
    openxlsx::addStyle(wb, sn, s$result_currency, rows = row, cols = 6)

    # G: % of total NPV = cumulative / final cumulative
    pct_f <- paste0("IF(F$", last_cf_row, "=0,0,F", row, "/ABS(F$",
                     last_cf_row, "))")
    openxlsx::writeFormula(wb, sn, pct_f, startCol = 7, startRow = row)
    openxlsx::addStyle(wb, sn, s$result_pct, rows = row, cols = 7)
  }

  # Conditional formatting: negative cash flows in red, positive in green
  first_cf <- cf_start_row + 1
  openxlsx::conditionalFormatting(
    wb, sn, cols = 2, rows = first_cf:last_cf_row,
    rule = "<0", style = s$negative
  )
  openxlsx::conditionalFormatting(
    wb, sn, cols = 2, rows = first_cf:last_cf_row,
    rule = ">0", style = s$positive
  )

  # Data bars on cumulative NPV
  openxlsx::conditionalFormatting(
    wb, sn, cols = 6, rows = first_cf:last_cf_row,
    type = "dataBar", style = c("#27AE60")
  )

  # Summary row
  sum_row <- last_cf_row + 2
  openxlsx::writeData(wb, sn, "Total NPV:", startCol = 1, startRow = sum_row)
  openxlsx::addStyle(wb, sn, s$bold, rows = sum_row, cols = 1)
  openxlsx::writeFormula(wb, sn, paste0("F", last_cf_row),
                          startCol = 2, startRow = sum_row)
  openxlsx::addStyle(wb, sn, s$result_currency, rows = sum_row, cols = 2)

  openxlsx::writeData(wb, sn, "Total Undiscounted:",
                       startCol = 1, startRow = sum_row + 1)
  openxlsx::addStyle(wb, sn, s$bold, rows = sum_row + 1, cols = 1)
  openxlsx::writeFormula(wb, sn, paste0("C", last_cf_row),
                          startCol = 2, startRow = sum_row + 1)
  openxlsx::addStyle(wb, sn, s$result_currency, rows = sum_row + 1, cols = 2)

  # Payback period: first year where cumulative NPV >= 0
  openxlsx::writeData(wb, sn, "Payback Period (years):",
                       startCol = 1, startRow = sum_row + 2)
  openxlsx::addStyle(wb, sn, s$bold, rows = sum_row + 2, cols = 1)
  # MATCH finds first cumulative NPV >= 0
  cum_range <- paste0("F", first_cf, ":F", last_cf_row)
  age_range <- paste0("A", first_cf, ":A", last_cf_row)
  payback_f <- paste0("IFERROR(INDEX(", age_range,
                       ",MATCH(TRUE,", cum_range, ">=0,0)),\"Never\")")
  openxlsx::writeFormula(wb, sn, payback_f, startCol = 2,
                          startRow = sum_row + 2)

  openxlsx::setColWidths(wb, sn, cols = 1:7,
                          widths = c(8, 14, 14, 14, 14, 16, 12))
  openxlsx::protectWorksheet(wb, sn, protect = TRUE,
                              lockFormattingCells = FALSE)
}
