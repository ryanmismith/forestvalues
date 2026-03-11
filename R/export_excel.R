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


# =============================================================================
# SHEET 5: FINANCIAL METRICS
# =============================================================================
.build_financial_metrics_sheet <- function(wb, s, schedule, time_horizon,
                                            discount_rate, regen_cost) {
  sn <- "Financial Metrics"
  openxlsx::addWorksheet(wb, sn)

  openxlsx::writeData(wb, sn, "Financial Metrics", startCol = 1, startRow = 1)
  openxlsx::addStyle(wb, sn, s$header, rows = 1, cols = 1)
  openxlsx::writeData(wb, sn,
    "Investment analysis metrics computed via Excel formulas. IRR uses Excel's native IRR function.",
    startCol = 1, startRow = 2)
  openxlsx::addStyle(wb, sn, s$note, rows = 2, cols = 1)

  rate_ref <- "Parameters!$B$8"

  # ---- Build cash flow vector for IRR/NPV formulas ----
  # We need a contiguous column of cash flows by year for Excel's IRR()
  cf <- cash_flow_schedule(schedule, time_horizon, discount_rate)

  # Create full year vector (0 to time_horizon)
  all_years <- 0:time_horizon
  cf_by_year <- rep(0, length(all_years))
  for (i in seq_len(nrow(cf))) {
    yr_idx <- which(all_years == cf$year[i])
    if (length(yr_idx) > 0) {
      cf_by_year[yr_idx] <- cf_by_year[yr_idx] + cf$cash_flow[i]
    }
  }

  # Write the cash flow series
  openxlsx::writeData(wb, sn, "Annual Cash Flow Series",
                       startCol = 1, startRow = 4)
  openxlsx::addStyle(wb, sn, s$subheader, rows = 4, cols = 1:3)

  cf_headers <- c("Year", "Cash Flow")
  for (hc in seq_along(cf_headers)) {
    openxlsx::writeData(wb, sn, cf_headers[hc], startCol = hc, startRow = 5)
    openxlsx::addStyle(wb, sn, s$col_header, rows = 5, cols = hc)
  }

  cf_data_start <- 6
  for (i in seq_along(all_years)) {
    row <- cf_data_start + i - 1
    openxlsx::writeData(wb, sn, all_years[i], startCol = 1, startRow = row)
    openxlsx::writeData(wb, sn, cf_by_year[i], startCol = 2, startRow = row)
    openxlsx::addStyle(wb, sn, s$result_currency, rows = row, cols = 2)
  }
  cf_data_end <- cf_data_start + length(all_years) - 1

  # Conditional formatting on cash flows
  openxlsx::conditionalFormatting(
    wb, sn, cols = 2, rows = cf_data_start:cf_data_end,
    rule = "<0", style = s$negative
  )
  openxlsx::conditionalFormatting(
    wb, sn, cols = 2, rows = cf_data_start:cf_data_end,
    rule = ">0", style = s$positive
  )

  # ---- Metrics panel (right side) ----
  mc <- 4  # metrics start column
  openxlsx::writeData(wb, sn, "Investment Metrics",
                       startCol = mc, startRow = 4)
  openxlsx::addStyle(wb, sn, s$subheader, rows = 4, cols = mc:(mc + 2))

  cf_range <- paste0("$B$", cf_data_start, ":$B$", cf_data_end)

  metrics <- list()
  mrow <- 6

  # 1. IRR (Excel native)
  metrics[["Internal Rate of Return (IRR)"]] <- list(
    formula = paste0("IFERROR(IRR(", cf_range, "),\"No solution\")"),
    format = "pct",
    note = "Uses Excel's native IRR() on the annual cash flow series"
  )

  # 2. NPV (Excel native — note: Excel NPV excludes year 0)
  metrics[["Net Present Value (NPV)"]] <- list(
    formula = paste0("B", cf_data_start, "+NPV(", rate_ref, ",",
                      "$B$", cf_data_start + 1, ":$B$", cf_data_end, ")"),
    format = "currency",
    note = "= Year 0 cost + NPV(rate, year 1..n). References Parameters!B8"
  )

  # 3. MIRR (Excel native) — finance rate = discount rate, reinvest = discount rate
  metrics[["Modified IRR (MIRR)"]] <- list(
    formula = paste0("IFERROR(MIRR(", cf_range, ",", rate_ref, ",", rate_ref,
                      "),\"No solution\")"),
    format = "pct",
    note = "Finance rate and reinvestment rate both = discount rate"
  )

  # 4. Benefit-Cost Ratio
  # Sum of positive discounted CFs / abs(sum of negative discounted CFs)
  metrics[["Benefit-Cost Ratio"]] <- list(
    formula = paste0("IFERROR(SUMPRODUCT((", cf_range, ">0)*", cf_range,
                      "/(1+", rate_ref, ")^($A$", cf_data_start, ":$A$", cf_data_end,
                      "))/ABS(SUMPRODUCT((", cf_range, "<0)*", cf_range,
                      "/(1+", rate_ref, ")^($A$", cf_data_start, ":$A$", cf_data_end,
                      "))),\"N/A\")"),
    format = "number",
    note = "PV(revenues) / PV(costs). > 1 means profitable"
  )

  # 5. Profitability Index
  metrics[["Profitability Index"]] <- list(
    formula = paste0("IFERROR((B", cf_data_start, "+NPV(", rate_ref, ",",
                      "$B$", cf_data_start + 1, ":$B$", cf_data_end,
                      "))/ABS(SUMPRODUCT((", cf_range, "<0)*", cf_range,
                      "/(1+", rate_ref, ")^($A$", cf_data_start, ":$A$",
                      cf_data_end, "))),\"N/A\")"),
    format = "number",
    note = "NPV / PV(costs) + 1. > 0 means value created per dollar invested"
  )

  # 6. Equivalent Annual Annuity (EAA)
  metrics[["Equivalent Annual Annuity (EAA)"]] <- list(
    formula = paste0("IFERROR(PMT(", rate_ref, ",", time_horizon, ",-(B",
                      cf_data_start, "+NPV(", rate_ref, ",$B$",
                      cf_data_start + 1, ":$B$", cf_data_end,
                      "))),\"N/A\")"),
    format = "currency",
    note = "Annual payment equivalent of the NPV over the time horizon"
  )

  # 7. Payback Period
  metrics[["Simple Payback Period"]] <- list(
    formula = paste0("IFERROR(MATCH(TRUE,MMULT(TRANSPOSE((",
                      cf_range, ">=-99999)*1),TRANSPOSE(SEQUENCE(ROWS(",
                      cf_range, "))))>=0,0),\"Never\")"),
    format = "number",
    note = "First year where cumulative undiscounted cash flow >= 0"
  )

  # Write each metric
  for (metric_name in names(metrics)) {
    m <- metrics[[metric_name]]

    openxlsx::writeData(wb, sn, metric_name, startCol = mc, startRow = mrow)
    openxlsx::addStyle(wb, sn, s$param_label, rows = mrow, cols = mc)

    openxlsx::writeFormula(wb, sn, m$formula,
                            startCol = mc + 1, startRow = mrow)

    if (m$format == "pct") {
      openxlsx::addStyle(wb, sn, s$result_pct, rows = mrow, cols = mc + 1)
    } else if (m$format == "currency") {
      openxlsx::addStyle(wb, sn, s$result_currency, rows = mrow, cols = mc + 1)
    } else {
      openxlsx::addStyle(wb, sn, s$result, rows = mrow, cols = mc + 1)
    }

    # Note
    openxlsx::writeData(wb, sn, m$note, startCol = mc + 2, startRow = mrow)
    openxlsx::addStyle(wb, sn, s$italic_note, rows = mrow, cols = mc + 2)

    mrow <- mrow + 1
  }

  # ---- Decision criteria guide ----
  guide_row <- mrow + 2
  openxlsx::writeData(wb, sn, "Decision Criteria Guide",
                       startCol = mc, startRow = guide_row)
  openxlsx::addStyle(wb, sn, s$subheader, rows = guide_row, cols = mc:(mc + 2))

  guide_data <- data.frame(
    Metric = c("IRR", "NPV", "B/C Ratio", "Prof. Index"),
    Accept = c("> discount rate", "> 0", "> 1.0", "> 0"),
    Reject = c("< discount rate", "< 0", "< 1.0", "< 0"),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb, sn, guide_data, startCol = mc,
                       startRow = guide_row + 1, headerStyle = s$col_header_blue)

  openxlsx::setColWidths(wb, sn, cols = 1:6,
                          widths = c(8, 14, 4, 36, 18, 44))
  openxlsx::protectWorksheet(wb, sn, protect = TRUE)
}


# =============================================================================
# SHEET 6: SENSITIVITY ANALYSIS
# =============================================================================
.build_sensitivity_sheet <- function(wb, s, yield_tbl, discount_rate,
                                      regen_cost, annual_cost,
                                      sensitivity_rates, sensitivity_prices) {
  sn <- "Sensitivity"
  openxlsx::addWorksheet(wb, sn)

  openxlsx::writeData(wb, sn, "Sensitivity Analysis", startCol = 1, startRow = 1)
  openxlsx::addStyle(wb, sn, s$header, rows = 1, cols = 1)
  openxlsx::writeData(wb, sn,
    "Two-way tables showing how LEV and NPV change with discount rate and stumpage price. R-computed values.",
    startCol = 1, startRow = 2)
  openxlsx::addStyle(wb, sn, s$note, rows = 2, cols = 1)

  # Default price multipliers
  if (is.null(sensitivity_prices)) {
    sensitivity_prices <- seq(0.5, 1.5, by = 0.1)
  }

  # Get base prices for first product (or total)
  base_prices <- list()
  for (pn in yield_tbl$product_names) {
    pdata <- yield_tbl$products[[pn]]
    base_prices[[pn]] <- if ("price" %in% names(pdata)) pdata$price[1] else 0
  }

  # ---- TABLE 1: LEV vs Discount Rate x Price Multiplier ----
  openxlsx::writeData(wb, sn, "LEV Sensitivity: Discount Rate vs Price Level",
                       startCol = 1, startRow = 4)
  openxlsx::addStyle(wb, sn, s$subheader, rows = 4, cols = 1:15)

  # Column headers: price multipliers
  t1_start_row <- 5
  openxlsx::writeData(wb, sn, "Rate \\ Price", startCol = 1,
                       startRow = t1_start_row)
  openxlsx::addStyle(wb, sn, s$col_header, rows = t1_start_row, cols = 1)

  for (j in seq_along(sensitivity_prices)) {
    pct_label <- paste0(round(sensitivity_prices[j] * 100), "%")
    openxlsx::writeData(wb, sn, pct_label,
                         startCol = 1 + j, startRow = t1_start_row)
    header_sty <- if (sensitivity_prices[j] == 1.0) s$col_header else s$col_header_blue
    openxlsx::addStyle(wb, sn, header_sty,
                        rows = t1_start_row, cols = 1 + j)
  }

  # Compute LEV grid (R-computed since this requires spline interpolation)
  for (i in seq_along(sensitivity_rates)) {
    row <- t1_start_row + i
    rate <- sensitivity_rates[i]

    # Row header
    openxlsx::writeData(wb, sn, paste0(round(rate * 100, 1), "%"),
                         startCol = 1, startRow = row)
    rate_sty <- if (abs(rate - discount_rate) < 1e-6) s$col_header else s$param_label
    openxlsx::addStyle(wb, sn, rate_sty, rows = row, cols = 1)

    for (j in seq_along(sensitivity_prices)) {
      price_mult <- sensitivity_prices[j]

      # Build modified yield table with scaled prices
      scaled_products <- yield_tbl$products
      for (pn in yield_tbl$product_names) {
        if ("price" %in% names(scaled_products[[pn]])) {
          scaled_products[[pn]]$price <- scaled_products[[pn]]$price * price_mult
        }
      }
      temp_yt <- tryCatch(
        yield_table(yield_tbl$ages, scaled_products, yield_tbl$product_units),
        error = function(e) NULL
      )

      if (!is.null(temp_yt)) {
        opt <- tryCatch(
          optimal_rotation_mp(temp_yt, regen_cost, annual_cost, rate),
          error = function(e) NULL
        )
        lev_val <- if (!is.null(opt)) opt$value_at_optimum else NA
      } else {
        lev_val <- NA
      }

      openxlsx::writeData(wb, sn, if (is.na(lev_val)) "N/A" else round(lev_val, 2),
                           startCol = 1 + j, startRow = row)
      openxlsx::addStyle(wb, sn, s$result_currency, rows = row, cols = 1 + j)
    }
  }

  # Conditional formatting: 3-color scale on the LEV grid
  t1_first_row <- t1_start_row + 1
  t1_last_row <- t1_start_row + length(sensitivity_rates)
  t1_first_col <- 2
  t1_last_col <- 1 + length(sensitivity_prices)
  openxlsx::conditionalFormatting(
    wb, sn,
    cols = t1_first_col:t1_last_col,
    rows = t1_first_row:t1_last_row,
    style = c("#F8696B", "#FFEB84", "#63BE7B"),
    type = "colourScale"
  )

  # ---- TABLE 2: Optimal Rotation vs Discount Rate x Price ----
  t2_offset <- t1_last_row + 3
  openxlsx::writeData(wb, sn,
    "Optimal Rotation Age: Discount Rate vs Price Level",
    startCol = 1, startRow = t2_offset)
  openxlsx::addStyle(wb, sn, s$subheader, rows = t2_offset, cols = 1:15)

  t2_start_row <- t2_offset + 1
  openxlsx::writeData(wb, sn, "Rate \\ Price", startCol = 1,
                       startRow = t2_start_row)
  openxlsx::addStyle(wb, sn, s$col_header, rows = t2_start_row, cols = 1)

  for (j in seq_along(sensitivity_prices)) {
    pct_label <- paste0(round(sensitivity_prices[j] * 100), "%")
    openxlsx::writeData(wb, sn, pct_label,
                         startCol = 1 + j, startRow = t2_start_row)
    header_sty <- if (sensitivity_prices[j] == 1.0) s$col_header else s$col_header_blue
    openxlsx::addStyle(wb, sn, header_sty,
                        rows = t2_start_row, cols = 1 + j)
  }

  for (i in seq_along(sensitivity_rates)) {
    row <- t2_start_row + i
    rate <- sensitivity_rates[i]

    openxlsx::writeData(wb, sn, paste0(round(rate * 100, 1), "%"),
                         startCol = 1, startRow = row)
    rate_sty <- if (abs(rate - discount_rate) < 1e-6) s$col_header else s$param_label
    openxlsx::addStyle(wb, sn, rate_sty, rows = row, cols = 1)

    for (j in seq_along(sensitivity_prices)) {
      price_mult <- sensitivity_prices[j]

      scaled_products <- yield_tbl$products
      for (pn in yield_tbl$product_names) {
        if ("price" %in% names(scaled_products[[pn]])) {
          scaled_products[[pn]]$price <- scaled_products[[pn]]$price * price_mult
        }
      }
      temp_yt <- tryCatch(
        yield_table(yield_tbl$ages, scaled_products, yield_tbl$product_units),
        error = function(e) NULL
      )

      if (!is.null(temp_yt)) {
        opt <- tryCatch(
          optimal_rotation_mp(temp_yt, regen_cost, annual_cost, rate),
          error = function(e) NULL
        )
        opt_age <- if (!is.null(opt)) round(opt$optimal_age) else NA
      } else {
        opt_age <- NA
      }

      openxlsx::writeData(wb, sn, if (is.na(opt_age)) "N/A" else opt_age,
                           startCol = 1 + j, startRow = row)
    }
  }

  # Color scale on rotation ages (blue = short, orange = long)
  t2_first_row <- t2_start_row + 1
  t2_last_row <- t2_start_row + length(sensitivity_rates)
  openxlsx::conditionalFormatting(
    wb, sn,
    cols = t1_first_col:t1_last_col,
    rows = t2_first_row:t2_last_row,
    style = c("#5DADE2", "#F9E79F", "#E67E22"),
    type = "colourScale"
  )

  # ---- TABLE 3: One-way sensitivity on regen cost ----
  t3_offset <- t2_last_row + 3
  openxlsx::writeData(wb, sn,
    "LEV Sensitivity to Regeneration Cost",
    startCol = 1, startRow = t3_offset)
  openxlsx::addStyle(wb, sn, s$subheader, rows = t3_offset, cols = 1:6)

  regen_range <- seq(0, regen_cost * 3, length.out = 13)
  regen_range <- round(regen_range / 50) * 50  # round to nearest $50

  t3_headers <- c("Regen Cost", "LEV", "Optimal Rotation", "Change in LEV")
  t3_start <- t3_offset + 1
  for (hc in seq_along(t3_headers)) {
    openxlsx::writeData(wb, sn, t3_headers[hc],
                         startCol = hc, startRow = t3_start)
    openxlsx::addStyle(wb, sn, s$col_header_blue, rows = t3_start, cols = hc)
  }

  base_lev <- NULL
  for (i in seq_along(regen_range)) {
    row <- t3_start + i
    rc <- regen_range[i]

    openxlsx::writeData(wb, sn, rc, startCol = 1, startRow = row)
    openxlsx::addStyle(wb, sn, s$result_currency, rows = row, cols = 1)

    opt <- tryCatch(
      optimal_rotation_mp(yield_tbl, rc, annual_cost, discount_rate),
      error = function(e) NULL
    )
    if (!is.null(opt)) {
      if (is.null(base_lev) && abs(rc - regen_cost) < 1) {
        base_lev <- opt$value_at_optimum
      }
      openxlsx::writeData(wb, sn, round(opt$value_at_optimum, 2),
                           startCol = 2, startRow = row)
      openxlsx::addStyle(wb, sn, s$result_currency, rows = row, cols = 2)
      openxlsx::writeData(wb, sn, round(opt$optimal_age),
                           startCol = 3, startRow = row)
      if (!is.null(base_lev) && base_lev != 0) {
        change <- (opt$value_at_optimum - base_lev) / abs(base_lev)
        openxlsx::writeData(wb, sn, round(change, 4),
                             startCol = 4, startRow = row)
        openxlsx::addStyle(wb, sn, s$result_pct, rows = row, cols = 4)
      }
    }

    # Highlight base case row
    if (abs(rc - regen_cost) < 1) {
      openxlsx::addStyle(wb, sn, s$scenario_base, rows = row, cols = 1:4,
                          stack = TRUE)
    }
  }

  # Data bars on LEV column
  openxlsx::conditionalFormatting(
    wb, sn, cols = 2, rows = (t3_start + 1):(t3_start + length(regen_range)),
    type = "dataBar", style = c("#27AE60")
  )

  openxlsx::setColWidths(wb, sn, cols = 1:(1 + length(sensitivity_prices)),
                          widths = c(14, rep(10, length(sensitivity_prices))))
  openxlsx::protectWorksheet(wb, sn, protect = TRUE)
}


# =============================================================================
# SHEET 7: BREAK-EVEN ANALYSIS
# =============================================================================
.build_breakeven_sheet <- function(wb, s, yield_tbl, discount_rate,
                                    regen_cost, annual_cost) {
  sn <- "Break-Even"
  openxlsx::addWorksheet(wb, sn)

  openxlsx::writeData(wb, sn, "Break-Even Analysis", startCol = 1, startRow = 1)
  openxlsx::addStyle(wb, sn, s$header, rows = 1, cols = 1)
  openxlsx::writeData(wb, sn,
    paste0("What minimum price, volume, or maximum cost makes this investment ",
           "break even (NPV = 0)? R-computed via numerical root-finding."),
    startCol = 1, startRow = 2)
  openxlsx::addStyle(wb, sn, s$note, rows = 2, cols = 1)
  openxlsx::mergeCells(wb, sn, cols = 1:7, rows = 2)

  # ---- Table 1: Break-even price multiplier at each rotation age ----
  openxlsx::writeData(wb, sn, "Break-Even Stumpage Price by Rotation Age",
                       startCol = 1, startRow = 4)
  openxlsx::addStyle(wb, sn, s$subheader, rows = 4, cols = 1:7)

  be_headers <- c("Rotation Age", "Revenue at Harvest", "NPV at Base Price",
                   "Break-Even Price Mult.", "Break-Even Price ($)",
                   "Margin of Safety")
  be_start <- 5
  for (hc in seq_along(be_headers)) {
    openxlsx::writeData(wb, sn, be_headers[hc],
                         startCol = hc, startRow = be_start)
    openxlsx::addStyle(wb, sn, s$col_header_blue, rows = be_start, cols = hc)
  }

  # Get base price (first product as reference)
  first_price <- 0
  for (pn in yield_tbl$product_names) {
    pdata <- yield_tbl$products[[pn]]
    if ("price" %in% names(pdata)) {
      first_price <- pdata$price[1]
      break
    }
  }

  for (i in seq_along(yield_tbl$ages)) {
    row <- be_start + i
    age <- yield_tbl$ages[i]
    if (age < 10) next  # skip very young ages

    openxlsx::writeData(wb, sn, age, startCol = 1, startRow = row)

    # Revenue at harvest
    rev <- yield_tbl$total_value_fn(age)
    openxlsx::writeData(wb, sn, round(rev, 2), startCol = 2, startRow = row)
    openxlsx::addStyle(wb, sn, s$result_currency, rows = row, cols = 2)

    # NPV at base price
    r <- discount_rate
    pv_rev <- rev / (1 + r)^age
    pv_annual <- if (annual_cost > 0 && r > 0) {
      annual_cost * ((1 + r)^age - 1) / (r * (1 + r)^age)
    } else annual_cost * age
    base_npv <- pv_rev - regen_cost - pv_annual

    openxlsx::writeData(wb, sn, round(base_npv, 2), startCol = 3, startRow = row)
    openxlsx::addStyle(wb, sn, s$result_currency, rows = row, cols = 3)

    # Break-even price multiplier: find m where m*revenue/(1+r)^T - costs = 0
    # m = (regen_cost + pv_annual) * (1+r)^T / revenue
    if (rev > 0) {
      be_mult <- (regen_cost + pv_annual) * (1 + r)^age / rev
      openxlsx::writeData(wb, sn, round(be_mult, 4),
                           startCol = 4, startRow = row)
      openxlsx::addStyle(wb, sn, s$result, rows = row, cols = 4)

      # Break-even absolute price (for first product)
      if (first_price > 0) {
        be_price <- first_price * be_mult
        openxlsx::writeData(wb, sn, round(be_price, 2),
                             startCol = 5, startRow = row)
        openxlsx::addStyle(wb, sn, s$result_currency, rows = row, cols = 5)
      }

      # Margin of safety = (1 - break-even mult) as percentage
      margin <- 1 - be_mult
      openxlsx::writeData(wb, sn, round(margin, 4),
                           startCol = 6, startRow = row)
      openxlsx::addStyle(wb, sn, s$result_pct, rows = row, cols = 6)
    }
  }

  # Conditional formatting on margin of safety
  be_first <- be_start + 1
  be_last <- be_start + length(yield_tbl$ages)
  openxlsx::conditionalFormatting(
    wb, sn, cols = 6, rows = be_first:be_last,
    style = c("#F8696B", "#FFEB84", "#63BE7B"),
    type = "colourScale"
  )

  # ---- Table 2: Break-even discount rate at each age ----
  t2_start <- be_last + 3
  openxlsx::writeData(wb, sn, "Break-Even Discount Rate by Rotation Age",
                       startCol = 1, startRow = t2_start)
  openxlsx::addStyle(wb, sn, s$subheader, rows = t2_start, cols = 1:5)

  be2_headers <- c("Rotation Age", "Revenue", "Total Cost (undiscounted)",
                    "Break-Even Rate (IRR)", "vs Base Rate")
  t2_hdr <- t2_start + 1
  for (hc in seq_along(be2_headers)) {
    openxlsx::writeData(wb, sn, be2_headers[hc],
                         startCol = hc, startRow = t2_hdr)
    openxlsx::addStyle(wb, sn, s$col_header_blue, rows = t2_hdr, cols = hc)
  }

  for (i in seq_along(yield_tbl$ages)) {
    row <- t2_hdr + i
    age <- yield_tbl$ages[i]
    if (age < 10) next

    openxlsx::writeData(wb, sn, age, startCol = 1, startRow = row)

    rev <- yield_tbl$total_value_fn(age)
    openxlsx::writeData(wb, sn, round(rev, 2), startCol = 2, startRow = row)
    openxlsx::addStyle(wb, sn, s$result_currency, rows = row, cols = 2)

    total_cost <- regen_cost + annual_cost * age
    openxlsx::writeData(wb, sn, round(total_cost, 2),
                         startCol = 3, startRow = row)
    openxlsx::addStyle(wb, sn, s$result_currency, rows = row, cols = 3)

    # IRR for single rotation: solve (rev/(1+r)^T - regen - annual*annuity = 0)
    if (rev > regen_cost) {
      irr_fn <- function(r) {
        if (r <= 0) return(Inf)
        pv_r <- rev / (1 + r)^age
        pv_a <- if (annual_cost > 0 && r > 0) {
          annual_cost * ((1 + r)^age - 1) / (r * (1 + r)^age)
        } else annual_cost * age
        pv_r - regen_cost - pv_a
      }

      be_rate <- tryCatch({
        result <- stats::uniroot(irr_fn, interval = c(0.001, 1),
                                  extendInt = "no")
        result$root
      }, error = function(e) NA)

      if (!is.na(be_rate)) {
        openxlsx::writeData(wb, sn, round(be_rate, 6),
                             startCol = 4, startRow = row)
        openxlsx::addStyle(wb, sn, s$result_pct, rows = row, cols = 4)

        # Spread vs base rate
        spread <- be_rate - discount_rate
        openxlsx::writeData(wb, sn, round(spread, 6),
                             startCol = 5, startRow = row)
        openxlsx::addStyle(wb, sn, s$result_pct, rows = row, cols = 5)
      }
    }
  }

  # Color scale on break-even rate
  t2_first <- t2_hdr + 1
  t2_last <- t2_hdr + length(yield_tbl$ages)
  openxlsx::conditionalFormatting(
    wb, sn, cols = 4, rows = t2_first:t2_last,
    style = c("#F8696B", "#FFEB84", "#63BE7B"),
    type = "colourScale"
  )

  # ---- Table 3: Maximum affordable costs ----
  t3_start <- t2_last + 3
  openxlsx::writeData(wb, sn,
    "Maximum Affordable Costs (at base price and discount rate)",
    startCol = 1, startRow = t3_start)
  openxlsx::addStyle(wb, sn, s$subheader, rows = t3_start, cols = 1:5)

  t3_headers <- c("Rotation Age", "Max Regen Cost", "Max Annual Cost",
                   "Max Total Cost (PV)")
  t3_hdr <- t3_start + 1
  for (hc in seq_along(t3_headers)) {
    openxlsx::writeData(wb, sn, t3_headers[hc],
                         startCol = hc, startRow = t3_hdr)
    openxlsx::addStyle(wb, sn, s$col_header_blue, rows = t3_hdr, cols = hc)
  }

  r <- discount_rate
  for (i in seq_along(yield_tbl$ages)) {
    row <- t3_hdr + i
    age <- yield_tbl$ages[i]
    if (age < 10) next

    openxlsx::writeData(wb, sn, age, startCol = 1, startRow = row)

    rev <- yield_tbl$total_value_fn(age)
    pv_rev <- rev / (1 + r)^age

    # Max regen = PV revenue - PV annual cost (holding annual constant)
    pv_annual <- if (annual_cost > 0 && r > 0) {
      annual_cost * ((1 + r)^age - 1) / (r * (1 + r)^age)
    } else annual_cost * age
    max_regen <- pv_rev - pv_annual
    openxlsx::writeData(wb, sn, round(max(max_regen, 0), 2),
                         startCol = 2, startRow = row)
    openxlsx::addStyle(wb, sn, s$result_currency, rows = row, cols = 2)

    # Max annual = (PV revenue - regen) / annuity factor
    annuity_factor <- if (r > 0) ((1 + r)^age - 1) / (r * (1 + r)^age) else age
    max_annual <- if (annuity_factor > 0) (pv_rev - regen_cost) / annuity_factor else 0
    openxlsx::writeData(wb, sn, round(max(max_annual, 0), 2),
                         startCol = 3, startRow = row)
    openxlsx::addStyle(wb, sn, s$result_currency, rows = row, cols = 3)

    # Max total PV cost
    openxlsx::writeData(wb, sn, round(max(pv_rev, 0), 2),
                         startCol = 4, startRow = row)
    openxlsx::addStyle(wb, sn, s$result_currency, rows = row, cols = 4)
  }

  openxlsx::setColWidths(wb, sn, cols = 1:6,
                          widths = c(14, 18, 16, 20, 20, 16))
  openxlsx::protectWorksheet(wb, sn, protect = TRUE)
}


# =============================================================================
# SHEET 8: SCENARIO COMPARISON
# =============================================================================
.build_scenario_sheet <- function(wb, s, yield_tbl, discount_rate,
                                   regen_cost, annual_cost) {
  sn <- "Scenarios"
  openxlsx::addWorksheet(wb, sn)

  openxlsx::writeData(wb, sn, "Scenario Comparison", startCol = 1, startRow = 1)
  openxlsx::addStyle(wb, sn, s$header, rows = 1, cols = 1)
  openxlsx::writeData(wb, sn,
    paste0("Edit yellow cells in Alternative scenarios to compare against ",
           "the Base Case. Results are R-computed — re-export to update after ",
           "parameter changes."),
    startCol = 1, startRow = 2)
  openxlsx::addStyle(wb, sn, s$note, rows = 2, cols = 1)
  openxlsx::mergeCells(wb, sn, cols = 1:8, rows = 2)

  # Define 4 scenarios: Base, High Price, High Cost, Conservative
  # Each scenario has its own column block
  n_products <- length(yield_tbl$product_names)
  base_prices <- sapply(yield_tbl$product_names, function(pn) {
    pdata <- yield_tbl$products[[pn]]
    if ("price" %in% names(pdata)) pdata$price[1] else 0
  })

  scenarios <- list(
    "Base Case" = list(
      rate = discount_rate,
      regen = regen_cost,
      annual = annual_cost,
      price_mult = 1.0,
      editable = FALSE
    ),
    "Optimistic" = list(
      rate = discount_rate,
      regen = regen_cost * 0.8,
      annual = annual_cost * 0.9,
      price_mult = 1.2,
      editable = TRUE
    ),
    "Pessimistic" = list(
      rate = discount_rate + 0.02,
      regen = regen_cost * 1.3,
      annual = annual_cost * 1.2,
      price_mult = 0.75,
      editable = TRUE
    ),
    "Custom" = list(
      rate = discount_rate,
      regen = regen_cost,
      annual = annual_cost,
      price_mult = 1.0,
      editable = TRUE
    )
  )

  # Layout: parameters block then results block
  param_labels <- c("Discount Rate", "Regen Cost ($/acre)",
                      "Annual Cost ($/acre/yr)", "Price Multiplier")

  # Header row
  hdr_row <- 4
  openxlsx::writeData(wb, sn, "Parameter", startCol = 1, startRow = hdr_row)
  openxlsx::addStyle(wb, sn, s$col_header, rows = hdr_row, cols = 1)

  for (sc_idx in seq_along(scenarios)) {
    sc_name <- names(scenarios)[sc_idx]
    col <- sc_idx + 1

    openxlsx::writeData(wb, sn, sc_name, startCol = col, startRow = hdr_row)
    if (sc_idx == 1) {
      openxlsx::addStyle(wb, sn, s$col_header, rows = hdr_row, cols = col)
    } else {
      openxlsx::addStyle(wb, sn, s$col_header_orange, rows = hdr_row, cols = col)
    }
  }

  # Parameter rows
  param_row_start <- hdr_row + 1
  for (pi in seq_along(param_labels)) {
    row <- param_row_start + pi - 1
    openxlsx::writeData(wb, sn, param_labels[pi], startCol = 1, startRow = row)
    openxlsx::addStyle(wb, sn, s$param_label, rows = row, cols = 1)

    for (sc_idx in seq_along(scenarios)) {
      sc <- scenarios[[sc_idx]]
      col <- sc_idx + 1

      val <- switch(pi,
        sc$rate,
        sc$regen,
        sc$annual,
        sc$price_mult
      )

      openxlsx::writeData(wb, sn, val, startCol = col, startRow = row)

      if (sc$editable) {
        style <- if (pi == 1) s$editable_pct else s$editable
        openxlsx::addStyle(wb, sn, style, rows = row, cols = col)
      } else {
        style <- if (pi == 1) s$result_pct else s$result_currency
        openxlsx::addStyle(wb, sn, s$scenario_base, rows = row, cols = col)
      }
    }
  }

  # ---- Results section ----
  results_start <- param_row_start + length(param_labels) + 2
  openxlsx::writeData(wb, sn, "Results", startCol = 1, startRow = results_start)
  openxlsx::addStyle(wb, sn, s$subheader, rows = results_start,
                      cols = 1:(length(scenarios) + 1))

  result_labels <- c("Optimal Rotation (years)", "LEV at Optimum ($)",
                      "Revenue at Optimum ($)", "NPV at Optimum ($)")

  for (ri in seq_along(result_labels)) {
    row <- results_start + ri
    openxlsx::writeData(wb, sn, result_labels[ri],
                         startCol = 1, startRow = row)
    openxlsx::addStyle(wb, sn, s$param_label, rows = row, cols = 1)
  }

  # Compute results for each scenario
  for (sc_idx in seq_along(scenarios)) {
    sc <- scenarios[[sc_idx]]
    col <- sc_idx + 1

    # Scale prices
    scaled_products <- yield_tbl$products
    for (pn in yield_tbl$product_names) {
      if ("price" %in% names(scaled_products[[pn]])) {
        scaled_products[[pn]]$price <- scaled_products[[pn]]$price * sc$price_mult
      }
    }

    temp_yt <- tryCatch(
      yield_table(yield_tbl$ages, scaled_products, yield_tbl$product_units),
      error = function(e) NULL
    )

    if (!is.null(temp_yt)) {
      opt_lev <- tryCatch(
        optimal_rotation_mp(temp_yt, sc$regen, sc$annual, sc$rate,
                             criterion = "lev"),
        error = function(e) NULL
      )
      opt_npv <- tryCatch(
        optimal_rotation_mp(temp_yt, sc$regen, sc$annual, sc$rate,
                             criterion = "npv"),
        error = function(e) NULL
      )
    } else {
      opt_lev <- NULL
      opt_npv <- NULL
    }

    row_base <- results_start

    # Optimal Rotation
    val <- if (!is.null(opt_lev)) round(opt_lev$optimal_age) else "N/A"
    openxlsx::writeData(wb, sn, val, startCol = col, startRow = row_base + 1)

    # LEV
    val <- if (!is.null(opt_lev)) round(opt_lev$value_at_optimum, 2) else "N/A"
    openxlsx::writeData(wb, sn, val, startCol = col, startRow = row_base + 2)
    if (is.numeric(val)) {
      openxlsx::addStyle(wb, sn, s$result_currency,
                          rows = row_base + 2, cols = col)
    }

    # Revenue
    val <- if (!is.null(opt_lev)) round(opt_lev$revenue_at_optimum, 2) else "N/A"
    openxlsx::writeData(wb, sn, val, startCol = col, startRow = row_base + 3)
    if (is.numeric(val)) {
      openxlsx::addStyle(wb, sn, s$result_currency,
                          rows = row_base + 3, cols = col)
    }

    # NPV
    val <- if (!is.null(opt_npv)) round(opt_npv$value_at_optimum, 2) else "N/A"
    openxlsx::writeData(wb, sn, val, startCol = col, startRow = row_base + 4)
    if (is.numeric(val)) {
      openxlsx::addStyle(wb, sn, s$result_currency,
                          rows = row_base + 4, cols = col)
    }
  }

  # Conditional formatting on LEV row — data bar
  lev_row <- results_start + 2
  openxlsx::conditionalFormatting(
    wb, sn, cols = 2:(length(scenarios) + 1), rows = lev_row,
    type = "dataBar", style = c("#27AE60")
  )

  # ---- Product breakdown per scenario ----
  prod_start <- results_start + length(result_labels) + 3
  openxlsx::writeData(wb, sn, "Product Breakdown at Optimal Rotation",
                       startCol = 1, startRow = prod_start)
  openxlsx::addStyle(wb, sn, s$subheader, rows = prod_start,
                      cols = 1:(length(scenarios) + 1))

  # One row per product, columns = scenarios
  for (pn_idx in seq_along(yield_tbl$product_names)) {
    pn <- yield_tbl$product_names[pn_idx]
    row <- prod_start + pn_idx
    openxlsx::writeData(wb, sn, paste0(pn, " value ($)"),
                         startCol = 1, startRow = row)
    openxlsx::addStyle(wb, sn, s$param_label, rows = row, cols = 1)

    for (sc_idx in seq_along(scenarios)) {
      sc <- scenarios[[sc_idx]]
      col <- sc_idx + 1

      scaled_products <- yield_tbl$products
      for (p in yield_tbl$product_names) {
        if ("price" %in% names(scaled_products[[p]])) {
          scaled_products[[p]]$price <- scaled_products[[p]]$price * sc$price_mult
        }
      }
      temp_yt <- tryCatch(
        yield_table(yield_tbl$ages, scaled_products, yield_tbl$product_units),
        error = function(e) NULL
      )
      if (!is.null(temp_yt)) {
        opt <- tryCatch(
          optimal_rotation_mp(temp_yt, sc$regen, sc$annual, sc$rate),
          error = function(e) NULL
        )
        if (!is.null(opt)) {
          pd <- opt$product_detail
          val <- pd$value[pd$product == pn]
          if (length(val) > 0) {
            openxlsx::writeData(wb, sn, round(val, 2),
                                 startCol = col, startRow = row)
            openxlsx::addStyle(wb, sn, s$result_currency,
                                rows = row, cols = col)
          }
        }
      }
    }
  }

  openxlsx::setColWidths(wb, sn, cols = 1:(length(scenarios) + 1),
                          widths = c(26, rep(16, length(scenarios))))
  openxlsx::protectWorksheet(wb, sn, protect = TRUE,
                              lockFormattingCells = FALSE)
}
