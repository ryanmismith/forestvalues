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
