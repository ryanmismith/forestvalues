#' Export Forest Economics Analysis to Excel
#'
#' Creates a formatted Excel workbook with live formulas, locked result cells,
#' and unlocked parameter cells that coworkers can edit. Designed to accompany
#' reports for people who are comfortable with Excel but not R.
#'
#' The workbook contains:
#' \itemize{
#'   \item **Parameters** sheet: editable cells (highlighted) for discount rate,
#'     stumpage price, regeneration cost, annual cost, and rotation age. All
#'     other sheets reference these cells with live Excel formulas, so changing
#'     a parameter updates the entire workbook automatically.
#'   \item **Yield Table** sheet: the multi-product yield data.
#'   \item **Rotation Analysis** sheet: NPV and LEV at each age with formulas
#'     referencing the Parameters sheet.
#'   \item **Cash Flow** sheet: year-by-year cash flows with discounting formulas.
#'   \item **Summary** sheet: key results (optimal rotation, NPV, LEV, IRR).
#' }
#'
#' Locked cells (results, formulas) are protected; unlocked cells (parameters,
#' yield inputs) are highlighted in light yellow for easy identification.
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
#' @param title Character. Title for the workbook header. Default
#'   "Forest Economics Analysis".
#' @param author Character. Author name for the header. Default "".
#'
#' @return Invisibly returns the file path. The workbook is written to disk.
#'
#' @details
#' Requires the \code{openxlsx} package. If not installed, the function will
#' prompt you to install it.
#'
#' **Excel formulas used:**
#' \itemize{
#'   \item NPV at each age: \code{=revenue/(1+rate)^age - regen_cost - annual_pv}
#'   \item LEV: \code{=npv*(1+rate)^age/((1+rate)^age-1)}
#'   \item Discounted cash flow: \code{=cash_flow/(1+rate)^year}
#'   \item Cumulative NPV: \code{=SUM(discounted_range)}
#' }
#'
#' @references
#' Klemperer, W.D. (1996). *Forest Resource Economics and Finance*. McGraw-Hill.
#'
#' @examples
#' \dontrun{
#' # With a yield table
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
#' export_excel(yt, discount_rate = 0.06, regen_cost = 750,
#'              annual_cost = 50, time_horizon = 80,
#'              file = "stand_analysis.xlsx",
#'              title = "Township 7 Range 10 - Stand Analysis",
#'              author = "Ryan Smith")
#'
#' # With a management schedule
#' activities <- data.frame(
#'   name = c("Planting", "Tax", "Thinning", "Harvest"),
#'   amount = c(-750, -50, 2500, 6600),
#'   year = c(0, 0, 20, 40),
#'   frequency = c("once", "annual", "once", "once"),
#'   period_length = c(NA, NA, NA, NA)
#' )
#' export_excel(schedule = activities, discount_rate = 0.06,
#'              time_horizon = 40, file = "scenario_analysis.xlsx")
#' }
#'
#' @seealso \code{\link{yield_table}}, \code{\link{cash_flow_schedule}}
#'
#' @export
export_excel <- function(yield_tbl = NULL, schedule = NULL,
                          discount_rate = 0.06, regen_cost = 0,
                          annual_cost = 0, time_horizon = 80,
                          file = "forest_analysis.xlsx",
                          title = "Forest Economics Analysis",
                          author = "") {

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required for Excel export. ",
         "Install it with: install.packages('openxlsx')", call. = FALSE)
  }

  if (!grepl("\\.xlsx$", file)) {
    stop("'file' must end in .xlsx", call. = FALSE)
  }

  wb <- openxlsx::createWorkbook()

  # ---- Styles ----
  header_style <- openxlsx::createStyle(
    fontSize = 14, fontColour = "#1a5276", textDecoration = "bold"
  )
  subheader_style <- openxlsx::createStyle(
    fontSize = 11, fontColour = "#2c3e50", textDecoration = "bold",
    border = "bottom", borderColour = "#2c3e50"
  )
  param_label_style <- openxlsx::createStyle(
    fontColour = "#2c3e50", textDecoration = "bold",
    halign = "right"
  )
  # Editable cells: yellow background, unlocked
  editable_style <- openxlsx::createStyle(
    fgFill = "#FFFDE7", border = "TopBottomLeftRight",
    borderColour = "#FBC02D", locked = FALSE,
    numFmt = "0.00"
  )
  editable_pct_style <- openxlsx::createStyle(
    fgFill = "#FFFDE7", border = "TopBottomLeftRight",
    borderColour = "#FBC02D", locked = FALSE,
    numFmt = "0.00%"
  )
  editable_int_style <- openxlsx::createStyle(
    fgFill = "#FFFDE7", border = "TopBottomLeftRight",
    borderColour = "#FBC02D", locked = FALSE,
    numFmt = "0"
  )
  # Locked result cells
  result_style <- openxlsx::createStyle(
    numFmt = "#,##0.00", locked = TRUE
  )
  result_currency <- openxlsx::createStyle(
    numFmt = "$#,##0.00", locked = TRUE
  )
  col_header_style <- openxlsx::createStyle(
    textDecoration = "bold", fgFill = "#D5E8D4",
    border = "TopBottomLeftRight", halign = "center"
  )
  note_style <- openxlsx::createStyle(
    fontColour = "#7f8c8d", fontSize = 9, wrapText = TRUE
  )

  # ===========================================================================
  # PARAMETERS SHEET
  # ===========================================================================
  openxlsx::addWorksheet(wb, "Parameters")

  # Title
  openxlsx::writeData(wb, "Parameters", title, startCol = 1, startRow = 1)
  openxlsx::addStyle(wb, "Parameters", header_style, rows = 1, cols = 1)
  if (nchar(author) > 0) {
    openxlsx::writeData(wb, "Parameters", paste("Author:", author),
                         startCol = 1, startRow = 2)
  }
  openxlsx::writeData(wb, "Parameters",
                       paste("Generated:", format(Sys.Date(), "%B %d, %Y")),
                       startCol = 1, startRow = 3)

  # Section header
  openxlsx::writeData(wb, "Parameters", "Editable Parameters",
                       startCol = 1, startRow = 5)
  openxlsx::addStyle(wb, "Parameters", subheader_style, rows = 5, cols = 1:3)

  openxlsx::writeData(wb, "Parameters",
                       "Change the yellow cells below. All sheets update automatically.",
                       startCol = 1, startRow = 6)
  openxlsx::addStyle(wb, "Parameters", note_style, rows = 6, cols = 1)

  # Parameter cells (B8:B12) — these are the cells everything references
  param_labels <- c("Discount Rate", "Regeneration Cost ($/acre)",
                     "Annual Cost ($/acre/yr)", "Time Horizon (years)")
  param_values <- c(discount_rate, regen_cost, annual_cost, time_horizon)
  param_rows <- 8:11

  for (i in seq_along(param_labels)) {
    openxlsx::writeData(wb, "Parameters", param_labels[i],
                         startCol = 1, startRow = param_rows[i])
    openxlsx::addStyle(wb, "Parameters", param_label_style,
                        rows = param_rows[i], cols = 1)
    openxlsx::writeData(wb, "Parameters", param_values[i],
                         startCol = 2, startRow = param_rows[i])
  }
  # Style editable cells
  openxlsx::addStyle(wb, "Parameters", editable_pct_style,
                      rows = 8, cols = 2)   # discount rate as %
  openxlsx::addStyle(wb, "Parameters", editable_style,
                      rows = 9, cols = 2)   # regen cost
  openxlsx::addStyle(wb, "Parameters", editable_style,
                      rows = 10, cols = 2)  # annual cost
  openxlsx::addStyle(wb, "Parameters", editable_int_style,
                      rows = 11, cols = 2)  # time horizon

  # Product prices (if yield table provided)
  if (!is.null(yield_tbl) && inherits(yield_tbl, "yield_table")) {
    openxlsx::writeData(wb, "Parameters", "Product Prices",
                         startCol = 1, startRow = 13)
    openxlsx::addStyle(wb, "Parameters", subheader_style, rows = 13, cols = 1:3)

    price_row <- 14
    for (pn in yield_tbl$product_names) {
      pdata <- yield_tbl$products[[pn]]
      unit <- if (!is.null(yield_tbl$product_units) &&
                  pn %in% names(yield_tbl$product_units)) {
        yield_tbl$product_units[[pn]]
      } else "unit"

      price_val <- if ("price" %in% names(pdata)) pdata$price[1] else 0

      openxlsx::writeData(wb, "Parameters",
                           paste0(pn, " ($/", unit, ")"),
                           startCol = 1, startRow = price_row)
      openxlsx::addStyle(wb, "Parameters", param_label_style,
                          rows = price_row, cols = 1)
      openxlsx::writeData(wb, "Parameters", price_val,
                           startCol = 2, startRow = price_row)
      openxlsx::addStyle(wb, "Parameters", editable_style,
                          rows = price_row, cols = 2)
      price_row <- price_row + 1
    }
  }

  # Named ranges for easy reference
  # Parameters!$B$8 = discount_rate
  # Parameters!$B$9 = regen_cost
  # Parameters!$B$10 = annual_cost
  # Parameters!$B$11 = time_horizon

  openxlsx::setColWidths(wb, "Parameters", cols = 1:2, widths = c(30, 18))
  openxlsx::protectWorksheet(wb, "Parameters", protect = TRUE,
                              lockFormattingCells = FALSE,
                              lockFormattingColumns = FALSE)

  # ===========================================================================
  # YIELD TABLE SHEET (if provided)
  # ===========================================================================
  if (!is.null(yield_tbl) && inherits(yield_tbl, "yield_table")) {
    openxlsx::addWorksheet(wb, "Yield Table")

    openxlsx::writeData(wb, "Yield Table", "Yield Table (editable volumes)",
                         startCol = 1, startRow = 1)
    openxlsx::addStyle(wb, "Yield Table", header_style, rows = 1, cols = 1)
    openxlsx::writeData(wb, "Yield Table",
                         "Edit volume cells (yellow) to match your stand data.",
                         startCol = 1, startRow = 2)
    openxlsx::addStyle(wb, "Yield Table", note_style, rows = 2, cols = 1)

    # Headers: Age | Product1_vol | Product1_val | Product2_vol | ...
    start_row <- 4
    col <- 1
    openxlsx::writeData(wb, "Yield Table", "Age", startCol = col,
                         startRow = start_row)
    openxlsx::addStyle(wb, "Yield Table", col_header_style,
                        rows = start_row, cols = col)

    prod_vol_cols <- list()
    prod_val_cols <- list()

    for (pn in yield_tbl$product_names) {
      col <- col + 1
      vol_header <- paste0(pn, " Volume")
      openxlsx::writeData(wb, "Yield Table", vol_header,
                           startCol = col, startRow = start_row)
      openxlsx::addStyle(wb, "Yield Table", col_header_style,
                          rows = start_row, cols = col)
      prod_vol_cols[[pn]] <- col

      col <- col + 1
      val_header <- paste0(pn, " Value ($)")
      openxlsx::writeData(wb, "Yield Table", val_header,
                           startCol = col, startRow = start_row)
      openxlsx::addStyle(wb, "Yield Table", col_header_style,
                          rows = start_row, cols = col)
      prod_val_cols[[pn]] <- col
    }

    col <- col + 1
    openxlsx::writeData(wb, "Yield Table", "Total Value ($)",
                         startCol = col, startRow = start_row)
    openxlsx::addStyle(wb, "Yield Table", col_header_style,
                        rows = start_row, cols = col)
    total_val_col <- col

    # Data rows
    price_start_row <- 14  # where prices are on Parameters sheet
    for (i in seq_along(yield_tbl$ages)) {
      data_row <- start_row + i
      age <- yield_tbl$ages[i]

      # Age column
      openxlsx::writeData(wb, "Yield Table", age, startCol = 1,
                           startRow = data_row)

      sum_parts <- c()
      for (j in seq_along(yield_tbl$product_names)) {
        pn <- yield_tbl$product_names[j]
        pdata <- yield_tbl$products[[pn]]
        vol_col <- prod_vol_cols[[pn]]
        val_col <- prod_val_cols[[pn]]

        # Volume: editable
        openxlsx::writeData(wb, "Yield Table", pdata$volume[i],
                             startCol = vol_col, startRow = data_row)
        openxlsx::addStyle(wb, "Yield Table", editable_style,
                            rows = data_row, cols = vol_col)

        # Value: formula = volume * price (from Parameters sheet)
        vol_cell <- paste0(openxlsx::int2col(vol_col), data_row)
        price_cell <- paste0("Parameters!$B$", price_start_row + j - 1)
        formula <- paste0(vol_cell, "*", price_cell)
        openxlsx::writeFormula(wb, "Yield Table", formula,
                                startCol = val_col, startRow = data_row)
        openxlsx::addStyle(wb, "Yield Table", result_currency,
                            rows = data_row, cols = val_col)

        sum_parts <- c(sum_parts, paste0(openxlsx::int2col(val_col), data_row))
      }

      # Total value: sum of product values
      total_formula <- paste(sum_parts, collapse = "+")
      openxlsx::writeFormula(wb, "Yield Table", total_formula,
                              startCol = total_val_col, startRow = data_row)
      openxlsx::addStyle(wb, "Yield Table", result_currency,
                          rows = data_row, cols = total_val_col)
    }

    openxlsx::setColWidths(wb, "Yield Table", cols = 1:total_val_col,
                            widths = c(8, rep(c(14, 14), length(yield_tbl$product_names)), 14))
    openxlsx::protectWorksheet(wb, "Yield Table", protect = TRUE,
                                lockFormattingCells = FALSE)

    # =========================================================================
    # ROTATION ANALYSIS SHEET
    # =========================================================================
    openxlsx::addWorksheet(wb, "Rotation Analysis")

    openxlsx::writeData(wb, "Rotation Analysis", "Rotation Age Analysis",
                         startCol = 1, startRow = 1)
    openxlsx::addStyle(wb, "Rotation Analysis", header_style, rows = 1, cols = 1)
    openxlsx::writeData(wb, "Rotation Analysis",
                         "NPV and LEV at each age. Formulas reference Parameters sheet.",
                         startCol = 1, startRow = 2)
    openxlsx::addStyle(wb, "Rotation Analysis", note_style, rows = 2, cols = 1)

    ra_headers <- c("Age", "Total Revenue", "PV Revenue", "PV Annual Cost",
                     "Rotation NPV", "LEV")
    ra_start <- 4
    for (hc in seq_along(ra_headers)) {
      openxlsx::writeData(wb, "Rotation Analysis", ra_headers[hc],
                           startCol = hc, startRow = ra_start)
      openxlsx::addStyle(wb, "Rotation Analysis", col_header_style,
                          rows = ra_start, cols = hc)
    }

    # Reference cells
    rate_ref <- "Parameters!$B$8"
    regen_ref <- "Parameters!$B$9"
    annual_ref <- "Parameters!$B$10"

    # One row per age from yield table
    yt_data_start <- start_row + 1  # first data row in Yield Table sheet
    for (i in seq_along(yield_tbl$ages)) {
      row <- ra_start + i
      age <- yield_tbl$ages[i]

      # Age
      openxlsx::writeData(wb, "Rotation Analysis", age,
                           startCol = 1, startRow = row)

      # Total Revenue = from yield table total_val_col
      rev_ref <- paste0("'Yield Table'!", openxlsx::int2col(total_val_col),
                         yt_data_start + i - 1)
      openxlsx::writeFormula(wb, "Rotation Analysis", rev_ref,
                              startCol = 2, startRow = row)
      openxlsx::addStyle(wb, "Rotation Analysis", result_currency,
                          rows = row, cols = 2)

      # PV Revenue = B{row} / (1+rate)^age
      pv_rev_f <- paste0("B", row, "/(1+", rate_ref, ")^A", row)
      openxlsx::writeFormula(wb, "Rotation Analysis", pv_rev_f,
                              startCol = 3, startRow = row)
      openxlsx::addStyle(wb, "Rotation Analysis", result_currency,
                          rows = row, cols = 3)

      # PV Annual Cost = annual * ((1+r)^n - 1) / (r * (1+r)^n)
      pv_annual_f <- paste0(annual_ref, "*((1+", rate_ref, ")^A", row,
                             "-1)/(", rate_ref, "*(1+", rate_ref, ")^A", row, ")")
      openxlsx::writeFormula(wb, "Rotation Analysis", pv_annual_f,
                              startCol = 4, startRow = row)
      openxlsx::addStyle(wb, "Rotation Analysis", result_currency,
                          rows = row, cols = 4)

      # Rotation NPV = PV Revenue - Regen Cost - PV Annual Cost
      npv_f <- paste0("C", row, "-", regen_ref, "-D", row)
      openxlsx::writeFormula(wb, "Rotation Analysis", npv_f,
                              startCol = 5, startRow = row)
      openxlsx::addStyle(wb, "Rotation Analysis", result_currency,
                          rows = row, cols = 5)

      # LEV = NPV * (1+r)^T / ((1+r)^T - 1)
      lev_f <- paste0("E", row, "*(1+", rate_ref, ")^A", row,
                       "/((1+", rate_ref, ")^A", row, "-1)")
      openxlsx::writeFormula(wb, "Rotation Analysis", lev_f,
                              startCol = 6, startRow = row)
      openxlsx::addStyle(wb, "Rotation Analysis", result_currency,
                          rows = row, cols = 6)
    }

    openxlsx::setColWidths(wb, "Rotation Analysis", cols = 1:6,
                            widths = c(8, 16, 16, 16, 16, 16))
    openxlsx::protectWorksheet(wb, "Rotation Analysis", protect = TRUE)
  }

  # ===========================================================================
  # CASH FLOW SHEET (if schedule provided)
  # ===========================================================================
  if (!is.null(schedule) && is.data.frame(schedule)) {
    openxlsx::addWorksheet(wb, "Cash Flow")

    openxlsx::writeData(wb, "Cash Flow", "Cash Flow Schedule",
                         startCol = 1, startRow = 1)
    openxlsx::addStyle(wb, "Cash Flow", header_style, rows = 1, cols = 1)

    # Activities table
    openxlsx::writeData(wb, "Cash Flow", "Management Activities",
                         startCol = 1, startRow = 3)
    openxlsx::addStyle(wb, "Cash Flow", subheader_style, rows = 3, cols = 1:5)

    act_headers <- intersect(c("name", "amount", "year", "frequency", "period_length"),
                              names(schedule))
    for (hc in seq_along(act_headers)) {
      openxlsx::writeData(wb, "Cash Flow", act_headers[hc],
                           startCol = hc, startRow = 4)
      openxlsx::addStyle(wb, "Cash Flow", col_header_style,
                          rows = 4, cols = hc)
    }
    for (i in seq_len(nrow(schedule))) {
      for (hc in seq_along(act_headers)) {
        val <- schedule[[act_headers[hc]]][i]
        openxlsx::writeData(wb, "Cash Flow", val,
                             startCol = hc, startRow = 4 + i)
        if (act_headers[hc] == "amount") {
          openxlsx::addStyle(wb, "Cash Flow", editable_style,
                              rows = 4 + i, cols = hc)
        }
      }
    }

    # Expanded cash flow
    cf <- cash_flow_schedule(schedule, time_horizon, discount_rate)
    cf_start_row <- 4 + nrow(schedule) + 3
    cf_headers <- c("Year", "Cash Flow", "Cumulative", "Discounted",
                     "Cumulative NPV")

    openxlsx::writeData(wb, "Cash Flow", "Year-by-Year Cash Flows",
                         startCol = 1, startRow = cf_start_row - 1)
    openxlsx::addStyle(wb, "Cash Flow", subheader_style,
                        rows = cf_start_row - 1, cols = 1:5)

    for (hc in seq_along(cf_headers)) {
      openxlsx::writeData(wb, "Cash Flow", cf_headers[hc],
                           startCol = hc, startRow = cf_start_row)
      openxlsx::addStyle(wb, "Cash Flow", col_header_style,
                          rows = cf_start_row, cols = hc)
    }

    for (i in seq_len(nrow(cf))) {
      row <- cf_start_row + i
      openxlsx::writeData(wb, "Cash Flow", cf$year[i], startCol = 1, startRow = row)
      openxlsx::writeData(wb, "Cash Flow", cf$cash_flow[i], startCol = 2, startRow = row)

      # Cumulative
      if (i == 1) {
        cum_f <- paste0("B", row)
      } else {
        cum_f <- paste0("C", row - 1, "+B", row)
      }
      openxlsx::writeFormula(wb, "Cash Flow", cum_f, startCol = 3, startRow = row)

      # Discounted = cash_flow / (1+rate)^year
      disc_f <- paste0("B", row, "/(1+Parameters!$B$8)^A", row)
      openxlsx::writeFormula(wb, "Cash Flow", disc_f, startCol = 4, startRow = row)

      # Cumulative NPV
      if (i == 1) {
        cnpv_f <- paste0("D", row)
      } else {
        cnpv_f <- paste0("E", row - 1, "+D", row)
      }
      openxlsx::writeFormula(wb, "Cash Flow", cnpv_f, startCol = 5, startRow = row)

      for (cc in 2:5) {
        openxlsx::addStyle(wb, "Cash Flow", result_currency,
                            rows = row, cols = cc)
      }
    }

    openxlsx::setColWidths(wb, "Cash Flow", cols = 1:5,
                            widths = c(8, 14, 14, 14, 16))
    openxlsx::protectWorksheet(wb, "Cash Flow", protect = TRUE,
                                lockFormattingCells = FALSE)
  }

  # ===========================================================================
  # SUMMARY SHEET
  # ===========================================================================
  openxlsx::addWorksheet(wb, "Summary")

  openxlsx::writeData(wb, "Summary", title, startCol = 1, startRow = 1)
  openxlsx::addStyle(wb, "Summary", header_style, rows = 1, cols = 1)
  if (nchar(author) > 0) {
    openxlsx::writeData(wb, "Summary", paste("Prepared by:", author),
                         startCol = 1, startRow = 2)
  }
  openxlsx::writeData(wb, "Summary",
                       paste("Generated:", format(Sys.Date(), "%B %d, %Y")),
                       startCol = 1, startRow = 3)

  openxlsx::writeData(wb, "Summary", "Analysis Parameters",
                       startCol = 1, startRow = 5)
  openxlsx::addStyle(wb, "Summary", subheader_style, rows = 5, cols = 1:2)

  # Link back to parameters
  sum_params <- c("Discount Rate", "Regeneration Cost", "Annual Cost",
                   "Time Horizon")
  sum_refs <- c("Parameters!$B$8", "Parameters!$B$9",
                "Parameters!$B$10", "Parameters!$B$11")
  for (i in seq_along(sum_params)) {
    openxlsx::writeData(wb, "Summary", sum_params[i],
                         startCol = 1, startRow = 5 + i)
    openxlsx::addStyle(wb, "Summary", param_label_style,
                        rows = 5 + i, cols = 1)
    openxlsx::writeFormula(wb, "Summary", sum_refs[i],
                            startCol = 2, startRow = 5 + i)
  }

  # R-computed results
  openxlsx::writeData(wb, "Summary", "Key Results (computed in R)",
                       startCol = 1, startRow = 11)
  openxlsx::addStyle(wb, "Summary", subheader_style, rows = 11, cols = 1:2)

  result_row <- 12
  if (!is.null(yield_tbl) && inherits(yield_tbl, "yield_table")) {
    # Compute optimal rotation
    opt <- tryCatch(
      optimal_rotation_mp(yield_tbl, regen_cost, annual_cost, discount_rate),
      error = function(e) NULL
    )
    if (!is.null(opt)) {
      results_data <- list(
        "Optimal Rotation (LEV)" = paste0(round(opt$optimal_age), " years"),
        "LEV at Optimum" = paste0("$", formatC(opt$value_at_optimum, format = "f",
                                                 digits = 2, big.mark = ",")),
        "Revenue at Optimum" = paste0("$", formatC(opt$revenue_at_optimum, format = "f",
                                                     digits = 2, big.mark = ","))
      )
      for (rd in names(results_data)) {
        openxlsx::writeData(wb, "Summary", rd, startCol = 1, startRow = result_row)
        openxlsx::addStyle(wb, "Summary", param_label_style,
                            rows = result_row, cols = 1)
        openxlsx::writeData(wb, "Summary", results_data[[rd]],
                             startCol = 2, startRow = result_row)
        result_row <- result_row + 1
      }

      # Product breakdown
      result_row <- result_row + 1
      openxlsx::writeData(wb, "Summary", "Product Breakdown at Optimal Age",
                           startCol = 1, startRow = result_row)
      openxlsx::addStyle(wb, "Summary", subheader_style,
                          rows = result_row, cols = 1:3)
      result_row <- result_row + 1

      for (rr in seq_len(nrow(opt$product_detail))) {
        openxlsx::writeData(wb, "Summary", opt$product_detail$product[rr],
                             startCol = 1, startRow = result_row)
        openxlsx::writeData(wb, "Summary",
                             round(opt$product_detail$volume[rr], 1),
                             startCol = 2, startRow = result_row)
        openxlsx::writeData(wb, "Summary",
                             paste0("$", round(opt$product_detail$value[rr], 2)),
                             startCol = 3, startRow = result_row)
        result_row <- result_row + 1
      }
    }
  }

  if (!is.null(schedule)) {
    # Schedule NPV
    sched_npv <- npv_schedule(
      schedule[, c("amount", "year", "frequency", "period_length")],
      discount_rate, time_horizon
    )
    openxlsx::writeData(wb, "Summary", "Schedule NPV",
                         startCol = 1, startRow = result_row)
    openxlsx::addStyle(wb, "Summary", param_label_style,
                        rows = result_row, cols = 1)
    openxlsx::writeData(wb, "Summary",
                         paste0("$", formatC(sched_npv, format = "f",
                                              digits = 2, big.mark = ",")),
                         startCol = 2, startRow = result_row)
    result_row <- result_row + 1
  }

  # Note about formula updates
  result_row <- result_row + 2
  openxlsx::writeData(wb, "Summary",
                       paste0("Note: The rotation analysis and cash flow sheets contain ",
                              "live Excel formulas that update when you change parameters. ",
                              "The 'Key Results' above are computed in R and do NOT update ",
                              "automatically. Re-export from R for updated R-computed results."),
                       startCol = 1, startRow = result_row)
  openxlsx::addStyle(wb, "Summary", note_style, rows = result_row, cols = 1)
  openxlsx::mergeCells(wb, "Summary", cols = 1:3, rows = result_row)

  openxlsx::setColWidths(wb, "Summary", cols = 1:3, widths = c(30, 20, 16))
  openxlsx::protectWorksheet(wb, "Summary", protect = TRUE)

  # ===========================================================================
  # SAVE
  # ===========================================================================
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  message("Workbook saved to: ", file)
  message("Editable cells are highlighted in yellow on the Parameters and Yield Table sheets.")

  invisible(file)
}
