# Internal helper: create shared styles for Excel export
# Not exported — used by export_excel() and sheet builder functions
.excel_styles <- function() {
  list(
    header = openxlsx::createStyle(
      fontSize = 14, fontColour = "#1a5276", textDecoration = "bold"
    ),
    subheader = openxlsx::createStyle(
      fontSize = 11, fontColour = "#2c3e50", textDecoration = "bold",
      border = "bottom", borderColour = "#2c3e50"
    ),
    param_label = openxlsx::createStyle(
      fontColour = "#2c3e50", textDecoration = "bold", halign = "right"
    ),
    editable = openxlsx::createStyle(
      fgFill = "#FFFDE7", border = "TopBottomLeftRight",
      borderColour = "#FBC02D", locked = FALSE, numFmt = "0.00"
    ),
    editable_pct = openxlsx::createStyle(
      fgFill = "#FFFDE7", border = "TopBottomLeftRight",
      borderColour = "#FBC02D", locked = FALSE, numFmt = "0.00%"
    ),
    editable_int = openxlsx::createStyle(
      fgFill = "#FFFDE7", border = "TopBottomLeftRight",
      borderColour = "#FBC02D", locked = FALSE, numFmt = "0"
    ),
    result = openxlsx::createStyle(
      numFmt = "#,##0.00", locked = TRUE
    ),
    result_currency = openxlsx::createStyle(
      numFmt = "$#,##0.00", locked = TRUE
    ),
    result_pct = openxlsx::createStyle(
      numFmt = "0.00%", locked = TRUE
    ),
    col_header = openxlsx::createStyle(
      textDecoration = "bold", fgFill = "#D5E8D4",
      border = "TopBottomLeftRight", halign = "center"
    ),
    col_header_blue = openxlsx::createStyle(
      textDecoration = "bold", fgFill = "#D4E6F1",
      border = "TopBottomLeftRight", halign = "center"
    ),
    col_header_orange = openxlsx::createStyle(
      textDecoration = "bold", fgFill = "#FAD7A0",
      border = "TopBottomLeftRight", halign = "center"
    ),
    note = openxlsx::createStyle(
      fontColour = "#7f8c8d", fontSize = 9, wrapText = TRUE
    ),
    negative = openxlsx::createStyle(
      fontColour = "#C0392B", numFmt = "$#,##0.00"
    ),
    positive = openxlsx::createStyle(
      fontColour = "#27AE60", numFmt = "$#,##0.00"
    ),
    border_all = openxlsx::createStyle(
      border = "TopBottomLeftRight", borderColour = "#BDC3C7"
    ),
    bold = openxlsx::createStyle(textDecoration = "bold"),
    italic_note = openxlsx::createStyle(
      fontColour = "#7f8c8d", fontSize = 9, textDecoration = "italic"
    ),
    scenario_base = openxlsx::createStyle(
      fgFill = "#E8F8F5", border = "TopBottomLeftRight"
    ),
    scenario_alt = openxlsx::createStyle(
      fgFill = "#FEF9E7", border = "TopBottomLeftRight",
      locked = FALSE
    )
  )
}
