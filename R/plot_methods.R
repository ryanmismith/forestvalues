#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_vline geom_hline
#'   geom_bar geom_histogram geom_tile geom_segment labs theme_minimal
#'   scale_fill_gradient2 coord_flip element_text
#' @importFrom stats reorder
NULL

#' Plot One-Way Sensitivity Analysis
#'
#' @param x A \code{sensitivity_1way} object.
#' @param ... Additional arguments (unused).
#'
#' @return A ggplot2 object.
#' @export
plot.sensitivity_1way <- function(x, ...) {
  pname <- attr(x, "param_name")
  base_val <- attr(x, "base_value")

  p <- ggplot2::ggplot(x, ggplot2::aes(x = .data$param_value, y = .data$outcome)) +
    ggplot2::geom_line(linewidth = 1, color = "#2c7fb8") +
    ggplot2::geom_point(size = 2, color = "#2c7fb8") +
    ggplot2::geom_vline(xintercept = base_val, linetype = "dashed", color = "gray50") +
    ggplot2::labs(x = pname, y = "Outcome",
                  title = paste("Sensitivity to", pname)) +
    ggplot2::theme_minimal()

  p
}


#' Plot Two-Way Sensitivity Analysis
#'
#' @param x A \code{sensitivity_2way} object.
#' @param ... Additional arguments (unused).
#'
#' @return A ggplot2 object (heatmap).
#' @export
plot.sensitivity_2way <- function(x, ...) {
  # Expand matrix to long format
  df <- expand.grid(x_val = x$x_values, y_val = x$y_values)
  df$outcome <- as.vector(x$matrix)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x_val, y = .data$y_val,
                                         fill = .data$outcome)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "#d73027", mid = "#ffffbf",
                                  high = "#1a9850", midpoint = 0) +
    ggplot2::labs(x = x$param_x, y = x$param_y, fill = "Outcome",
                  title = paste("Sensitivity:", x$param_x, "vs.", x$param_y)) +
    ggplot2::theme_minimal()

  p
}


#' Plot Tornado Diagram
#'
#' @param x A \code{tornado} object.
#' @param ... Additional arguments (unused).
#'
#' @return A ggplot2 object.
#' @export
plot.tornado <- function(x, ...) {
  base_outcome <- attr(x, "base_outcome")

  # Prepare data for horizontal bar chart
  df <- x
  df$parameter <- factor(df$parameter, levels = rev(df$parameter))
  df$min_outcome <- pmin(df$low_outcome, df$high_outcome)
  df$max_outcome <- pmax(df$low_outcome, df$high_outcome)

  p <- ggplot2::ggplot(df) +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$min_outcome, xend = .data$max_outcome,
                   y = .data$parameter, yend = .data$parameter),
      linewidth = 8, color = "#2c7fb8"
    ) +
    ggplot2::geom_vline(xintercept = base_outcome, linetype = "dashed",
                         color = "gray30") +
    ggplot2::labs(x = "Outcome", y = NULL,
                  title = "Tornado Diagram: Parameter Sensitivity") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 11))

  p
}


#' Plot Monte Carlo Results
#'
#' @param x An \code{mc_forest} object.
#' @param ... Additional arguments (unused).
#'
#' @return A ggplot2 object (histogram).
#' @export
plot.mc_forest <- function(x, ...) {
  df <- data.frame(value = x$values)
  q5 <- x$summary$quantiles["5%"]
  q95 <- x$summary$quantiles["95%"]

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$value)) +
    ggplot2::geom_histogram(bins = 50, fill = "#2c7fb8", color = "white",
                             alpha = 0.8) +
    ggplot2::geom_vline(xintercept = x$summary$mean, color = "red",
                         linewidth = 1) +
    ggplot2::geom_vline(xintercept = c(q5, q95), color = "orange",
                         linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 0, color = "gray30", linetype = "dotted") +
    ggplot2::labs(
      x = "Outcome", y = "Frequency",
      title = "Monte Carlo Simulation Results",
      subtitle = paste0("Mean = ", round(x$summary$mean, 2),
                        " | P(loss) = ", round(x$summary$prob_loss * 100, 1), "%")
    ) +
    ggplot2::theme_minimal()

  p
}


#' Plot Cash Flow Schedule
#'
#' @param x A \code{cash_flow_schedule} object.
#' @param ... Additional arguments (unused).
#'
#' @return A ggplot2 object.
#' @export
plot.cash_flow_schedule <- function(x, ...) {
  p <- ggplot2::ggplot(x, ggplot2::aes(x = .data$year, y = .data$cumulative_npv)) +
    ggplot2::geom_line(linewidth = 1, color = "#2c7fb8") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_bar(ggplot2::aes(y = .data$cash_flow), stat = "identity",
                       fill = ifelse(x$cash_flow >= 0, "#1a9850", "#d73027"),
                       alpha = 0.5) +
    ggplot2::labs(x = "Year", y = "Value ($)",
                  title = "Cash Flow Schedule",
                  subtitle = "Bars = annual cash flows; Line = cumulative NPV") +
    ggplot2::theme_minimal()

  p
}


#' Plot Rotation Comparison
#'
#' @param x A \code{rotation_comparison} object.
#' @param ... Additional arguments (unused).
#'
#' @return A ggplot2 object.
#' @export
plot.rotation_comparison <- function(x, ...) {
  # Reshape for faceted plot
  df <- data.frame(
    age = rep(x$age, 3),
    value = c(x$npv, x$lev, x$mai),
    metric = rep(c("NPV", "LEV", "MAI"), each = nrow(x))
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$age, y = .data$value)) +
    ggplot2::geom_line(linewidth = 1, color = "#2c7fb8") +
    ggplot2::geom_point(size = 1.5, color = "#2c7fb8") +
    ggplot2::facet_wrap(~metric, scales = "free_y", ncol = 1) +
    ggplot2::labs(x = "Rotation Age (years)", y = "Value",
                  title = "Rotation Age Comparison") +
    ggplot2::theme_minimal()

  p
}
