#' Monte Carlo Simulation for Forest Economics
#'
#' Runs Monte Carlo simulations to assess uncertainty in forest economic
#' outcomes (NPV, LEV, IRR). Varies user-specified parameters according to
#' probability distributions and returns the distribution of outcomes.
#'
#' Forest investments span 20-80+ years. Pretending that stumpage prices,
#' timber yields, discount rates, and costs are known with certainty ignores
#' the reality of long-horizon investments. Monte Carlo simulation propagates
#' this uncertainty through the financial model.
#'
#' @param eval_fn A function that takes named parameters and returns a single
#'   numeric value (e.g., NPV, LEV). This is the function whose output
#'   uncertainty you want to assess.
#' @param base_params A named list of fixed parameter values passed to
#'   \code{eval_fn}.
#' @param stochastic_params A named list where each element specifies a
#'   parameter to vary. Each element should be a list with:
#'   \describe{
#'     \item{dist}{Character. Distribution: "normal", "lognormal", "uniform", "triangular".}
#'     \item{...}{Distribution parameters: "normal" uses \code{mean}, \code{sd};
#'       "lognormal" uses \code{meanlog}, \code{sdlog};
#'       "uniform" uses \code{min}, \code{max};
#'       "triangular" uses \code{min}, \code{mode}, \code{max}.}
#'   }
#' @param n_sims Integer. Number of simulations. Default 10000.
#' @param seed Integer or NULL. Random seed for reproducibility.
#'
#' @return An object of class \code{"mc_forest"} containing:
#'   \describe{
#'     \item{values}{Numeric vector of simulated outcomes.}
#'     \item{summary}{Named list with mean, sd, median, quantiles, prob_loss.}
#'     \item{n_sims}{Number of simulations run.}
#'     \item{stochastic_params}{The distributions used.}
#'   }
#'
#' @references
#' Brazee, R.J. & Mendelsohn, R. (1988). "Timber harvesting with fluctuating
#' prices." *Forest Science* 34(2): 359-372.
#'
#' Klemperer, W.D. (1996). *Forest Resource Economics and Finance*.
#' McGraw-Hill. Ch. 10.
#'
#' @examples
#' # Uncertain stumpage price and discount rate
#' my_npv <- function(price, cost, discount_rate) {
#'   npv(c(-cost, price * 150), c(0, 30), discount_rate)
#' }
#'
#' result <- monte_carlo(
#'   eval_fn = my_npv,
#'   base_params = list(cost = 750),
#'   stochastic_params = list(
#'     price = list(dist = "lognormal", meanlog = log(50), sdlog = 0.3),
#'     discount_rate = list(dist = "uniform", min = 0.03, max = 0.09)
#'   ),
#'   n_sims = 1000, seed = 42
#' )
#' summary(result)
#'
#' @seealso \code{\link{stochastic_prices}}, \code{\link{risk_metrics}},
#'   \code{\link{simulate_yield}}
#'
#' @family stochastic
#' @export
monte_carlo <- function(eval_fn, base_params, stochastic_params,
                         n_sims = 10000, seed = NULL) {
  if (!is.function(eval_fn)) stop("'eval_fn' must be a function", call. = FALSE)
  if (!is.list(base_params)) stop("'base_params' must be a named list", call. = FALSE)
  if (!is.list(stochastic_params)) {
    stop("'stochastic_params' must be a named list", call. = FALSE)
  }

  if (!is.null(seed)) set.seed(seed)

  results <- numeric(n_sims)

  for (i in seq_len(n_sims)) {
    params <- base_params

    # Draw from each stochastic parameter's distribution
    for (param_name in names(stochastic_params)) {
      dist_spec <- stochastic_params[[param_name]]
      params[[param_name]] <- draw_from_dist(dist_spec)
    }

    results[i] <- tryCatch(
      do.call(eval_fn, params),
      error = function(e) NA_real_
    )
  }

  # Remove NAs and warn
  n_failed <- sum(is.na(results))
  if (n_failed > 0) {
    warning(n_failed, " of ", n_sims, " simulations failed and were excluded",
            call. = FALSE)
    results <- results[!is.na(results)]
  }

  structure(
    list(
      values = results,
      summary = list(
        mean = mean(results),
        sd = stats::sd(results),
        median = stats::median(results),
        quantiles = stats::quantile(results, c(0.05, 0.25, 0.50, 0.75, 0.95)),
        prob_loss = mean(results < 0)
      ),
      n_sims = length(results),
      stochastic_params = stochastic_params
    ),
    class = "mc_forest"
  )
}


#' @family stochastic
#' @export
summary.mc_forest <- function(object, ...) {
  cat("Monte Carlo Forest Economics Simulation\n")
  cat("Simulations:", object$n_sims, "\n")
  cat("Stochastic parameters:", paste(names(object$stochastic_params), collapse = ", "), "\n\n")
  cat("Mean:      ", formatC(object$summary$mean, format = "f", digits = 2), "\n")
  cat("Std Dev:   ", formatC(object$summary$sd, format = "f", digits = 2), "\n")
  cat("Median:    ", formatC(object$summary$median, format = "f", digits = 2), "\n")
  cat("P(loss):   ", formatC(object$summary$prob_loss * 100, format = "f", digits = 1), "%\n\n")
  cat("Percentiles:\n")
  q <- object$summary$quantiles
  for (i in seq_along(q)) {
    cat("  ", names(q)[i], ":", formatC(q[i], format = "f", digits = 2), "\n")
  }
  invisible(object)
}


#' Draw from a distribution specification
#'
#' @param spec A list with \code{dist} and distribution parameters.
#' @return A single random draw.
#' @keywords internal
draw_from_dist <- function(spec) {
  dist <- spec$dist
  if (dist == "normal") {
    stats::rnorm(1, mean = spec$mean, sd = spec$sd)
  } else if (dist == "lognormal") {
    stats::rlnorm(1, meanlog = spec$meanlog, sdlog = spec$sdlog)
  } else if (dist == "uniform") {
    stats::runif(1, min = spec$min, max = spec$max)
  } else if (dist == "triangular") {
    # Triangular distribution using inverse CDF method
    a <- spec$min
    b <- spec$max
    c <- spec$mode
    u <- stats::runif(1)
    fc <- (c - a) / (b - a)
    if (u < fc) {
      a + sqrt(u * (b - a) * (c - a))
    } else {
      b - sqrt((1 - u) * (b - a) * (b - c))
    }
  } else {
    stop("Unknown distribution: '", dist, "'. Supported: normal, lognormal, ",
         "uniform, triangular", call. = FALSE)
  }
}


#' Simulate Stochastic Price Paths
#'
#' Generates simulated price paths using either Geometric Brownian Motion (GBM)
#' or an Ornstein-Uhlenbeck (OU) mean-reverting process. Both are standard
#' models in timber price literature.
#'
#' **GBM** (Geometric Brownian Motion): Prices follow a random walk with drift.
#' Prices can trend upward/downward and cannot go negative. Standard in
#' financial economics.
#' \deqn{dP = \mu P \, dt + \sigma P \, dW}
#'
#' **OU** (Ornstein-Uhlenbeck): Prices revert toward a long-run mean. More
#' realistic for commodity prices that are mean-reverting due to supply
#' adjustments (Brazee & Mendelsohn 1988; Thomson 1992; Insley 2002).
#' \deqn{dP = \kappa(\theta - P) \, dt + \sigma \, dW}
#'
#' **Empirical parameter guidance (Southern pine sawtimber):**
#' \describe{
#'   \item{OU}{kappa = 0.12-0.25, theta = $240-$320/MBF, sigma = $35-$55/MBF
#'     (Mei, Clutter & Harris 2010; Prestemon & Wear 2000)}
#'   \item{GBM}{mu = 0.5-2.5\%, sigma = 12-20\%
#'     (Thomson 1992; Brazee & Mendelsohn 1988)}
#' }
#' See the package vignette (\code{vignette("forest_economics_guide")},
#' Section "Empirical Price Process Parameters") for full parameter tables
#' by product and region.
#'
#' @param n_periods Integer. Number of time periods (years) to simulate.
#' @param initial_price Numeric. Starting price.
#' @param process Character. "gbm" for Geometric Brownian Motion or "ou" for
#'   Ornstein-Uhlenbeck mean-reverting process.
#' @param drift Numeric. Annual drift rate for GBM (mu). Default 0.
#' @param volatility Numeric. Annual volatility (sigma). Default 0.2.
#'   For GBM this is proportional volatility (e.g., 0.20 = 20\% per year).
#'   For OU this is absolute volatility in price units (e.g., 45 = $45/MBF).
#' @param mean_reversion_rate Numeric. Speed of mean reversion for OU (kappa).
#'   Half-life of deviation = ln(2)/kappa. Required when process = "ou".
#' @param long_run_mean Numeric. Long-run equilibrium price for OU (theta).
#'   Required when process = "ou".
#' @param n_paths Integer. Number of price paths to simulate. Default 1.
#' @param seed Integer or NULL. Random seed.
#'
#' @return A matrix with \code{n_periods + 1} rows and \code{n_paths} columns.
#'   Row 1 is the initial price.
#'
#' @references
#' Brazee, R.J. & Mendelsohn, R. (1988). "Timber harvesting with fluctuating
#' prices." *Forest Science* 34(2): 359-372.
#'
#' Thomson, T.A. (1992). "Optimal forest rotation when stumpage prices follow
#' a diffusion process." *Land Economics* 68(3): 329-342.
#'
#' Yoshimoto, A. & Shoji, I. (1998). "Searching for an optimal rotation age
#' under stochastic log prices." *European Journal of Operational Research*
#' 105(1): 100-112.
#'
#' Insley, M. (2002). "A real options approach to the valuation of a forestry
#' investment." *Journal of Environmental Economics and Management* 44(3): 471-492.
#'
#' Mei, B., Clutter, M.L., & Harris, T.G. (2010). "Modeling and forecasting
#' pine sawtimber stumpage prices in the US South." *Forest Science* 56(5):
#' 471-479.
#'
#' Dixit, A.K. & Pindyck, R.S. (1994). *Investment Under Uncertainty*.
#' Princeton University Press.
#'
#' @examples
#' # Southern pine sawtimber GBM (Thomson 1992 range)
#' paths <- stochastic_prices(40, 270, process = "gbm",
#'                             drift = 0.015, volatility = 0.18,
#'                             n_paths = 100, seed = 42)
#' matplot(0:40, paths, type = "l", col = "gray70", lty = 1,
#'         xlab = "Year", ylab = "Price ($/MBF)")
#'
#' # Southern pine sawtimber OU (Mei & Clutter 2010 range)
#' # kappa=0.18, theta=$280/MBF, sigma=$45/MBF
#' paths_ou <- stochastic_prices(40, 270, process = "ou",
#'                                volatility = 45, mean_reversion_rate = 0.18,
#'                                long_run_mean = 280, n_paths = 100, seed = 42)
#'
#' # Southern pine pulpwood OU (Prestemon & Wear 2000)
#' paths_pulp <- stochastic_prices(40, 9, process = "ou",
#'                                   volatility = 2.0,
#'                                   mean_reversion_rate = 0.30,
#'                                   long_run_mean = 10,
#'                                   n_paths = 50, seed = 42)
#'
#' @seealso \code{\link{monte_carlo}}, \code{\link{simulate_yield}}
#'
#' @family stochastic
#' @export
stochastic_prices <- function(n_periods, initial_price,
                               process = c("gbm", "ou"),
                               drift = 0, volatility = 0.2,
                               mean_reversion_rate = NULL,
                               long_run_mean = NULL,
                               n_paths = 1, seed = NULL) {
  process <- match.arg(process)

  if (!is.null(seed)) set.seed(seed)

  if (process == "ou") {
    if (is.null(mean_reversion_rate)) {
      stop("'mean_reversion_rate' (kappa) is required for OU process", call. = FALSE)
    }
    if (is.null(long_run_mean)) {
      stop("'long_run_mean' (theta) is required for OU process", call. = FALSE)
    }
  }

  # Initialize matrix: rows = time, cols = paths
  prices <- matrix(NA_real_, nrow = n_periods + 1, ncol = n_paths)
  prices[1, ] <- initial_price

  dt <- 1  # annual time steps

  for (t in seq_len(n_periods)) {
    z <- stats::rnorm(n_paths)

    if (process == "gbm") {
      # Euler-Maruyama discretization of GBM
      # P(t+1) = P(t) * exp((mu - 0.5*sigma^2)*dt + sigma*sqrt(dt)*Z)
      prices[t + 1, ] <- prices[t, ] * exp(
        (drift - 0.5 * volatility^2) * dt + volatility * sqrt(dt) * z
      )
    } else {
      # Euler-Maruyama discretization of OU
      # P(t+1) = P(t) + kappa*(theta - P(t))*dt + sigma*sqrt(dt)*Z
      kappa <- mean_reversion_rate
      theta <- long_run_mean
      prices[t + 1, ] <- prices[t, ] +
        kappa * (theta - prices[t, ]) * dt +
        volatility * sqrt(dt) * z
    }
  }

  prices
}


#' Simulate Stochastic Yield Variation
#'
#' Adds stochastic variation to a deterministic yield curve to capture
#' site quality variability, weather effects, and other biological uncertainty.
#' Uses multiplicative lognormal noise to keep yields positive.
#'
#' Y_sim = Y_det * exp(N(0, sigma^2) - sigma^2/2)
#'
#' The bias correction (-sigma^2/2) ensures E[Y_sim] = Y_det (unbiased).
#'
#' @param yield_fn A function that takes age and returns volume.
#' @param ages Numeric vector. Ages at which to simulate yields.
#' @param cv Numeric. Coefficient of variation for yield noise. Default 0.10.
#' @param n_sims Integer. Number of simulated yield sets. Default 1000.
#' @param seed Integer or NULL. Random seed.
#'
#' @return A matrix with \code{length(ages)} rows and \code{n_sims} columns.
#'
#' @references
#' Brazee, R.J. & Mendelsohn, R. (1988). "Timber harvesting with fluctuating
#' prices." *Forest Science* 34(2): 359-372.
#'
#' @examples
#' yield <- function(age) 200 * (1 - exp(-0.03 * age))^3
#' sim_yields <- simulate_yield(yield, ages = c(20, 30, 40), cv = 0.15,
#'                               n_sims = 1000, seed = 42)
#' # Mean should be close to deterministic values
#' rowMeans(sim_yields)
#' yield(c(20, 30, 40))
#'
#' @family stochastic
#' @export
simulate_yield <- function(yield_fn, ages, cv = 0.10, n_sims = 1000,
                            seed = NULL) {
  if (!is.function(yield_fn)) stop("'yield_fn' must be a function", call. = FALSE)
  if (!is.numeric(ages)) stop("'ages' must be numeric", call. = FALSE)
  if (cv <= 0) stop("'cv' must be positive", call. = FALSE)

  if (!is.null(seed)) set.seed(seed)

  # sigma for lognormal from CV
  sigma <- sqrt(log(1 + cv^2))

  det_yields <- yield_fn(ages)
  n_ages <- length(ages)

  # Multiplicative lognormal noise (unbiased)
  noise <- matrix(stats::rlnorm(n_ages * n_sims,
                                 meanlog = -sigma^2 / 2,
                                 sdlog = sigma),
                   nrow = n_ages, ncol = n_sims)

  det_yields * noise
}


#' Risk Metrics from Simulation Results
#'
#' Computes Value-at-Risk (VaR), probability of loss, and expected shortfall
#' (Conditional VaR) from a vector of simulated outcomes.
#'
#' @param simulations Numeric vector of simulated outcomes (e.g., NPV values),
#'   or an object of class \code{"mc_forest"}.
#' @param threshold Numeric. Value below which outcomes are considered losses.
#'   Default 0.
#' @param confidence Numeric. Confidence level for VaR. Default 0.95.
#'
#' @return A named list:
#'   \describe{
#'     \item{var}{Value-at-Risk: the threshold below which (1-confidence)\% of outcomes fall.}
#'     \item{prob_loss}{Probability that outcome < threshold.}
#'     \item{expected_shortfall}{Mean outcome conditional on exceeding VaR (Conditional VaR).}
#'     \item{mean}{Mean of all simulated outcomes.}
#'     \item{sd}{Standard deviation.}
#'   }
#'
#' @references
#' Klemperer, W.D. (1996). *Forest Resource Economics and Finance*.
#' McGraw-Hill. Ch. 10.
#'
#' @examples
#' # From Monte Carlo results
#' set.seed(42)
#' sims <- rnorm(10000, mean = 5000, sd = 3000)
#' risk_metrics(sims)
#'
#' @family stochastic
#' @export
risk_metrics <- function(simulations, threshold = 0, confidence = 0.95) {
  if (inherits(simulations, "mc_forest")) {
    simulations <- simulations$values
  }
  if (!is.numeric(simulations)) {
    stop("'simulations' must be numeric or an mc_forest object", call. = FALSE)
  }

  var_level <- stats::quantile(simulations, probs = 1 - confidence,
                                names = FALSE)
  prob_loss <- mean(simulations < threshold)
  es <- mean(simulations[simulations <= var_level])

  list(
    var = var_level,
    prob_loss = prob_loss,
    expected_shortfall = es,
    mean = mean(simulations),
    sd = stats::sd(simulations)
  )
}
