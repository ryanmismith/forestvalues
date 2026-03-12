# forestvalues 2.0: A Comprehensive Forest Economics Toolkit
## 10-Point Implementation Plan

**Author**: Claude (for Ryan Smith)
**Date**: 2026-03-11
**Package**: forestvalues
**Current Version**: 0.1.1 → **Target Version**: 2.0.0

---

## Current Package Assessment

### Goals
The `forestvalues` package was designed to solve a real and important problem: replacing error-prone, version-fragmented spreadsheet analyses in forest management with reproducible, standardized R functions. The concept document articulates this well — when one forester works on `T7R10_V2.xlsx` and another on `T7R10_V3.xlsx` with different discount rates, decisions get made on inconsistent math. Functional programming fixes this.

### What It Does Well
1. **Correct architectural instinct**: Building blocks (SinglePayment → SeriesPayment → ComplexNPV) compose into larger analyses. This is the right pattern.
2. **Real-world grounding**: The carbon credits vs. BAU scenario demonstrates genuine forestry practice, not toy examples.
3. **Faustmann LEV**: The `LandExpectVal()` formula is correctly implemented — the most important equation in forest economics.
4. **Species data**: `ValueEstimate()` contains a 44-species stumpage lookup table with real 2019 Maine prices, including green weights and product class breakdowns. This is hard-won data.
5. **Roxygen2 infrastructure**: Documentation scaffolding is in place and can be built upon.

### Critical Deficiencies
1. **Bugs**: SeriesPayment loop processes only one element; ComplexNPV/ComplexLEV use wrong data frames for annual costs/revenues; operator precedence errors in periodic formulas.
2. **Missing core capabilities**: No IRR, no sensitivity analysis, no stochastic modeling, no optimal rotation (stub only).
3. **No tests**: Zero test coverage.
4. **No citations**: No academic references anywhere in the documentation. For a field built on Faustmann (1849), this is a significant gap.
5. **Hardcoded data**: 2019 Maine stumpage prices baked into function code with no way to update or regionalize.
6. **Documentation failures**: ComplexNPV.Rd contains copy-pasted PresentValue documentation with wrong signatures and examples.

---

## The 10-Point Plan

### Point 1: Fix Bugs & Establish Clean Foundation

**Problem**: The existing codebase has bugs that produce incorrect results. Nothing else matters until these are fixed.

**Actions**:

| Bug | File | Fix |
|-----|------|-----|
| SeriesPayment loop | `R/SeriesPayment.R:73` | `for(i in length(...))` → `for(i in 1:length(...))`, or better: vectorize entirely |
| Operator precedence | `R/SeriesPayment.R:123,127,132` | Add explicit parentheses to periodic formulas: `(1+r)^n - 1` not `(1+r)^(n-1)` |
| Annual costs use Periodic data | `R/ComplexNPV.R:99` | `PeriodicCosts$Flow` → `AnnualCosts$Flow` (and all columns in that mapply) |
| Annual revenues use Periodic data | `R/ComplexNPV.R:147` | `PeriodicRevenues$Flow` → `AnnualRevenues$Flow` |
| Same bugs in ComplexLEV | `R/ComplexLEV.R` | Mirror all ComplexNPV fixes |
| Wrong documentation | `man/ComplexNPV.Rd` | Regenerate from corrected roxygen2 |
| Misspelling | All files | `Occurance` → `Occurrence` globally |
| Validation via print() | `R/ComplexNPV.R`, `R/ComplexLEV.R` | Replace `print()`/`ifelse()` validation with proper `if()`/`stop()`/`warning()` |

**Additional foundation work**:
- Add `testthat` infrastructure: `tests/testthat.R`, `tests/testthat/test-*.R`
- Add `ggplot2` to Imports in DESCRIPTION
- Add `R (>= 4.0.0)` dependency
- Standardize all parameter naming to snake_case
- **Remove** old function names entirely (clean break, per user decision)

**Testing**: Verify SeriesPayment produces correct results for all 6 payment types against textbook values (Klemperer Ch. 4 tables).

---

### Point 2: NPV Suite — Net Present Value Done Right

**Problem**: SimpleNPV works but has clunky Class/NegativeValues logic. ComplexNPV is buggy and poorly documented. Neither is cited.

**New functions**:

```r
npv(cash_flows, times, discount_rate)
```
- Clean, vectorized NPV computation: `sum(cash_flows / (1 + discount_rate)^times)`
- Cash flows are signed (negative = cost, positive = revenue). No "Class" labeling.
- Validates inputs: length matching, numeric types, non-negative times.

```r
npv_schedule(schedule, discount_rate, time_horizon)
```
- Takes a data.frame with columns: `amount`, `year`, `frequency` ("once", "annual", "periodic"), `period_length`
- Handles mixed payment types in a single call
- Replaces ComplexNPV with correct implementation

```r
incremental_npv(schedule_a, schedule_b, discount_rate, time_horizon)
```
- Computes NPV(A) - NPV(B) for comparing mutually exclusive alternatives
- Returns the incremental NPV and a combined summary

**Key formulas**:
- NPV = Σ CF_t / (1+r)^t (Klemperer 1996, p. 98)
- For annual series: PV = A × [(1+r)^n - 1] / [r(1+r)^n] (Bullard & Straka 2011, p. 45)
- For periodic series: PV = A × [(1+r)^n - 1] / [(1+r)^p - 1)(1+r)^n] (Klemperer 1996, p. 108)

**Citations**:
- Klemperer, W.D. (1996). *Forest Resource Economics and Finance*. McGraw-Hill.
- Bettinger, P., Boston, K., Siry, J.P., & Grebner, D.L. (2017). *Forest Management and Planning*. 2nd ed. Academic Press.
- Bullard, S.H. & Straka, T.J. (2011). *Basic Concepts in Forest Valuation and Investment Analysis*. 3rd ed.

---

### Point 3: IRR — Internal Rate of Return

**Problem**: IRR doesn't exist in the package. It's the second most-requested metric after NPV.

**New functions**:

```r
irr(cash_flows, times)
```
- Finds the discount rate where NPV = 0 using `stats::uniroot()`
- Checks Descartes' rule of signs: if >1 sign change in cash flows, warns about multiple IRRs
- Search interval: [-0.5, 10] (allows negative IRR detection)
- Returns: numeric scalar (the IRR as a decimal, e.g., 0.08 for 8%)

```r
mirr(cash_flows, times, finance_rate, reinvest_rate)
```
- Modified IRR: separates financing cost from reinvestment rate
- MIRR = (FV of positives at reinvest_rate / PV of negatives at finance_rate)^(1/n) - 1
- Resolves the multiple-IRR problem and the reinvestment rate assumption problem

**Key formulas**:
- IRR: find r such that Σ CF_t / (1+r)^t = 0 (no closed-form; numerical solution)
- MIRR = [(-FV_positives / PV_negatives)^(1/n)] - 1 where FV_positives = Σ max(CF_t,0) × (1+r_reinvest)^(n-t)
- Descartes' rule: number of positive real roots ≤ number of sign changes in the polynomial

**Citations**:
- Klemperer (1996), Ch. 7: "The Internal Rate of Return"
- Bullard & Straka (2011), Ch. 6
- Hazen, G.B. (2003). "A new perspective on multiple internal rates of return." *The Engineering Economist* 48(1): 31-51.
- Boulding, K.E. (1935). "The theory of a single investment." *Quarterly Journal of Economics* 49(3): 475-494. (Original IRR paper)

**Important warning for documentation**: IRR and NPV can give conflicting rankings for mutually exclusive projects (Fisher intersection problem). The QMD must explain this clearly with a forestry example.

---

### Point 4: LEV & Faustmann — The Foundation of Forest Economics

**Problem**: `LandExpectVal()` is correctly implemented but uncited and poorly documented. `ComplexLEV()` has bugs and isn't exported.

**New functions**:

```r
lev(net_revenue, rotation_age, discount_rate)
```
- The Faustmann formula: LEV = NR / ((1+r)^T - 1)
- Where NR is the net revenue at rotation age T, discounted to year 0
- For multi-cash-flow rotations: LEV = NPV_rotation × (1+r)^T / ((1+r)^T - 1)

```r
bare_land_value(net_revenue, rotation_age, discount_rate)
```
- Alias for `lev()` — same computation, different name used by practitioners
- Documentation explains the naming convention difference

```r
lev_schedule(schedule, discount_rate, rotation_age)
```
- Computes LEV for a complex management schedule
- Takes same data.frame format as `npv_schedule()`
- First computes single-rotation NPV, then converts to LEV

**Key formulas**:
- LEV = V_T / ((1+r)^T - 1) for simple case (Faustmann 1849)
- LEV = NPV × (1+r)^T / ((1+r)^T - 1) for general case (Chang 1998)
- Equivalently: LEV = Σ_{j=0}^{∞} NPV / (1+r)^{jT} = NPV / (1 - (1+r)^{-T})

**Citations**:
- Faustmann, M. (1849). "Berechnung des Werthes welchen Waldboden sowie noch nicht haubare Holzbestände für die Waldwirthschaft besitzen." *Allgemeine Forst- und Jagd-Zeitung* 25: 441-455.
- Samuelson, P.A. (1976). "Economics of forestry in an evolving society." *Economic Inquiry* 14(4): 466-492.
- Chang, S.J. (1998). "A generalized Faustmann model for the determination of optimal harvest age." *Canadian Journal of Forest Research* 28(5): 652-659.
- Pearse, P.H. (1990). *Introduction to Forestry Economics*. UBC Press.
- Brazee, R.J. (2001). "The Faustmann Formula: Fundamental to Forest Economics 150 Years After Publication." *Forest Science* 47(4): 441-442.

---

### Point 5: Optimal Rotation Age

**Problem**: `optRotationAge()` is a stub that returns its input. This is arguably the most important applied question in forest economics.

**New functions**:

```r
optimal_rotation(yield_fn, stumpage_price, regen_cost,
                  annual_cost = 0, discount_rate,
                  age_range = c(10, 150),
                  criterion = c("lev", "npv", "mai"))
```
- `criterion = "lev"`: Faustmann rotation — maximizes LEV (economically optimal, accounts for opportunity cost of land)
- `criterion = "npv"`: Single rotation — maximizes NPV (no land opportunity cost; appropriate when land has no alternative use)
- `criterion = "mai"`: Maximum sustained yield — maximizes mean annual increment (biological, not economic)
- `yield_fn`: either a function `f(age)` returning volume, or a data.frame with `age` and `volume` columns (interpolated with `stats::splinefun()`)
- Returns: list with `optimal_age`, `value_at_optimum`, `criterion_used`

```r
rotation_comparison(yield_fn, stumpage_price, regen_cost,
                     annual_cost = 0, discount_rate,
                     ages = seq(10, 100, by = 5))
```
- Tabulates NPV, LEV, MAI, and marginal value of waiting across a range of ages
- Returns a data.frame suitable for plotting
- Includes a `plot.rotation_comparison()` S3 method

**Key economic insight documented**:
- Faustmann rotation is ALWAYS shorter than biological rotation (MSY)
- Higher discount rates → shorter optimal rotations
- Higher regeneration costs → longer optimal rotations
- The marginal condition: harvest when marginal growth rate = `r + r/((1+r)^T - 1)` (Pressler 1860)

**Citations**:
- Faustmann (1849) — LEV-maximizing rotation
- Pressler, M.R. (1860). "Aus der Holzzuwachlehre." *Allgemeine Forst- und Jagd-Zeitung* 36: 173-191.
- Newman, D.H. (2002). "Forestry's golden rule and the development of the optimal forest rotation literature." *Journal of Forest Economics* 8(1): 5-27.
- Amacher, G.S., Ollikainen, M., & Koskela, E. (2009). *Economics of Forest Resources*. MIT Press.
- Johansson, P.O. & Löfgren, K.G. (1985). *The Economics of Forestry and Natural Resources*. Blackwell.

---

### Point 6: Stumpage & Timber Valuation

**Problem**: `ValueEstimate()` has hardcoded 2019 Maine prices with no way to update, regionalize, or customize.

**New functions**:

```r
stumpage_value(species, volume, product_class = "sawlog",
                price_table = NULL, unit = "mbf")
```
- If `price_table = NULL`, uses `maine_stumpage_2019` default with a message noting the vintage
- Accepts user-supplied price tables (data.frame with `species`, `product_class`, `price`, `unit`)
- Supports units: "mbf" (thousand board feet), "cord", "ton", "m3"
- Returns: data.frame with species, volume, unit_price, total_value

```r
create_price_table(species, product_class, price, unit = "mbf")
```
- Helper to build a properly formatted price table
- Validates inputs, checks for common errors (negative prices, unknown units)

```r
update_prices(price_table, inflation_rate = NULL, base_year = NULL, target_year = NULL)
```
- Simple inflation adjustment: `new_price = old_price × (1 + inflation_rate)^(target_year - base_year)`
- User supplies inflation rate (package does NOT fetch economic data)

**Package data**:
- `maine_stumpage_2019`: Extracted from current hardcoded table in ValueEstimate.R
- `data-raw/maine_stumpage_2019.R`: Provenance script with source documentation

**Citations**:
- Wagner, J.E. (2012). *Forestry Economics: A Managerial Approach*. Routledge. Ch. 5.
- Howard, T.E. (2016). "Stumpage price reporting." *Journal of Forestry* 114(2): 195-200.
- Timber Mart-South reporting methodology documentation.

---

### Point 7: Income Projection & Cash Flow Scheduling

**Problem**: No way to build, visualize, or project a complete management cash flow schedule.

**New functions**:

```r
cash_flow_schedule(activities, time_horizon, discount_rate)
```
- `activities`: data.frame with columns `name`, `amount`, `year`, `frequency`, `period_length`, `class` (cost/revenue)
- Expands all activities into an annual cash flow table
- Computes: annual_net, cumulative_net, annual_npv, cumulative_npv
- Returns: data.frame with one row per year, plus `plot.cash_flow_schedule()` method

```r
project_income(yield_fn, stumpage_price, harvest_ages, costs = NULL,
                discount_rate, time_horizon)
```
- Projects timber income from a yield function and price assumptions
- Handles multiple harvests (thinnings + final harvest)
- Integrates with `stumpage_value()` for price lookups

```r
annualize(present_value, discount_rate, n_years)
```
- Converts a lump-sum present value to an equivalent annual annuity
- AE = PV × r / (1 - (1+r)^(-n))
- Useful for comparing projects with different time horizons

```r
discount(future_value, rate, time)
compound(present_value, rate, time)
```
- Clean, simple utility functions replacing the confusing SinglePayment wrapper
- `discount()` = FV / (1+r)^t
- `compound()` = PV × (1+r)^t

**Citations**:
- Klemperer (1996), Ch. 9: "Comparing Forest and Non-Forest Investments"
- Bettinger et al. (2017), Ch. 8
- Gregory, G.R. (1987). *Resource Economics for Foresters*. Wiley.

---

### Point 8: Stochastic Modeling — Monte Carlo & Risk Analysis

**Problem**: Forest investments span 20-80+ years. Pretending prices, yields, and rates are known with certainty is unrealistic.

**New functions**:

```r
monte_carlo(eval_fn, base_params, stochastic_params,
             n_sims = 10000, seed = NULL)
```
- General-purpose Monte Carlo engine
- `eval_fn`: any function from the package (npv, lev, irr, etc.)
- `stochastic_params`: named list where each element specifies `dist` (distribution) and parameters
  - e.g., `list(stumpage_price = list(dist = "lognormal", meanlog = 5.5, sdlog = 0.3))`
  - Supported distributions: "normal", "lognormal", "uniform", "triangular"
- Returns: S3 object of class `mc_forest` with simulated values and summary stats
- `plot.mc_forest()`: histogram with percentile markers and probability-of-loss annotation
- `summary.mc_forest()`: mean, sd, median, 5th/25th/75th/95th percentiles, P(loss), VaR

```r
stochastic_prices(n_periods, initial_price, process = c("gbm", "ou"),
                   drift = 0, volatility = 0.2,
                   mean_reversion_rate = NULL, long_run_mean = NULL,
                   n_paths = 1, seed = NULL)
```
- **GBM** (Geometric Brownian Motion): `dP = μPdt + σPdW`
  - Standard in financial modeling; prices can trend and cannot go negative
  - Parameters: drift (μ), volatility (σ)
- **OU** (Ornstein-Uhlenbeck / mean-reverting): `dP = κ(θ - P)dt + σdW`
  - More realistic for commodity prices — prices pulled back toward long-run mean
  - Parameters: mean_reversion_rate (κ), long_run_mean (θ), volatility (σ)
- Both processes available; user chooses via `process` parameter
- Returns: matrix of price paths (n_periods × n_paths)

```r
simulate_yield(yield_fn, ages, cv = 0.10, n_sims = 1000, seed = NULL)
```
- Adds stochastic variation to a deterministic yield curve
- Multiplicative lognormal noise: `Y_sim = Y_det × exp(N(0, σ²) - σ²/2)` (unbiased)
- `cv`: coefficient of variation (default 10%)
- Returns: matrix of simulated yields

```r
risk_metrics(simulations, threshold = 0)
```
- Value-at-Risk (VaR) at specified confidence levels
- Probability of loss (P(NPV < threshold))
- Expected shortfall (Conditional VaR)
- Returns: named list

**Citations**:
- Thomson, T.A. (1992). "Optimal forest rotation when stumpage prices follow a diffusion process." *Land Economics* 68(3): 329-342.
- Yoshimoto, A. & Shoji, I. (1998). "Searching for an optimal rotation age under stochastic log prices." *European Journal of Operational Research* 105(1): 100-112.
- Insley, M. (2002). "A real options approach to the valuation of a forestry investment." *Journal of Environmental Economics and Management* 44(3): 471-492.
- Plantinga, A.J. (1998). "The optimal timber rotation: an option value approach." *Forest Science* 44(2): 192-202.
- Brazee, R.J. & Mendelsohn, R. (1988). "Timber harvesting with fluctuating prices." *Forest Science* 34(2): 359-372.
- Dixit, A.K. & Pindyck, R.S. (1994). *Investment Under Uncertainty*. Princeton University Press. (General real options theory)

---

### Point 9: Sensitivity Analysis

**Problem**: No systematic way to explore how input assumptions affect outcomes. Sensitivity analysis is standard practice in forest investment analysis.

**New functions**:

```r
sensitivity_1way(eval_fn, param_name, param_range, base_params)
```
- Varies one parameter while holding all others at base case
- Returns: S3 object of class `sensitivity_1way` (data.frame with param_value, outcome)
- `plot.sensitivity_1way()`: line plot with base case marked

```r
sensitivity_2way(eval_fn, param_x, range_x, param_y, range_y, base_params)
```
- Varies two parameters across a grid
- Returns: S3 object with matrix of outcomes
- `plot.sensitivity_2way()`: filled contour / heatmap plot

```r
tornado_plot(eval_fn, param_ranges, base_params)
```
- `param_ranges`: named list of (low, high) pairs for each parameter
- Computes outcome at each extreme, ranks by impact width
- Returns: S3 object with ranked parameter impacts
- `plot.tornado()`: horizontal bar chart (tornado diagram)

```r
breakeven_analysis(eval_fn, param_name, base_params, target = 0)
```
- Finds the parameter value where the metric equals the target (default: NPV = 0)
- Uses `stats::uniroot()`
- Example: "What stumpage price makes this investment break even?"

**Citations**:
- Klemperer (1996), Ch. 10: "Risk and Uncertainty in Forest Management"
- Bettinger et al. (2017), Ch. 8, Section 8.4
- Wagner (2012), Ch. 10
- Cubbage, F.W., O'Laughlin, J., & Bullock, C.S. (1993). *Forest Resource Policy*. Wiley.

---

### Point 10: Comprehensive QMD Documentation

**Deliverable**: `vignettes/forest_economics_guide.qmd` — a complete practitioner and researcher reference.

**Structure**:

| Chapter | Title | Key Functions |
|---------|-------|---------------|
| 1 | Introduction & Installation | — |
| 2 | Time Value of Money | `discount()`, `compound()` |
| 3 | Net Present Value | `npv()`, `npv_schedule()`, `incremental_npv()` |
| 4 | Internal Rate of Return | `irr()`, `mirr()` |
| 5 | Land Expectation Value | `lev()`, `bare_land_value()`, `lev_schedule()` |
| 6 | Optimal Rotation Age | `optimal_rotation()`, `rotation_comparison()` |
| 7 | Timber Valuation | `stumpage_value()`, `create_price_table()`, `update_prices()` |
| 8 | Income Projection | `cash_flow_schedule()`, `project_income()`, `annualize()` |
| 9 | Stochastic Modeling | `monte_carlo()`, `stochastic_prices()`, `simulate_yield()`, `risk_metrics()` |
| 10 | Sensitivity Analysis | `sensitivity_1way()`, `sensitivity_2way()`, `tornado_plot()`, `breakeven_analysis()` |
| App A | Complete Worked Example | All functions integrated |
| App B | Function Reference | Quick-reference table |
| App C | Full Citation List | All academic references |

**Every function section includes**:
1. **Purpose** — What it computes and why
2. **Mathematical basis** — The formula, typeset in LaTeX
3. **Use cases** — Practitioner scenario + researcher scenario
4. **Best practices** — Recommended parameterization, workflow tips
5. **Limitations** — What the function does NOT do (taxes, externalities, etc.)
6. **Warnings** — Common mistakes and misinterpretations
7. **Examples** — Runnable code with realistic forestry data
8. **Citations** — Full references for the method

---

## Execution Order

```
Phase 1: Foundation (Point 1)
    ↓
Phase 2: Core Financial Functions (Points 2, 3, 4 — parallel)
    ↓
Phase 3: Applied Forest Economics (Points 5, 6, 7 — parallel)
    ↓
Phase 4: Risk & Uncertainty (Points 8, 9 — sequential, 8 before 9)
    ↓
Phase 5: Documentation (Point 10 — written incrementally, finalized last)
```

---

## Complete Citation List

### Textbooks
- Amacher, G.S., Ollikainen, M., & Koskela, E. (2009). *Economics of Forest Resources*. MIT Press.
- Bettinger, P., Boston, K., Siry, J.P., & Grebner, D.L. (2017). *Forest Management and Planning*. 2nd ed. Academic Press.
- Bullard, S.H. & Straka, T.J. (2011). *Basic Concepts in Forest Valuation and Investment Analysis*. 3rd ed.
- Cubbage, F.W., O'Laughlin, J., & Bullock, C.S. (1993). *Forest Resource Policy*. Wiley.
- Dixit, A.K. & Pindyck, R.S. (1994). *Investment Under Uncertainty*. Princeton University Press.
- Gregory, G.R. (1987). *Resource Economics for Foresters*. Wiley.
- Johansson, P.O. & Löfgren, K.G. (1985). *The Economics of Forestry and Natural Resources*. Blackwell.
- Klemperer, W.D. (1996). *Forest Resource Economics and Finance*. McGraw-Hill.
- Pearse, P.H. (1990). *Introduction to Forestry Economics*. UBC Press.
- Wagner, J.E. (2012). *Forestry Economics: A Managerial Approach*. Routledge.

### Seminal Papers
- Boulding, K.E. (1935). "The theory of a single investment." *Quarterly Journal of Economics* 49(3): 475-494.
- Brazee, R.J. (2001). "The Faustmann Formula: Fundamental to Forest Economics 150 Years After Publication." *Forest Science* 47(4): 441-442.
- Brazee, R.J. & Mendelsohn, R. (1988). "Timber harvesting with fluctuating prices." *Forest Science* 34(2): 359-372.
- Chang, S.J. (1998). "A generalized Faustmann model for the determination of optimal harvest age." *Canadian Journal of Forest Research* 28(5): 652-659.
- Faustmann, M. (1849). "Berechnung des Werthes welchen Waldboden sowie noch nicht haubare Holzbestände für die Waldwirthschaft besitzen." *Allgemeine Forst- und Jagd-Zeitung* 25: 441-455.
- Hazen, G.B. (2003). "A new perspective on multiple internal rates of return." *The Engineering Economist* 48(1): 31-51.
- Howard, T.E. (2016). "Stumpage price reporting." *Journal of Forestry* 114(2): 195-200.
- Insley, M. (2002). "A real options approach to the valuation of a forestry investment." *Journal of Environmental Economics and Management* 44(3): 471-492.
- Newman, D.H. (2002). "Forestry's golden rule and the development of the optimal forest rotation literature." *Journal of Forest Economics* 8(1): 5-27.
- Plantinga, A.J. (1998). "The optimal timber rotation: an option value approach." *Forest Science* 44(2): 192-202.
- Pressler, M.R. (1860). "Aus der Holzzuwachlehre." *Allgemeine Forst- und Jagd-Zeitung* 36: 173-191.
- Samuelson, P.A. (1976). "Economics of forestry in an evolving society." *Economic Inquiry* 14(4): 466-492.
- Thomson, T.A. (1992). "Optimal forest rotation when stumpage prices follow a diffusion process." *Land Economics* 68(3): 329-342.
- Yoshimoto, A. & Shoji, I. (1998). "Searching for an optimal rotation age under stochastic log prices." *European Journal of Operational Research* 105(1): 100-112.
