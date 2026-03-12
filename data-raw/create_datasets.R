# =========================================================================
# Create package datasets for forestvalues
# Run this script to regenerate data/*.rda files
# =========================================================================

# ---- 1. Southern Pine Plantation Yield Table ----
# Typical loblolly pine plantation in the U.S. Southeast
# Site index 60 (base age 25), planted at 600 trees/acre
pine_yields <- data.frame(
  age = seq(10, 50, by = 5),
  sawlog_vol = c(0, 0, 1.5, 4.0, 8.0, 13.0, 17.5, 21.0, 23.0),
  pulpwood_vol = c(8, 16, 22, 26, 28, 29, 29, 28, 27),
  sawlog_price = 254,
  pulpwood_price = 9,
  stringsAsFactors = FALSE
)
attr(pine_yields, "description") <- "Loblolly pine plantation yield table, SI 60"
attr(pine_yields, "units") <- c(sawlog = "mbf/acre", pulpwood = "tons/acre")
attr(pine_yields, "region") <- "U.S. Southeast"

# ---- 2. Northern Hardwood Selection Yield Table ----
# Uneven-aged northern hardwoods (sugar maple, yellow birch, beech)
# Managed on a 15-year cutting cycle
hardwood_yields <- data.frame(
  age = seq(10, 80, by = 10),
  sawtimber_vol = c(0, 0.5, 1.5, 3.5, 6.0, 8.5, 10.0, 11.0),
  cordwood_vol = c(3, 6, 10, 14, 17, 19, 20, 20),
  sawtimber_price = 380,
  cordwood_price = 12,
  stringsAsFactors = FALSE
)
attr(hardwood_yields, "description") <- "Northern hardwood uneven-aged yield table"
attr(hardwood_yields, "units") <- c(sawtimber = "mbf/acre", cordwood = "cords/acre")
attr(hardwood_yields, "region") <- "U.S. Northeast (Maine/NH/VT)"

# ---- 3. Management Schedule: Southern Pine ----
# Full management schedule for a loblolly pine plantation
pine_schedule <- data.frame(
  name = c("Site Prep & Planting", "Herbicide Release", "Property Tax",
           "Insurance", "Pre-commercial Thin", "First Thinning",
           "Final Harvest"),
  amount = c(-750, -125, -12, -5, -200, 2500, 8500),
  year = c(0, 1, 0, 0, 8, 20, 35),
  frequency = c("once", "once", "annual", "annual", "once", "once", "once"),
  period_length = c(NA, NA, NA, NA, NA, NA, NA),
  stringsAsFactors = FALSE
)

# ---- 4. Timber Sale Data for Tax Examples ----
# Three representative timber sales for tax analysis
timber_sales <- data.frame(
  sale_id = c("TS-2020-001", "TS-2023-001", "TS-2025-001"),
  year = c(2020, 2023, 2025),
  gross_revenue = c(45000, 78000, 125000),
  cost_basis = c(8000, 15000, 22000),
  volume_mbf = c(180, 310, 500),
  holding_period_years = c(25, 30, 18),
  species = c("Loblolly Pine", "Mixed Pine", "Longleaf Pine"),
  state = c("Georgia", "Alabama", "Mississippi"),
  stringsAsFactors = FALSE
)

# ---- 5. Multi-Harvest Depletion Example ----
# Simulates a 1,000-acre tract harvested over 15 years
pine_harvests <- data.frame(
  year = c(2018, 2021, 2024, 2027, 2033),
  volume = c(2000, 2500, 3000, 1500, 4000),
  revenue = c(480000, 650000, 810000, 420000, 1200000),
  acres = c(200, 250, 300, 150, 100),
  stringsAsFactors = FALSE
)
attr(pine_harvests, "initial_basis") <- 800000
attr(pine_harvests, "initial_volume") <- 13000
attr(pine_harvests, "description") <- "1,000-acre pine tract harvested in 5 entries"

# ---- 6. Comparable Timberland Sales ----
# Recent sales for the sales comparison approach
comparable_sales <- data.frame(
  sale_id = c("CS-001", "CS-002", "CS-003", "CS-004"),
  sale_date = as.Date(c("2024-03-15", "2024-07-22", "2023-11-01", "2025-01-10")),
  price_per_acre = c(2100, 1850, 2400, 2250),
  acres = c(450, 1200, 280, 650),
  timber_vol_mbf = c(4.2, 2.8, 5.5, 3.9),
  site_index = c(65, 55, 70, 62),
  road_access = c("good", "poor", "excellent", "good"),
  county = c("Baldwin, AL", "Ware, GA", "Berkeley, SC", "Jones, MS"),
  stringsAsFactors = FALSE
)

# ---- 7. Discount Rate Components (historical benchmarks) ----
rate_components <- data.frame(
  component = c("Risk-free rate", "Inflation premium", "Illiquidity premium",
                 "Management risk", "Market/timber risk", "Property-specific"),
  typical_low = c(0.03, 0.015, 0.005, 0.003, 0.005, 0.00),
  typical_mid = c(0.04, 0.025, 0.010, 0.005, 0.010, 0.005),
  typical_high = c(0.05, 0.035, 0.020, 0.010, 0.020, 0.015),
  description = c(
    "10-year U.S. Treasury yield",
    "Expected CPI inflation rate",
    "Premium for non-traded, hard-to-sell asset",
    "Cost/risk of professional forest management",
    "Timber price volatility and market risk",
    "Property-level factors (title, access, environmental)"),
  stringsAsFactors = FALSE
)

# ---- Save all datasets ----
usethis_save <- function(obj, name) {
  assign(name, obj)
  save(list = name,
       file = file.path("data", paste0(name, ".rda")),
       compress = "xz")
}

usethis_save(pine_yields, "pine_yields")
usethis_save(hardwood_yields, "hardwood_yields")
usethis_save(pine_schedule, "pine_schedule")
usethis_save(timber_sales, "timber_sales")
usethis_save(pine_harvests, "pine_harvests")
usethis_save(comparable_sales, "comparable_sales")
usethis_save(rate_components, "rate_components")

cat("All datasets saved to data/\n")
