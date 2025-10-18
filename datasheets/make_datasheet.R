# library(dplyr)
# 
# set.seed(42)
# 
# # ---- Basic parameters ----
# farms <- c("NY001", "NY002")
# n_panels <- 100
# 
# # ---- Helper function to generate panel life info ----
# generate_farm_data <- function(farm_id, mean_life, sd_life, fail_type_probs, maint_delay_mean) {
#   data.frame(
#     panel_id = paste0(farm_id, "_P", sprintf("%03d", 1:n_panels)),
#     system_id = farm_id,
#     install_date = as.Date("2000-01-01") + sample(0:200, n_panels, replace = TRUE),
#     component_type = "Panel",
#     failure_type = sample(
#       c("Crack", "Wiring", "Delamination", "Wear"),
#       n_panels,
#       replace = TRUE,
#       prob = fail_type_probs
#     ),
#     last_maintenance_date = as.Date("2020-01-01") +
#       sample(0:(maint_delay_mean * 2), n_panels, replace = TRUE)
#   ) -> df
#   
#   # Simulate lifetimes and failures
#   lifetimes_years <- rnorm(n_panels, mean = mean_life, sd = sd_life)
#   df$failure_date <- df$install_date + round(lifetimes_years * 365)
#   
#   cutoff_date <- as.Date("2025-01-01")
#   df$event <- ifelse(df$failure_date <= cutoff_date, 1, 0)
#   df$failure_date[df$failure_date > cutoff_date] <- NA
#   
#   df
# }
# 
# # ---- Generate the two farms ----
# farm_normal <- generate_farm_data(
#   farm_id = "NY001",
#   mean_life = 28, sd_life = 3,
#   fail_type_probs = c(0.3, 0.2, 0.3, 0.2),  # balanced failure causes
#   maint_delay_mean = 365                     # ~1 year between maintenance
# )
# 
# farm_bad <- generate_farm_data(
#   farm_id = "NY002",
#   mean_life = 20, sd_life = 4,
#   fail_type_probs = c(0.1, 0.7, 0.1, 0.1),  # mostly Wiring failures
#   maint_delay_mean = 800                     # infrequent maintenance
# )
# 
# panel_life <- rbind(farm_normal, farm_bad)
# 
# # ---- Panel telemetry (performance data) ----
# library(dplyr)
# 
# telemetry_days <- seq(as.Date("2020-01-01"), as.Date("2025-01-01"), by = "day")
# 
# panel_telemetry <- expand.grid(
#   panel_id = sample(panel_life$panel_id, 10), # subset for demo
#   timestep = telemetry_days
# ) %>%
#   left_join(panel_life[, c("panel_id", "system_id")], by = "panel_id") %>%
#   mutate(
#     ambient_temp_C = runif(n(), 0, 35),
#     solar_irradiance_Wm2 = runif(n(), 200, 1000),
#     expected_output_kW = 0.5 * (solar_irradiance_Wm2 / 1000),
#     degradation_factor = case_when(
#       system_id == "NY001" ~ 1 - 0.005 * ((as.numeric(timestep - min(timestep)))/365),  # 0.5%/yr
#       system_id == "NY002" ~ 1 - 0.015 * ((as.numeric(timestep - min(timestep)))/365)   # 1.5%/yr
#     ),
#     actual_output_kW = expected_output_kW * degradation_factor *
#       ifelse(system_id == "NY002", runif(n(), 0.85, 1.0), runif(n(), 0.9, 1.0)),
#     adverse_condition = ifelse(system_id == "NY002",
#                                sample(c(0,1), n(), replace=TRUE, prob=c(0.9,0.1)),
#                                sample(c(0,1), n(), replace=TRUE, prob=c(0.95,0.05)))
#   )
# 
# # ---- Save datasets ----
# write.csv(panel_life, "panel_life.csv", row.names = FALSE)
# write.csv(panel_telemetry, "panel_telemetry.csv", row.names = FALSE)

library(dplyr)

set.seed(42)

# ---- Basic parameters ----
n_panels <- 100
n_telemetry_panels <- 10
n_telemetry_points <- 100

# ---- Helper function to generate panel life info ----
generate_farm_data <- function(farm_id, mean_life, sd_life, fail_type_probs, maint_delay_mean) {
  df <- data.frame(
    panel_id = paste0(farm_id, "_P", sprintf("%03d", 1:n_panels)),
    system_id = farm_id,
    install_date = as.Date("2000-01-01") + sample(0:200, n_panels, replace = TRUE),
    component_type = "Panel",
    failure_type = sample(
      c("Crack", "Wiring", "Delamination", "Wear"),
      n_panels,
      replace = TRUE,
      prob = fail_type_probs
    ),
    last_maintenance_date = as.Date("2020-01-01") +
      sample(0:(maint_delay_mean * 2), n_panels, replace = TRUE)
  )
  
  lifetimes_years <- rnorm(n_panels, mean = mean_life, sd = sd_life)
  df$failure_date <- df$install_date + round(lifetimes_years * 365)
  
  cutoff_date <- as.Date("2025-01-01")
  df$event <- ifelse(df$failure_date <= cutoff_date, 1, 0)
  df$failure_date[df$failure_date > cutoff_date] <- NA
  
  return(df)
}

# ---- Function to generate telemetry for one farm ----
generate_farm_telemetry <- function(farm_df, farm_id, deg_rate, irr_factor_range, noise_range) {
  # select a subset of panels
  sample_panels <- sample(farm_df$panel_id, n_telemetry_panels)
  
  # select ~1000 dates evenly spaced from 2020–2025
  full_days <- seq(as.Date("2020-01-01"), as.Date("2025-01-01"), by = "day")
  sample_days <- full_days[seq(1, length(full_days), length.out = n_telemetry_points)]
  
  telemetry <- expand.grid(
    panel_id = sample_panels,
    timestep = sample_days
  ) %>%
    mutate(
      ambient_temp_C = runif(n(), 0, 35),
      solar_irradiance_Wm2 = runif(n(), 200, 1000),
      expected_output_kW = 0.5 * (solar_irradiance_Wm2 / 1000),
      degradation_factor = 1 - deg_rate * ((as.numeric(timestep - min(timestep)))/365),
      actual_output_kW = expected_output_kW * degradation_factor * runif(n(), noise_range[1], noise_range[2]),
      adverse_condition = sample(c(0,1), n(), replace=TRUE, prob=c(0.95,0.05))
    )
  
  telemetry <- left_join(telemetry, farm_df[, c("panel_id", "system_id")], by = "panel_id")
  return(telemetry)
}

# ---- Generate the two farms ----
farm_normal <- generate_farm_data(
  farm_id = "NY001",
  mean_life = 28, sd_life = 3,
  fail_type_probs = c(0.3, 0.2, 0.3, 0.2),  # balanced causes
  maint_delay_mean = 365                     # ~1 year maintenance
)

farm_bad <- generate_farm_data(
  farm_id = "NY002",
  mean_life = 20, sd_life = 4,
  fail_type_probs = c(0.1, 0.7, 0.1, 0.1),  # mostly Wiring
  maint_delay_mean = 800                     # infrequent maintenance
)

# ---- Telemetry for each farm ----
telemetry_normal <- generate_farm_telemetry(
  farm_df = farm_normal,
  farm_id = "NY001",
  deg_rate = 0.005,                 # 0.5%/yr degradation
  irr_factor_range = c(200, 1000),
  noise_range = c(0.9, 1.0)
)

telemetry_bad <- generate_farm_telemetry(
  farm_df = farm_bad,
  farm_id = "NY002",
  deg_rate = 0.015,                 # 1.5%/yr degradation
  irr_factor_range = c(200, 1000),
  noise_range = c(0.85, 1.0)
)

# ---- Save datasets ----
write.csv(farm_normal, "panel_life_NY001.csv", row.names = FALSE)
write.csv(farm_bad, "panel_life_NY002.csv", row.names = FALSE)
write.csv(telemetry_normal, "panel_telemetry_NY001.csv", row.names = FALSE)
write.csv(telemetry_bad, "panel_telemetry_NY002.csv", row.names = FALSE)

cat("Saved panel_life_NY001.csv, panel_life_NY002.csv, panel_telemetry_NY001.csv, and panel_telemetry_NY002.csv\n")

