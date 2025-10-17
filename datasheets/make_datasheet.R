library(dplyr)

set.seed(42)

# ---- Basic parameters ----
farms <- c("NY001", "NY002")
n_panels <- 100

# ---- Helper function to generate panel life info ----
generate_farm_data <- function(farm_id, mean_life, sd_life, fail_type_probs, maint_delay_mean) {
  data.frame(
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
  ) -> df
  
  # Simulate lifetimes and failures
  lifetimes_years <- rnorm(n_panels, mean = mean_life, sd = sd_life)
  df$failure_date <- df$install_date + round(lifetimes_years * 365)
  
  cutoff_date <- as.Date("2025-01-01")
  df$event <- ifelse(df$failure_date <= cutoff_date, 1, 0)
  df$failure_date[df$failure_date > cutoff_date] <- NA
  
  df
}

# ---- Generate the two farms ----
farm_normal <- generate_farm_data(
  farm_id = "NY001",
  mean_life = 28, sd_life = 3,
  fail_type_probs = c(0.3, 0.2, 0.3, 0.2),  # balanced failure causes
  maint_delay_mean = 365                     # ~1 year between maintenance
)

farm_bad <- generate_farm_data(
  farm_id = "NY002",
  mean_life = 20, sd_life = 4,
  fail_type_probs = c(0.1, 0.7, 0.1, 0.1),  # mostly Wiring failures
  maint_delay_mean = 800                     # infrequent maintenance
)

panel_life <- rbind(farm_normal, farm_bad)

# ---- Panel telemetry (performance data) ----
library(dplyr)

telemetry_days <- seq(as.Date("2020-01-01"), as.Date("2025-01-01"), by = "day")

panel_telemetry <- expand.grid(
  panel_id = sample(panel_life$panel_id, 10), # subset for demo
  timestep = telemetry_days
) %>%
  left_join(panel_life[, c("panel_id", "system_id")], by = "panel_id") %>%
  mutate(
    ambient_temp_C = runif(n(), 0, 35),
    solar_irradiance_Wm2 = runif(n(), 200, 1000),
    expected_output_kW = 0.5 * (solar_irradiance_Wm2 / 1000),
    degradation_factor = case_when(
      system_id == "NY001" ~ 1 - 0.005 * ((as.numeric(timestep - min(timestep)))/365),  # 0.5%/yr
      system_id == "NY002" ~ 1 - 0.015 * ((as.numeric(timestep - min(timestep)))/365)   # 1.5%/yr
    ),
    actual_output_kW = expected_output_kW * degradation_factor *
      ifelse(system_id == "NY002", runif(n(), 0.85, 1.0), runif(n(), 0.9, 1.0)),
    adverse_condition = ifelse(system_id == "NY002",
                               sample(c(0,1), n(), replace=TRUE, prob=c(0.9,0.1)),
                               sample(c(0,1), n(), replace=TRUE, prob=c(0.95,0.05)))
  )

# ---- Save datasets ----
write.csv(panel_life, "panel_life.csv", row.names = FALSE)
write.csv(panel_telemetry, "panel_telemetry.csv", row.names = FALSE)
