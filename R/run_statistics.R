# 1️⃣ Load your script (contains the panel_stats function)
source("R/statistics.R")

# 2️⃣ Run the function directly on your CSV file
stats <- panel_stats("R/panel_life_NY001.csv",
                     install_col = "install_date",
                     failure_col = "failure_date",
                     horizons = c(365, 730, 1825))  # 1, 2, and 5 years

library(dplyr)
library(ggplot2)
library(ggpubr)

# --- params from your fitted model ---
lambda <- stats$failure_rate_per_year   # per year
mttf   <- stats$mttf_days/365  #per year

# (quick unit tip) if you ever work in *years* instead:
# lambda_year <- stats$failure_rate_per_year
# then use t_years everywhere instead of t_days.

# --- make a smooth theoretical curve domain (in days) ---
t_max <- qexp(0.90, rate = lambda)                # ~99.9th percentile lifetime
curve_df <- tibble::tibble(
  t_years = seq(0, t_max, length.out = 100)
) %>%
  mutate(
    pdf  = dexp(t_years, rate = lambda),            # f(t)
    cdf  = pexp(t_years, rate = lambda),            # F(t)
    surv = exp(-lambda * t_years),                  # S(t)
    haz  = lambda                                  # h(t) (constant for exponential)
  )

# --- key percentiles & quick summary (days) ---
summ_tbl <- tibble::tibble(
  metric = c("MTTF (years)",
             "Median (years)", "P90 (years)", "P95 (years)"),
  value  = c(mttf,
             qexp(0.50, lambda),
             qexp(0.90, lambda),
             qexp(0.95, lambda))
)
print(summ_tbl)

# --- plots (all purely theoretical) ---
p_pdf <- ggplot(curve_df, aes(t_years, pdf)) +
  geom_line(linewidth = 1) +
  geom_area(fill = "burlywood")+
  labs(title = "Probability Density Function", subtitle = sprintf("λ = %.6f /year", lambda),
       x = "Time (years)", y = "Density") +
  theme_minimal(base_size = 12)

p_cdf <- ggplot(curve_df, aes(t_years, cdf)) +
  geom_line(linewidth = 1) +
  geom_area(fill = "brown")+
  labs(title = "Cumulative Distribution Function", x = "Time (years)", y = "Cumulative Probability") +
  theme_minimal(base_size = 12)

p_surv <- ggplot(curve_df, aes(t_years, surv)) +
  geom_line(linewidth = 1) +
  geom_area(fill = "chocolate")+
  labs(title = "Survival Function", x = "Time (years)", y = "Survival Probability") +
  theme_minimal(base_size = 12)

# arrange
ggpubr::ggarrange(p_pdf, p_cdf, p_surv, ncol = 3, labels = c("A","B","C"))