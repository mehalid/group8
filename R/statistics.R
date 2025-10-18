# statistics.R
# Minimal reliability stats for solar panels using ONLY these libs:
library(dplyr)   # data wrangling
library(readr)   # read_csv
library(ggplot2) # optional quick plot
library(ggpubr)  # (loaded per your spec)
library(moments) # (loaded per your spec)

# ---- Helper: robust date parsing without extra packages ----
.parse_date_vec <- function(x) {
  if (inherits(x, "Date")) return(x)
  d <- suppressWarnings(as.Date(x))                           # ISO, e.g., 2000-02-18
  if (all(is.na(d))) d <- suppressWarnings(as.Date(x, "%m/%d/%Y"))
  if (all(is.na(d))) d <- suppressWarnings(as.Date(x, "%d/%m/%Y"))
  if (all(is.na(d)) && is.numeric(x)) d <- as.Date(x, origin = "1899-12-30") # Excel serials
  d
}

# ---- Core: compute MTTF, failure rate (λ), and exponential survival S(t) ----
# Uses ONLY rows with known failure_date (no censoring math).
panel_stats <- function(csv_path,
                        install_col = "install_date",
                        failure_col = "failure_date",
                        horizons    = c(365, 730, 1825)) {
  
  dat_raw <- readr::read_csv(csv_path, show_col_types = FALSE)

  
  dat <- dat_raw %>%
    mutate(
      .install = .parse_date_vec(.data[[install_col]]),
      .failure = .parse_date_vec(.data[[failure_col]])
    ) %>%
    filter(!is.na(.install)) %>%                                # need install date
    filter(is.na(.failure) | .failure >= .install)              # keep valid rows
  
  n_total  <- nrow(dat)
  n_failed <- sum(!is.na(dat$.failure))
  
  # Time-to-failure for *failed* rows only
  ttf_days <- dat %>%
    filter(!is.na(.failure)) %>%
    transmute(ttf = as.numeric(.failure - .install)) %>%
    pull(ttf)
  
  # if (length(ttf_days) == 0) {
  #   return(list(
  #     n_total = n_total,
  #     n_failed = 0,
  #     mttf_days = NA_real_,
  #     failure_rate_per_day = NA_real_,
  #     failure_rate_per_year = NA_real_,
  #     survival_fn = function(t_days) rep(NA_real_, length(t_days)),
  #     survival_at = data.frame(time_days = horizons, survival = NA_real_)
  #   ))
  # }
  
  mttf_days <- mean(ttf_days, na.rm = TRUE)
  lambda    <- 1 / mttf_days
  
  survival_fn <- function(t_days) {
    #if (is.na(lambda)) return(rep(NA_real_, length(t_days)))
    exp(-lambda * t_days)
  }
  
  survival_at <- data.frame(
    time_days = horizons,
    survival  = survival_fn(horizons)
  )
  
  list(
    n_total = n_total,
    n_failed = n_failed,
    mttf_days = mttf_days,
    failure_rate_per_day = lambda,
    failure_rate_per_year = if (is.na(lambda)) NA_real_ else lambda * 365,
    survival_fn = survival_fn,
    survival_at = survival_at
  )
}

# ---- Optional quick plot of exponential survival curve S(t) ----
plot_exponential_survival <- function(stats, max_days = 3650) {
  t <- seq(0, max_days, by = 30)
  df <- data.frame(time_days = t, survival = stats$survival_fn(t))
  ggplot(df, aes(time_days, survival)) +
    geom_line(size = 1) +
    labs(title = "Exponential Survival Curve (S(t) = e^{-λ t})",
         subtitle = sprintf("λ = %.6f per day | MTTF = %.1f days",
                            stats$failure_rate_per_day, stats$mttf_days),
         x = "Time (days)", y = "Survival probability") +
    theme_minimal(base_size = 12)
}

# ---------------------- Example usage ----------------------
# stats <- panel_stats("your_panels.csv",
#                      install_col = "install_date",
#                      failure_col = "failure_date",
#                      horizons = c(365, 730, 1825))
# print(stats$mttf_days)
# print(stats$failure_rate_per_day)
# print(stats$survival_at)
# plot_exponential_survival(stats, max_days = 3650)
