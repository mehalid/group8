library(dplyr)
library(ggplot2)
library(ggpubr)

#---- Helper: build theoretical life plots from your `stats` (YEARS) ----
build_life_plots <- function(stats, warranty_years = 25) {
  mttf_years <- stats$mttf_days / 365
  lambda_yr  <- stats$failure_rate_per_year  # per year
  
  # Domain up to ~99.9th percentile
  t_max <- qexp(0.999, rate = lambda_yr)
  curve_df <- data.frame(
    t_years = seq(0, t_max, length.out = 600)
  ) %>%
    mutate(
      pdf  = dexp(t_years, rate = lambda_yr),
      cdf  = pexp(t_years, rate = lambda_yr),
      surv = exp(-lambda_yr * t_years)
    )
  
  p_pdf <- ggplot(curve_df, aes(t_years, pdf)) +
    geom_line(linewidth = 1) +
    labs(title = "Exponential PDF",
         subtitle = sprintf("λ = %.5f per year | MTTF ≈ %.1f years", lambda_yr, mttf_years),
         x = "Time (years)", y = "Density") +
    theme_minimal(base_size = 12)
  
  p_cdf <- ggplot(curve_df, aes(t_years, cdf)) +
    geom_line(linewidth = 1) +
    labs(title = "CDF: P(TTF ≤ t)",
         x = "Time (years)", y = "Cumulative probability") +
    theme_minimal(base_size = 12)
  
  p_surv <- ggplot(curve_df, aes(t_years, surv)) +
    geom_line(linewidth = 1) +
    geom_vline(xintercept = warranty_years, linetype = "dashed") +
    annotate("text", x = warranty_years, y = 0.05, angle = 90, vjust = -0.5,
             label = sprintf("Warranty: %d yrs", warranty_years)) +
    labs(title = "Survival S(t) = e^{-λ t}",
         x = "Time (years)", y = "Survival probability") +
    theme_minimal(base_size = 12)
  
  list(pdf = p_pdf, cdf = p_cdf, surv = p_surv)
}

#---- Helper: small summary table (as a ggplot table) ----
build_summary_table_plot <- function(stats) {
  mttf_years <- stats$mttf_days / 365
  lambda_yr  <- stats$failure_rate_per_year
  n_total    <- if (!is.null(stats$n_total))  stats$n_total  else NA_integer_
  n_failed   <- if (!is.null(stats$n_failed)) stats$n_failed else NA_integer_
  
  surv10 <- exp(-lambda_yr * 10)
  surv20 <- exp(-lambda_yr * 20)
  surv25 <- exp(-lambda_yr * 25)
  
  tbl <- data.frame(
    Metric = c("Total panels", "Failed panels",
               "MTTF (years)", "Failure rate (per year)",
               "Survival @ 10y", "Survival @ 20y", "Survival @ 25y"),
    Value  = c(
      n_total,
      n_failed,
      sprintf("%.1f", mttf_years),
      sprintf("%.4f", lambda_yr),
      sprintf("%.1f%%", 100 * surv10),
      sprintf("%.1f%%", 100 * surv20),
      sprintf("%.1f%%", 100 * surv25)
    ),
    stringsAsFactors = FALSE
  )
  
  ggpubr::ggtexttable(tbl, rows = NULL, theme = ttheme("mOrange"))
}

#---- Helper: short narrative paragraph (as an annotated figure) ----
build_narrative_plot <- function(stats) {
  mttf_years <- stats$mttf_days / 365
  lambda_yr  <- stats$failure_rate_per_year
  n_total    <- if (!is.null(stats$n_total))  stats$n_total  else NA_integer_
  n_failed   <- if (!is.null(stats$n_failed)) stats$n_failed else NA_integer_
  surv10 <- 100 * exp(-lambda_yr * 10)
  surv25 <- 100 * exp(-lambda_yr * 25)
  
  text <- paste0(
    "Summary\n\n",
    "We analyzed reliability using an exponential model fit to observed failures.\n",
    "Estimated MTTF is ~", sprintf("%.1f", mttf_years), " years (λ ≈ ",
    sprintf("%.4f", lambda_yr), " per year). With ", n_total, " panels and ",
    n_failed, " observed failures, the model projects ~", sprintf("%.1f", surv10),
    "% survival at 10 years and ~", sprintf("%.1f", surv25),
    "% at 25 years. Results support planning maintenance and warranty strategies\n",
    "based on stable, memoryless failure behavior characteristic of random faults."
  )
  
  ggpubr::annotate_figure(ggplot() + theme_void(),
                          top = ggpubr::text_grob("Solar Panel Reliability Report",
                                                  face = "bold", size = 16),
                          bottom = NULL,
                          left   = NULL,
                          right  = NULL,
                          fig.lab = NULL) %>%
    ggpubr::ggparagraph(text = text, face = "plain", size = 11, color = "black")
}

#---- Main: write a multi-page PDF report ----
# - df: your panel data.frame (for control chart if your function needs it)
# - u_plot: the control chart ggplot you already generate (pass it in)
# - stats: list from panel_stats(...)
# - out_pdf: file path to save
write_panel_report <- function(df, stats, u_plot, out_pdf = "panel_report.pdf",
                               warranty_years = 25) {
  # Build life plots & summary blocks
  life_plots <- build_life_plots(stats, warranty_years = warranty_years)
  tbl_plot   <- build_summary_table_plot(stats)
  narrative  <- build_narrative_plot(stats)
  
  # Page 1: Title + narrative + summary table
  page1 <- ggpubr::ggarrange(
    narrative,
    tbl_plot,
    ncol = 1, heights = c(2, 1)
  )
  
  # Page 2: Theoretical life distributions (PDF, CDF, Survival)
  page2 <- ggpubr::ggarrange(
    life_plots$pdf, life_plots$cdf, life_plots$surv,
    ncol = 3, labels = c("A", "B", "C")
  )
  
  # Page 3: Control chart
  page3 <- u_plot + ggtitle("Control Chart (Failures per 1,000 panel-months)")
  
  # ---- Write the multi-page PDF ----
  grDevices::pdf(out_pdf, width = 11, height = 8.5) # Landscape Letter
  print(page1)
  print(page2)
  print(page3)
  grDevices::dev.off()
  
  message(sprintf("Wrote report: %s", normalizePath(out_pdf)))
}
