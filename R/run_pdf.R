# Assuming you already have:
#   stats  <- panel_stats("R/panel_life_NY001.csv", ...)
#   uout   <- make_failure_uchart(panel_life_NY001, ...)
#   u_plot <- uout$plot

write_panel_report(
  df      = ,
  stats   = stats,
  u_plot  = uout$plot,
  out_pdf = "NY001_Reliability_Report.pdf",
  warranty_years = 25
)
