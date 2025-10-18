StatisticalProcessTest <- function (input1, input2, input3) {
  #Load Packages
  library(tidyverse)
  library(viridis)
  library(ggpubr)
  library(moments)
  
  input= tibble( 
    actual=input1,
    expected=input2,
    Id=input3,
    ratio = input1 / input2
    )
  
  # look at input data
  stat_s = input %>% 
    # For each time step
    group_by(Id) %>%
    # Calculate these statistics of interest!
    summarize(
      # within-group mean
      xbar = mean(ratio),
      # within-group range
      r = max(ratio) - min(ratio),
      # within-group standard deviation
      sd = sd(ratio),
      # within-group sample size
      nw = n(),
      # Degrees of freedom within groups
      df = nw - 1) %>%
    # Last, we'll calculate sigma_short (within-group variance)
    # We're going to calculate the short-term variation parameter sigma_s (sigma_short)
    # by taking the square root of the average of the standard deviation
    # Essentially, we're weakening the impact of any special cause variation
    # so that our sigma is mostly representative of common cause (within-group) variation
    mutate(
      # these are equivalent
      sigma_s = sqrt( sum(df * sd^2) / sum(df) ),
      sigma_s = sqrt(mean(sd^2)), 
      # And get standard error (in a way that retains each subgroup's sample size!)
      se = sigma_s / sqrt(nw),
      # Calculate 6-sigma control limits!
      upper1= mean(xbar) + 2*se,
      lower1= mean(xbar) -2*se,
      upper2=mean(xbar)+se,
      lower2=mean(xbar)-se,
      upper = mean(xbar) + 3*se,
      lower = mean(xbar) - 3*se)
  
  # Calculate trend slope (degradation rate)
  trend_model=lm(xbar ~ Id, data = stat_s)
  slope=as.numeric(coef(trend_model)[2])  # slope per timestep

  
  # Check it!
  stat_s %>% head(3)
  #Finding dx factors 
  # Let's calculate our own d function
  
  
  labels = stat_s %>%
    summarize(
      time = c(max(Id), max(Id), max(Id), max(Id), max(Id), max(Id), max(Id)),
      type = c("xbbar",  "upper", "lower", "upper1", "lower1", "upper2", "lower2"),
      name = c("mean", "+3 s", "-3 s", "+2 s", "-2 s", "+1 s", "-1 s"),
      value = c(mean(xbar), unique(upper), unique(lower), unique(upper1), unique(lower1), unique(upper2), unique(lower2)),
      text = paste(name, value, sep = " = "))
  
  g1 = stat_s %>%
    ggplot(mapping = aes(x = Id, y = xbar)) +
    geom_hline(aes(yintercept = mean(xbar)), color = "lightgrey", size = 3) +
    geom_hline(aes(yintercept = lower1), color = "burlywood", size = 3) +
    geom_hline(aes(yintercept = upper1), color = "burlywood", size = 3) +
    geom_hline(aes(yintercept = upper2), color = "brown", size = 3) +
    geom_hline(aes(yintercept = lower2), color = "brown", size = 3) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
    geom_line(size = 1) +
    geom_point(size = 5) +
    # Plot labels
    #geom_label(data = labels, mapping = aes(x = labels$time, y = value, label = text),  hjust = 1)  +
    geom_label(data = labels, aes(x = time, y = value, label = text), hjust = 1) +
    labs(x = "Time", y = "Average Efficiency",
         title = "Control Chart") +
            theme_minimal()+
            theme(
              plot.title = element_text(hjust = 0.5))
  return(list(plot = g1, slope = slope))
}

var = StatisticalProcessTest(telemetry_normal$actual_output_kW[1:1000],telemetry_normal$expected_output_kW[1:1000],telemetry_normal$timestep[1:1000])
var$plot
var$slope
