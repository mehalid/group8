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
  
  # look at inputted data
  stat_s = input %>% 
    # For each timestpe
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
    geom_hline(aes(yintercept = lower1), color = "pink", size = 3) +
    geom_hline(aes(yintercept = upper1), color = "pink", size = 3) +
    geom_hline(aes(yintercept = upper2), color = "purple", size = 3) +
    geom_hline(aes(yintercept = lower2), color = "purple", size = 3) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
    geom_line(size = 1) +
    geom_point(size = 5) +
    # Plot labels
    #geom_label(data = labels, mapping = aes(x = labels$time, y = value, label = text),  hjust = 1)  +
    geom_label(data = labels, aes(x = time, y = value, label = text), hjust = 1) +
    labs(x = "Time", y = "Average Efficiency",
         subtitle = "Average Chart")
    return(g1)
  # 
  # dn = function(n, reps = 1e4){
  #   # For 10,0000 reps
  #   tibble(rep = 1:reps) %>%
  #     # For each rep,
  #     group_by(rep) %>%
  #     # Simulate the ranges of n values
  #     summarize(r = rnorm(n = n, mean = 0, sd = 1) %>% range() %>% diff() %>% abs()) %>%
  #     ungroup() %>%
  #     # And calculate...
  #     summarize(
  #       # Mean range
  #       d2 = mean(r),
  #       # standard deviation of ranges
  #       d3 = sd(r),
  #       # and constants for obtaining lower and upper ci for rbar
  #       D3 = 1 - 3*(d3/d2), # sometimes written D3
  #       D4 = 1 + 3*(d3/d2), # sometimes written D4
  #       # Sometimes D3 goes negative; we need to bound it at zero
  #       D3 = if_else(D3 < 0, true = 0, false = D3) ) %>%
  #     return()
  # }
  # 
  # stat_i = input %>%
  #   group_by(Id) %>%
  #   summarize(r = input %>% range() %>% diff() %>% abs(),
  #             n_w = n()) # get subgroup size
  # # Let's get average within group range for temperature...
  # stat = stat_i %>%
  #   summarize(rbar = mean(r), # get Rbar
  #             n_w = unique(n_w)) # assuming constant subgroup size...
  # # Check it!
  # stat
  # 
  # #Finding bx factors 
  # bn = function(n, reps = 1e4){
  #   tibble(rep = 1:reps) %>%
  #     group_by(rep) %>%
  #     summarize(s = rnorm(n, mean = 0, sd = 1) %>% sd()) %>%
  #     summarize(b2 = mean(s), 
  #               b3 = sd(s),
  #               C4 = b2, # this is sometimes called C4
  #               A3 = 3 / (b2 * sqrt( n  )),
  #               B3 = 1 - 3 * b3/b2,
  #               B4 = 1 + 3 * b3/b2,
  #               # bound B3 at 0, since we can't have a standard deviation below 0
  #               B3 = if_else(B3 < 0, true = 0, false = B3)) %>%
  #     return()
  # }
  # 
  # stat_i = input %>%
  #   group_by(Id) %>%
  #   summarize(s = temp %>% sd(),
  #             n_w = n()) # get subgroup size
  # 
  # stat_i
  # 
  # # Let's get average within group range for temperature...
  # stat = stat_i %>%
  #   summarize(sbar = mean(s), # get Rbar
  #             n_w = unique(n_w)) # assuming constant subgroup size...
  # # Check it!
  # stat
  # 
  # mybstat = bn(n = stat$n_w)
  # # Calculate Control Limits
  # stat = stat %>%
  #   # Add our constants to the data.frame...
  #   mutate(mybstat) %>%
  #   # Calculate 3 sigma control limits
  #   mutate(sbar_lower = sbar * B3,
  #          sbar_upper = sbar * B4)
  # 
  # # Check it out!
  # stat %>%
  #   select(sbar, sbar_lower, sbar_upper)
  
  
}

g1= StatisticalProcessTest(telemetry_normal$actual_output_kW[1:1000],telemetry_normal$expected_output_kW[1:1000],telemetry_normal$timestep[1:1000])



