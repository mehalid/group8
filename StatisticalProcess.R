StatisticalProcess <- function (input, input)
#Load Packages
library(tidyverse)
library(viridis)
library(ggpubr)
library(moments)

# look at inputted data
input %>% glimpse() 
  
# Withingroupstats
stat_s = input %>% 
  # For each timestpe
  group_by(time) %>%
  # Calculate these statistics of interest!
  summarize(
    # within-group mean
    xbar = mean(temp),
    # within-group range
    r = max(temp) - min(temp),
    # within-group standard deviation
    sd = sd(temp),
    # within-group sample size
    nw = n(),
    # Degrees of freedom within groups
    df = nw - 1) %>%
  mutate(
    # these are equivalent
    sigma_s = sqrt( sum(df * sd^2) / sum(df) ),
    sigma_s = sqrt(mean(sd^2)), 
    # And get standard error (in a way that retains each subgroup's sample size!)
    se = sigma_s / sqrt(nw),
    # Calculate 6-sigma control limits!
    upper = mean(xbar) + 3*se,
    lower = mean(xbar) - 3*se)
# Check it!
stat_s %>% head(3)

# Between Group stats
stat_t = stat_s %>%
  summarize(
    xbbar = mean(xbar),
    rbar = mean(r),
    sdbar = mean(sd),
    # We can also recalculate sigma_short here too
    sigma_s = sqrt( mean(sd^2) ),
    # Or we can calculate overall standard deviation 
    sigma_t = sd(input$temp) )

#Create a average chart
# Let's extract some labels
labels = stat_s %>%
  summarize(
    time = max(time),
    type = c("xbbar",  "upper", "lower"),
    name = c("mean", "+3 s", "-3 s"),
    value = c(mean(xbar), unique(upper), unique(lower)),
    value = round(value, 2),
    text = paste(name, value, sep = " = "))

stat_s %>%
  ggplot(mapping = aes(x = time, y = xbar)) +
  geom_hline(mapping = aes(yintercept = mean(xbar)), color = "lightgrey", size = 3) +
  geom_ribbon(mapping = aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
  geom_line(size = 1) +
  geom_point(size = 5) +
  # Plot labels
  geom_label(data = labels, mapping = aes(x = time, y = value, label = text),  hjust = 1)  +
  labs(x = "Time (Subgroups)", y = "Average",
       subtitle = "Average and Standard Deviation Chart")

