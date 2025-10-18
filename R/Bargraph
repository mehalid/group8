Bargraph <- function (input) { 
# Load packages
library(tidyverse)
library(ggplot2)

# Summarize number of failures by type
failure_summary <- input %>%
  filter(!is.na(failure_type)) %>%       # remove NA failure types
  group_by(failure_type) %>%
  summarize(Number_of_Failures = n()) %>%
  arrange(desc(Number_of_Failures))

# View summary table
print(failure_summary)

# Create bar chart
gbar=ggplot(failure_summary, aes(x = reorder(failure_type, -Number_of_Failures),
                            y = Number_of_Failures,
                            fill = failure_type)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = Number_of_Failures),
            vjust = -0.4, size = 4) +
  scale_fill_viridis_d(option = "plasma") +
  labs(title = "Number of Solar Panels Failed by Failure Type",
       x = "Failure Type",
       y = "Number of Failures") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
return(gbar)
}

Bargraph(farm_bad)
