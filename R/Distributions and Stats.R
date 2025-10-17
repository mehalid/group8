DataDistrubition <- function (panel_telemetry$actual_output_kW)
  
  #load packages     
library(ggplot2) # for visualization
library(dplyr) # for pipelines!
library(MASS) # for fitting distributions

meandata=mean(input)
mediandata= median(input)
rangedata=range(input)
sddata=sd(input)
variancedata=var(input)
sedata= sd(input)/(length(input))
diff= input - mean(input)
ndata= length(input)-1 
skewdata= sum(diff^3)/(ndata*sddata^3)
kurtdata= sum(diff^4)/(ndata*sddata^4)

# Generate Graphs
normaldis=rnorm(n=1000, mean= meandata, sd=sddata)
normaldis %>% hist()
poisdis = rpois(1000, lambda = meandata)
poisdis %>% hist()
ratedata= 1/mean(input)
expdis= rexp(n=1000, rate=ratedata)
expdis %>% hist()
shapedata= mean(input)^2/var(input)
rate= 1 / (var(input) / mean(input) )
gammadis= rgamma(1000, shape = shapedata, rate = rate)
gammadis %>% hist()

stats= input %>% fitdistr(densfun = "weibull")
shape_w <- stats$estimate[1]
scale_w <- stats$estimate[2]
weibulldis <- rweibull(n = 1000, shape = shape_w, scale = scale_w)
weibulldis %>% hist()

Datasim <- bind_rows(
  data.frame(x = input, type = "Observed"),
  data.frame(x = normaldis, type = "Normal"),
  data.frame(x = poisdis, type = "Poisson"),
  data.frame(x = gammadis, type = "Gamma"),
  data.frame(x = expdis, type = "Exponential"),
  data.frame(x = weibulldis, type = "Weibull")
)
g1 <- ggplot(data = Datasim, mapping = aes(x = x, fill = type)) +
  geom_density(alpha = 0.5) +
  labs(x = "Seawall Height (m)", y = "Density (Frequency)", 
       subtitle = "Distribution of best fits", fill = "Type")
# Then view it!
g1


