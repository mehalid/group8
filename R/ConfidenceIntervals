ConfidenceIntervals <- function (input, input)
 #load packages
library(tidyverse)
library(viridis)

# look at inputted data
input %>% glimpse() 
  
# index functions 
# Capability Index (for centered, normal data)
cp = function(sigma_s, upper, lower){  abs(upper - lower) / (6*sigma_s)   }

# Process Performance Index (for centered, normal data)
pp = function(sigma_t, upper, lower){  abs(upper - lower) / (6*sigma_t)   }

# Capability Index (for skewed, uncentered data)
cpk = function(mu, sigma_s, lower = NULL, upper = NULL){
  if(!is.null(lower)){
    a = abs(mu - lower) / (3 * sigma_s)
  }
  if(!is.null(upper)){
    b = abs(upper - mu) /  (3 * sigma_s)
  }
  # We can also write if else statements like this
  # If we got both stats, return the min!
  if(!is.null(lower) & !is.null(upper)){
    min(a,b) %>% return()
    
    # If we got just the upper stat, return b (for upper)
  }else if(is.null(lower)){ return(b) 
    
    # If we got just the lower stat, return a (for lower)
  }else if(is.null(upper)){ return(a) }
}


# Process Performance Index (for skewed, uncentered data)
ppk = function(mu, sigma_t, lower = NULL, upper = NULL){
  if(!is.null(lower)){
    a = abs(mu - lower) / (3 * sigma_t)
  }
  if(!is.null(upper)){
    b = abs(upper - mu) /  (3 * sigma_t)
  }
  # We can also write if else statements like this
  # If we got both stats, return the min!
  if(!is.null(lower) & !is.null(upper)){
    min(a,b) %>% return()
    
    # If we got just the upper stat, return b (for upper)
  }else if(is.null(lower)){ return(b) 
    
    # If we got just the lower stat, return a (for lower)
  }else if(is.null(upper)){ return(a) }
}

# Calculate th data of intrest 
stat = input %>%
  group_by(time) %>%
  summarize(
    xbar = mean(temp),  # Get within group mean
    sd = sd(temp),  # Get within group sigma
    n_w = n()             # Get within subgroup size
  ) %>%
  summarize(
    xbbar = mean(xbar),               # Get grand mean
    sigma_s = sqrt(mean(sd^2)), # Get sigma_short
    sigma_t = sd(water$temp), # get sigma_total
    n = sum(n_w), # Get total observations
    n_w = unique(n_w), # get size of subgroups
    k = n())   # Get number of subgroups k
# Check it!
stat

#Have to input lower and upper bounds
cpData = cp(sigma_s = stat$sigma_s, lower = 42, upper = 80)
cpData
ppData = pp(sigma_t = stat$sigma_t, lower = 42, upper = 80)
myData

cpkData = cpk(sigma_s = stat$sigma_s, mu = stat$xbbar, lower = 42, upper = 80)
cpkData

ppkData = ppk(sigma_t = stat$sigma_t, mu = stat$xbbar, lower = 42, upper = 80)
ppkData

#Confidence Interval Using Theoretical Sampling Distributions
stat = input %>%
  group_by(time) %>%
  summarize(xbar = mean(temp),
            s = sd(temp),
            n_w = n()) %>%
  summarize(
    xbbar = mean(xbar), # grand mean x-double-bar
    sigma_s = sqrt(mean(s^2)), # sigma_short
    sigma_t = water$temp %>% sd(), # sigma_total!
    n = sum(n_w), # or just n = n()   # Total sample size
    n_w = unique(n_w),
    k = n())   #Find the number of subgroups
# Check it!
stat

cp = function(sigma_s, upper, lower){  abs(upper - lower) / (6*sigma_s)   }


stat %>% 
  summarize(
    limit_lower = 42,
    limit_upper = 50,
    # index
    estimate = cp(sigma_s = sigma_s, lower = limit_lower, upper = limit_upper))



# 2 sided 95% CI for cp
# Capability Index (for centered, normal data)
cp = function(sigma_s, upper, lower){  abs(upper - lower) / (6*sigma_s)   }
stat %>% 
  summarize(
    # index
    estimate = cp(sigma_s = sigma_s, lower = 42, upper = 50),
    # Get our extra quantities of interest
    v_short = k*(n_w - 1), # get degrees of freedom
    # Get standard error for cpk
    se = estimate * sqrt(1 / (2*v_short)),
    # Get z score
    z = qnorm(0.975), # get position of 97.5th percentile in normal distribution
    # Get upper and lower confidence interval!
    lower = estimate - z * se,
    upper = estimate + z * se)


# 2 sided 95% CI for cpk
# Capability Index (for skewed, uncentered data)
cpk = function(mu, sigma_s, lower = NULL, upper = NULL){
  if(!is.null(lower)){
    a = abs(mu - lower) / (3 * sigma_s)
  }
  if(!is.null(upper)){
    b = abs(upper - mu) /  (3 * sigma_s)
  }
  # We can also write if else statements like this
  # If we got both stats, return the min!
  if(!is.null(lower) & !is.null(upper)){
    min(a,b) %>% return()
    
    # If we got just the upper stat, return b (for upper)
  }else if(is.null(lower)){ return(b) 
    
    # If we got just the lower stat, return a (for lower)
  }else if(is.null(upper)){ return(a) }
}

stat %>% 
  summarize(
    # index
    estimate = cpk(mu = xbbar, sigma_s = sigma_s, lower = 42, upper = 50),
    # Get our extra quantities of interest
    v_short = k*(n_w - 1), # get degrees of freedom
    # Get standard error for ppk
    se = estimate * sqrt( 1 / (2*v_short)  + 1 / (9 * n * estimate^2) ),
    # Get z score
    z = qnorm(0.975), # get position of 97.5th percentile in normal distribution
    # Get upper and lower confidence interval!
    lower = estimate - z * se,
    upper = estimate + z * se)



# 2 sided 95% CI for pp
# Process Performance Index (for centered, normal data)
pp = function(sigma_t, upper, lower){  abs(upper - lower) / (6*sigma_t)   }


stat %>% 
  summarize(
    # index
    estimate = pp(sigma_t = sigma_t, lower = 42, upper = 50),
    # Get our extra quantities of interest
    v_total = n_w*k - 1, # get degrees of freedom
    # Get standard error for cpk
    se = estimate * sqrt(1 / (2*v_total)),
    # Get z score
    z = qnorm(0.975), # get position of 97.5th percentile in normal distribution
    # Get upper and lower confidence interval!
    lower = estimate - z * se,
    upper = estimate + z * se)


# 2 sided 95% CI for ppk
# Process Performance Index (for skewed, uncentered data)
ppk = function(mu, sigma_t, lower = NULL, upper = NULL){
  if(!is.null(lower)){
    a = abs(mu - lower) / (3 * sigma_t)
  }
  if(!is.null(upper)){
    b = abs(upper - mu) /  (3 * sigma_t)
  }
  # We can also write if else statements like this
  # If we got both stats, return the min!
  if(!is.null(lower) & !is.null(upper)){
    min(a,b) %>% return()
    
    # If we got just the upper stat, return b (for upper)
  }else if(is.null(lower)){ return(b) 
    
    # If we got just the lower stat, return a (for lower)
  }else if(is.null(upper)){ return(a) }
}

stat %>% 
  summarize(
    # index
    estimate = ppk(mu = xbbar, sigma_t = sigma_t, lower = 42, upper = 50),
    # Get our extra quantities of interest
    v_total = n_w*k - 1, # get degrees of freedom
    # Get standard error for cpk
    se = estimate * sqrt( 1 / (2*v_total)  + 1 / (9 * n * estimate^2) ),
    # Get z score
    z = qnorm(0.975), # get position of 97.5th percentile in normal distribution
    # Get upper and lower confidence interval!
    lower = estimate - z * se,
    upper = estimate + z * se)
