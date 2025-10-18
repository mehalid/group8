Chilambda <- function (input1) {

r=100
d4 %>%
  summarize(
    lambda_hat= 1/input1,
    r=r,
    k_upper= qchisq(0.95, df=2*r)/(2*r), 
    k_lower= qchisq(0.05, df=2*r)/(2*r),
    lower=lambda_hat*k_lower,
    upper=lambda_hat*k_upper
  )


return(d4$upper)
return(d4$lower) 
}
