##########################################################################################################################################################
# Measures of performance
##########################################################################################################################################################

#####  Arguments  #####
# t_star: horizon time (integer > 0)
# lambda and rho: parameters of the Weibull distribution (float > 0)
# beta: treatment effect (float)
# dataset: estimations on the D dataset of the  (data.frame)
# gamma: parameter of the gamma distribution of the frailty terms (float >0)
# PH: does the hazards are proportional ? TRUE or FALSE (bool)

####  Values ####
# relative bias (RE), relative error (RE), the coverage rate (coverage), type I error rate (for scenario where HR=1) (rejection.rate) and the number 
#of simulation iterations which converged (D) (data.frame)


performance_measures <- function(t_star, lambda, rho, beta, dataset, gamma, PH){
  D <- nrow(dataset)
  
  #True difference in RMSTs
  if (PH){
    true.delta <- true_rmst_difference_PH(t_star, 0.0001,lambda, rho, beta, gamma)
  } else {
    true.delta <- true_rmst_difference_NPH(t_star, 0.0001, lambda, rho, beta, gamma, 90)
  }
  
  #Mean difference in RMST
  mean.delta <- mean(dataset$delta.rmst)
  
  #Performance measures
  relative.bias <- (mean.delta - true.delta)/true.delta *100
  
  ASD <- sqrt(mean(dataset$var))
  ESD <- sqrt(sum((dataset$delta.rmst - mean.delta)^2)/(D - 1)) 

  relative.error <- (ASD-ESD)/ESD *100
    
  coverage <- mean(dataset$ci.low <= true.delta & dataset$ci.up >= true.delta) * 100 
  
  rejection.rate <- mean(dataset$ci.low > 0 | dataset$ci.up < 0) * 100 
  
  #Results
  return(c("RB" = relative.bias, 
           "RE" = relative.error, 
           "coverage" = coverage, 
           "rejection.rate" = rejection.rate, 
           "D" = D))
}
