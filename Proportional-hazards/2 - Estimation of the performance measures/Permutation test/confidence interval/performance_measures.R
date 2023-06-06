##########################################################################################################################################################
# Coverage rate
##########################################################################################################################################################

#####  Arguments  #####
# t_star: horizon time (integer > 0)
# lambda and rho: parameters of the Weibull distribution (float > 0)
# beta: treatment effect (float)
# dataset: estimations on the D dataset of the  (data.frame)
# gamma: parameter of the gamma distribution of the frailty terms (float >0)
# PH: does the hazards are proportional ? TRUE or FALSE (bool)

####  Values ####
# Coverage rate (data.frame)

performance_measures <- function(t_star, lambda, rho, beta, dataset, gamma, PH){
  
  #True difference in RMSTs
  if (PH){
    true.delta <- true_rmst_difference_PH(t_star, 0.0001,lambda, rho, beta, gamma)
  } else {
    true.delta <- true_rmst_difference_NPH(t_star, 0.0001, lambda, rho, beta, gamma, 90)
  }
  
  #Coverage rate
  coverage <- mean(dataset$ci_low <= true.delta & dataset$ci_up >= true.delta) * 100 
  
  #Results
  return(data.frame(coverage=coverage))
}