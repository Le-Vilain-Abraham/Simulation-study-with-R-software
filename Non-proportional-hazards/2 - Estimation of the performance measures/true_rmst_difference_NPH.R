##########################################################################################################################################################
# Computation of the true difference in RMST
# (non-proportional-hazards assumption)
##########################################################################################################################################################

#####  Arguments  #####
# t_star: horizon time (integer > 0)
# step: step of the right Riemann sum (float > 0)
# lambda and rho: parameters of the Weibull distribution (float > 0)
# beta: treatment effect (float)
# gamma: parameter of the gamma distribution of the frailty terms (float >0)
#T_change: change point (int > 0)

####  Values ####
# True difference in RMSTs (float)

true_rmst_difference_NPH <- function(t_star, step, lambda, rho, beta, gamma, T_change) {
  
  return((sum(survival_function_NPH(seq(T_change, t_star, by = step),lambda, rho, 1, beta, gamma, T_change))
         - sum(survival_function_NPH(seq(T_change, t_star, by = step),lambda, rho, 0, beta, gamma, T_change)))*step)
  
}
