##########################################################################################################################################################
# Estimation of the coverage rate for the permutation-based confidence intervals
# (proportional-hazards assumption)
##########################################################################################################################################################

#####  Arguments  #####
# dataset: dataset of the 1000 estimations of difference in RMST, its variance and 95% confidence interval (data.frame)

####  Values ####
# Coverage rate for the  (data.frame)


pm_estimation_permutation_NPH <- function(dataset) {

  results <- performance_measures(365, 
                                 lambda = 0.000016, 
                                 rho = 2, 
                                 beta = log(dataset$HR[1]), 
                                 data = dataset, 
                                 gamma = ifelse(dataset$tau[1]==0, 0, (1-dataset$tau[1])/(2*dataset$tau[1])),
                                 PH = TRUE)
  
  data.frame(methods = "ind",
             tstar = 365,
             K = dataset$K[1],
             m = dataset$m[1],
             HR = dataset$HR[1],
             tau = dataset$tau[1],
             censoring = dataset$censoring[1],
             results)
}
