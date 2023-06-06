########################################################################################
# Initialisations of the search procedure
########################################################################################

#####  Arguments  #####
# dataset: dataset to analyse (data.frame)
# matrix: working correlation matrix (character: "independence" for independent matrix)
# m: magnitude of step
# k: step lenght multiplier
# bound_p: initialisation of the confidence bound (float)
# nperm: number of step of the search procedure (integer > 0)
# beta1_hat: point estimate of beta1 (difference in RMST) based on original dataset (float)
# bound: confidence bound (character: "low" for the lower bound or "up" for the upper bound)
# alpha: significance level of the test (float between 0 and 1)

####  Values ####
#results: initialisations for the search procedure (data.frame)


ci_permutation <- function(dataset, matrix, m, k, bound_p, nperm, beta1_hat, bound, alpha = 0.05){
 
  init <- bound_p
  vec <- rep(NA, nperm)
  dataset$bound_p <- bound_p

 for(p in 1:nperm){
    # Observed test statistic
    log <- capture.output(suppressMessages(fit_gee <- gee(pv ~ arm + offset(arm.obs * bound_p), 
                                                              data = dataset, 
                                                              id = cluster, 
                                                              family = gaussian, 
                                                              corstr = matrix)))
    T_obs <- coef(summary(fit_gee))["arm","Robust z"]
   
    # Permuted test statistic
    dataset_permu <- allocation(dataset)
    log <- capture.output(suppressMessages(fit_gee_per <- gee(pv ~ arm + offset(arm.obs * bound_p), 
                                                              data = dataset_permu, 
                                                              id = cluster, 
                                                              family = gaussian, 
                                                              corstr = matrix)))
    T_permuted <- coef(summary(fit_gee_per))["arm","Robust z"]
    
    # update of the bound
    bound_p <- update_bound(bound_p, beta1_hat, T_obs, T_permuted, alpha, p, m, k, bound)
    dataset$bound_p <- bound_p
    vec[p] <- bound_p
    }

  return(vec[nperm])
}
