########################################################################################
#  Estimation of the test statistic for one permuted dataset
########################################################################################

#####  Arguments  #####
# dataset: dataset to analyse (data.frame)
# t_star: horizon time (float > 0)
# matrix: working correlation matrix (character: "independence" for independent matrix or "exchangeable" for exchangeable matrix)
# allocation: allocation of clusters between the intervention and control group (vector)

####  Values ####
#results: estimations of the test statistic (float)


permutation_test <- function(dataset, t_star, matrix, allocation) {
  
  #Re-assign the intervention and control groups
  dataset$arm <- 0
  dataset[dataset$cluster %in% allocation, ]$arm <- 1
  
  #Analysis
  fit_gee_per <- gee(pv ~ arm, 
                  data = dataset, 
                  id = cluster, 
                  family = gaussian, 
                  corstr = matrix)
  
  #Return permuted test statistic
  if(fit_gee_per$error == 0 ) {
    return(coef(summary(fit_gee_per))["arm","Robust z"])
  }else {
    return(NA)
  }
}
