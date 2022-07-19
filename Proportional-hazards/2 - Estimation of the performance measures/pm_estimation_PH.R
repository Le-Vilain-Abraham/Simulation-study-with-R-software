##########################################################################################################################################################
# Estimation of the performance measures of the 5 methods
# (proportional-hazards assumption)
##########################################################################################################################################################

#####  Arguments  #####
# dataset: dataset of the 1000 estimations of difference in RMST, its variance and 95% confidence interval (data.frame)

####  Values ####
# relative bias (RE), relative error (RE), the coverage rate (coverage), type I error rate (for scenario where HR=1) (rejection.rate) and the number 
# of simulation iterations which converged (D) for the 5 methods (data.frame)


pm_estimation_PH <- function(dataset) {
  
  dataset <- dataset[which(is.na(dataset$delta.rmst)==FALSE),] 
  
  results <-rbind(performance_measures(dataset$t_star[1], 
                                       lambda = 0.000016, 
                                       rho = 2, 
                                       beta = log(dataset$HR[1]), 
                                       dataset = dataset[which(dataset$clustering == "no" & dataset$method == "KM"),], 
                                       gamma = ifelse(dataset$tau[1] == 0, 0, (1-dataset$tau[1])/(2*dataset$tau[1])),
                                       PH = TRUE),
                  performance_measures(dataset$t_star[1], 
                                       lambda = 0.000016, 
                                       rho = 2, 
                                       beta = log(dataset$HR[1]), 
                                       dataset = dataset[which(dataset$clustering == "yes" & dataset$method == "KM"),], 
                                       gamma = ifelse(dataset$tau[1] == 0, 0, (1-dataset$tau[1])/(2*dataset$tau[1])),
                                       PH = TRUE),
                  performance_measures(dataset$t_star[1], 
                                       lambda = 0.000016, 
                                       rho = 2, 
                                       beta = log(dataset$HR[1]), 
                                       dataset = dataset[which(dataset$clustering == "no" & dataset$method == "pseudo"),], 
                                       gamma = ifelse(dataset$tau[1] == 0, 0, (1-dataset$tau[1])/(2*dataset$tau[1])),
                                       PH = TRUE),
                  performance_measures(dataset$t_star[1], 
                                       lambda = 0.000016, 
                                       rho = 2, 
                                       beta = log(dataset$HR[1]), 
                                       dataset = dataset[which(dataset$clustering =="yes" & dataset$method == "pseudoInd"),], 
                                       gamma = ifelse(dataset$tau[1] == 0, 0, (1-dataset$tau[1])/(2*dataset$tau[1])),
                                       PH = TRUE),
                  performance_measures(dataset$t_star[1], 
                                       lambda = 0.000016, 
                                       rho = 2, 
                                       beta = log(dataset$HR[1]), 
                                       dataset = dataset[which(dataset$clustering == "yes" & dataset$method == "pseudoExch"),], 
                                       gamma = ifelse(dataset$tau[1] == 0, 0, (1-dataset$tau[1])/(2*dataset$tau[1])),
                                       PH = TRUE))
  
  data.frame(methods = c("KM", "KM.boot", "pseudo", "pseudo.ind", "pseudo.exch"),
             tstar = dataset$t_star[1],
             K = dataset$K[1],
             m = dataset$m[1],
             HR = dataset$HR[1],
             tau = dataset$tau[1],
             censoring = dataset$censoring[1],
             results)

  
}
