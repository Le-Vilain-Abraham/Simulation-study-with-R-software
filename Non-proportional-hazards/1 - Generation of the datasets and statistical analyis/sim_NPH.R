##########################################################################################################################################################
# Simulate one dataset and estimate the difference in RMST, its associated variance and the 95% confidence with the 5 methods 
# (non-proportional-hazards assumption)
##########################################################################################################################################################

#####  Arguments  #####
# K: number of clusters (int > 0)
# m: mean cluster size (int > 0)
# lambda and rho: parameters of the Weibull distribution (floats > 0)
# gamma: parameter of the Gamma frailty distribution (float > 0)
# beta: treatment effect (float)
# t_delay: time from which there is an intervention effect (integer > 0)
# censoring: censoring rate (float between 0 and 1)
# t_star: horizon time(s) (vector)
# d: iteration number
# name.file: name of the folder and the file where the dataset and the estimation will be saved

####  Values ####
# estimations of the difference in RMST, its variance and the 95% confidence intevral for the 5 methods for one simulated dataset saved in txt files


sim_NPH <- function(K, m, lambda, rho, gamma, beta, t_delay, censoring, t_star, d, name.file){
  
  ###### Generate one dataset ######
  dataset <- generate_data_NPH(K, m, lambda, rho, gamma, beta, t_delay, censoring)
  
  while(min(max(dataset[which(dataset$arm == 1), ]$time), 
            max(dataset[which(dataset$arm == 0), ]$time)) < 365){
    dataset <- generate_data_NPH(K, m, lambda, rho, gamma, beta, t_delay, censoring)
  }

  dataset <- dataset[order(dataset[ , "cluster"], decreasing = T), ]
  
  ###### Save the dataset ######
  write.table(dataset, 
              file= paste(name.file, "/dataset", d, ".txt", sep = ""), 
              sep = " ", dec = ".", 
              col.names = TRUE, row.names = FALSE)
  
  ###### Analyses using all the methods #######
  results <- rbind(data.frame(data.table::rbindlist(lapply(t_star, function(x) RMST_KM(dataset, x)))),
                   data.frame(data.table::rbindlist(lapply(t_star, function(x) RMST_pseudo(dataset, x))))) 
  
  ###### Save the results ######
  write.table(cbind(d = d,
 		                K = K,
                    m = m,
                    HR = exp(beta),
                    t_delay = t_delay, 
                    tau = ifelse(gamma == 0, 0 ,1/(1+2*gamma)),
                    censoring = censoring,
                    results), 
              file= paste(name.file, ".txt", sep = ""), 
              append = TRUE, 
              sep = " ", dec = ".", 
              col.names = FALSE, row.names = FALSE)
}

