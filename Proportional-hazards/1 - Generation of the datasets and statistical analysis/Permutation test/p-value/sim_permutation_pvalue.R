##########################################################################################################################################################
# Permutation test for one dataset
##########################################################################################################################################################

#####  Arguments  #####
# K: number of clusters (int > 0)
# m: mean cluster size (int > 0)
# lambda and rho: parameters of the Weibull distribution (floats > 0)
# gamma: parameter of the Gamma frailty distribution (float > 0)
# beta: treatment effect (float)
# censoring: censoring rate (float between 0 and 1)
# t_star: horizon time(s) (vector)
# d: iteration number
# name.file: name of the folder and the file where the dataset and the estimation will be saved

####  Values ####
# estimations of the p-value with the permutation test witg pseudo-values regression in txt file


sim_permutation_pvalue <- function(K, m, lambda, rho, gamma, beta, censoring, t_star, d, name.file){
  
  ###### Load one dataset ######
  ## set directory where the simulated datasets have been saved
  dataset <- read.table(file = paste("~/your/path/to/Simulated_datasets/",name.file, "/dataset", d, ".txt", sep = ""),
                        header = T, sep = " ", dec = ".")
  dataset <- dataset[order(dataset[ , "arm"], decreasing = T), ]

  ###### Permutation test #######
  results <-permutation(dataset, 365)
  
  ###### Save results ######
  write.table(cbind(d = d,
                    K = K,
                    m = m,
                    HR = exp(beta),
                    tau = ifelse(gamma == 0, 0 ,1/(1 + 2*gamma)),
                    censoring = censoring,
                    results), 
              file= paste(name.file, ".txt", sep = ""), 
              append = TRUE, 
              sep = " ", dec = ".", 
              col.names = FALSE, row.names = FALSE)
}

