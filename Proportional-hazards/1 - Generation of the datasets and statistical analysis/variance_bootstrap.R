##########################################################################################################################################################
# Resample one dataset and estimate the difference in RMST with the Kaplan-Meier-based method
# (function to estimate the variance of the difference in RMST using bootstrap method)
##########################################################################################################################################################

#####  Arguments  #####
# K: total number of clusters
# dataset: dataset to analyse (data.frame)
# t_star: horizon time

####  Values ####
# estimation of the difference in RMST for the boostraped sample (float)


variance_bootstrap <- function(K, dataset, t_star) {
  
  # Sample K/2 clusters in each arm
  c <-c(sample(1:(K/2), K/2, replace = TRUE), 
        sample(((K/2)+1):K, K/2, replace = TRUE))
  
  # Select the clusters in the data.frame
  data.boot <- dataset[unlist(lapply(c , function(x) which(dataset$cluster == x))), ]

  while(min(max(data.boot[which(data.boot$arm == 1), ]$time), 
            max(data.boot[which(data.boot$arm == 0), ]$time)) < 365){
    c <-c(sample(1:(K/2), K/2, replace = TRUE), 
          sample(((K/2)+1):K, K/2, replace = TRUE))
    
    data.boot <- dataset[unlist(lapply(c, function(x) which(dataset$cluster == x))), ]
  }

  # Analysis with the bootstraped dataset
  results <- rmst2(data.boot$time,       
                     data.boot$status,            
                     data.boot$arm, 
                     tau = t_star)
  
 #return the difference in RMST
 return(results$unadjusted.result[1, 1]) 
}
