##########################################################################################################################################################
# Generate one simulated dataset for one scenario 
# (non-proportional-hazards assumption)
##########################################################################################################################################################

#####  Arguments  #####
# K: number of clusters (int > 0)
# m: cluster size (int > 0)
# lambda and rho: parameters of the Weibull distribution (float > 0)
# gamma: parameter of the Gamma frailty distribution (float > 0)
# beta : treatment effect (float)
# t_delay: time from which there is an intervention effect (integer > 0)
# censoring: censoring rate (float between 0 and 1)

####  Values ####
# data: one simulated dataset (data.frame) 


generate_data_NPH <- function(K, m, lambda, rho, gamma, beta, t_delay, censoring){
  
  ###################################################################################################
  #Time-to-event
  ###################################################################################################
    # Calculation of the parameters of the negative binomial distribution
    v = (0.6*m)^2 #calculation of the variance considering a coefficient of variation = 0.6

    # Generation of the cluster sizes for the control and intervention arms
    cluster_sizes_inter <- rnbinom(K/2, size = m^2/(v-m), mu = m)
    cluster_sizes_control <- rnbinom(K/2, size = m^2/(v-m), mu = m)
    
    # Checking if there is a null cluster size
    while(is.element(0, c(cluster_sizes_inter, cluster_sizes_control))){
      cluster_sizes_inter <- rnbinom(K/2, size = m^2/(v-m), mu = m)
      cluster_sizes_control <- rnbinom(K/2, size = m^2/(v-m), mu = m)
    }
    
    #Intervention arm
    tte_inter <- sapply(cluster_sizes_inter, 
                        function(x) generate_cluster_NPH(x, 
                                                        lambda, rho,
                                                        u_k = rgamma(n = 1, shape = gamma, rate = gamma), 
                                                        beta, t_delay, Z = 1))
    data_inter <- data.frame("time" = unlist(tte_inter), 
                             "arm" = 1, 
                             "cluster" = rep(1:(K/2), times = cluster_sizes_inter))

    #Control arm
    tte_control <-  sapply(cluster_sizes_control, 
                           function(x) generate_cluster_NPH(x, 
                                                           lambda, rho, 
                                                           u_k = rgamma(n = 1, shape = gamma, rate = gamma), 
                                                           beta, t_delay, Z = 0))
    data_control <- data.frame("time" = unlist(tte_control), 
                               "arm" = 0,
                               "cluster" = rep(((K/2)+1):(K), times = cluster_sizes_control))
      

  ###################################################################################################
  #Creation of the database 
  ###################################################################################################
  data <- rbind(data_inter, data_control) #combine control and intervention time-to-event
  data$id_patient <- 1:nrow(data) #ID of the individuals
  
  ###################################################################################################
  #Censoring
  ###################################################################################################
  data$status <- ifelse(runif(nrow(data), 0, 1) > censoring, 1, 0)
  data[which(data$status == 0), ]$time <-  sapply(data[which(data$status == 0), ]$time, function(x) runif(1, 0, x))

  return(data)
}



