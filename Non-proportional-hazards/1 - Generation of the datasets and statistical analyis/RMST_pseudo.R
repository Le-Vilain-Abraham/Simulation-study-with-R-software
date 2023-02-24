##########################################################################################################################################################
# Analysis with the pseudo-value regression based methods
##########################################################################################################################################################

#####  Arguments  #####
# dataset: dataset to analyse (data.frame)
# t_star: horizon time (float > 0)

####  Values ####
#results: estimations of the RMST difference, variance and 95% CI for pseudo-value regression based methods accounting and not accounting 
#         for clustering (data.frame)


RMST_pseudo <- function(dataset, t_star) {
  
  ############# Computation of pseudo-values for each individual #############
  data_pseudo <-cbind(dataset, 
                      pv = pseudomean(dataset$time,
                                      dataset$status,
                                      tmax = t_star))
  
  ############# Method not accounting for clustering #############
  fit1_gee <- gee(pv ~ arm, 
                  data = data_pseudo, 
                  id = id_patient, 
                  family = gaussian)
  
  ############# Method accounting for clustering (independent matrix) #############
  fit2_gee <- gee(pv ~ arm, 
                  data = data_pseudo, 
                  id = cluster, 
                  family = gaussian, 
                  corstr = "independence")
  
  ############# Method accounting for clustering (exchangeable matrix) #############
  fit3_gee <- gee(pv ~ arm, 
                  data = data_pseudo, 
                  id = cluster, 
                  family = gaussian, 
                  corstr = "exchangeable")


  ############# Estimations #############
  results <- data.frame(clustering=c("no", "yes", "yes"), 
                        delta.rmst=c(summary(fit1_gee)$coefficients["arm", "Estimate"], 
                                     summary(fit2_gee)$coefficients["arm" ,"Estimate"], 
                                     summary(fit3_gee)$coefficients["arm", "Estimate"]),
                        var=c(summary(fit1_gee)$coefficients["arm", "Robust S.E."]^2, 
                              summary(fit2_gee)$coefficients["arm", "Robust S.E."]^2,
                              summary(fit3_gee)$coefficients["arm", "Robust S.E."]^2))
  results$ci.low <- results[, "delta.rmst"]-qnorm(0.975)*sqrt(results[, "var"])
  results$ci.up <- results[, "delta.rmst"]+qnorm(0.975)*sqrt(results[, "var"])
  results$method <- c("pseudo", "pseudoInd", "pseudoExch")
  results$t_star <- t_star
  
  ############# Checking convergence #############   
  if(fit1_gee$error != 0){
    results[1, 2:5] <- NA
  }
  if(fit2_gee$error != 0){
    results[2, 2:5] <- NA
  }
  if(fit3_gee$error != 0){
    results[3, 2:5] <- NA
  }
  
  return(results)
}

