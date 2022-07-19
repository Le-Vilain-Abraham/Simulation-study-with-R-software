##########################################################################################################################################################
# Analysis of one dataset with the Kaplan-Meier-based methods 
##########################################################################################################################################################

#####  Arguments  #####
# dataset: dataset to analyse (data.frame)
# t_star: horizon time (float > 0)

####  Values ####
#results: estimations of the RMST difference, variance and 95% CI for Kaplan-Meier-based methods accounting and not accounting for clustering (data.frame)


RMST_KM <- function(dataset, t_star) {
  
  K <-length(unique(dataset$cluster)) #total number of clusters
    
  ############# method not accounting for clustering #############
  results <- rmst2(dataset$time,       
              dataset$status,            
              dataset$arm, 
              tau = t_star)
  
  ############# method accounting for clustering #############
  delta.boot <- replicate(10000, variance_bootstrap(K, dataset, t_star))

  ############# save estimations #############
  results <- data.frame(clustering = c("no", "yes"), 
                        delta.rmst = c(results$unadjusted.result[1, "Est."], results$unadjusted.result[1, "Est."]),
                        var = c(results$RMST.arm0$rmst.var + results$RMST.arm1$rmst.var, sum((delta.boot-mean(delta.boot))^2)/(length(delta.boot)-1)),
                        ci.low = c(results$unadjusted.result[1, "lower .95"], quantile(delta.boot, 0.025)),
                        ci.up = c(results$unadjusted.result[1, "upper .95"], quantile(delta.boot, 0.975)),
                        method = "KM",
                        t_star = t_star)

  return(results)
}


