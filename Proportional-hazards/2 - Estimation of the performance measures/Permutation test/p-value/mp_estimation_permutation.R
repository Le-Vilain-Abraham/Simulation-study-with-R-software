##########################################################################################################################################################
# Estimation of the type I error rate for the permutation test for pseudo-values regression-based methods
##########################################################################################################################################################

#####  Arguments  #####
# dataset: dataset of the 1000 estimations of the p-values (data.frame)

####  Values ####
# Type I error rate for the permutation test for pseudo-values based-methods (data.frame)


mp_onescenario365PH<- function(dataset) {
  
  #dataset whitout convergence issues
  data <- data[which(is.na(data$pvalue)==FALSE),]  
  
  results <- data.frame()
  
  # estimation of the type I error rate
  for(mat in c("ind", "exc")) {
    results <-rbind(results, 
                    cbind(matrix = data[which(data$matrix==mat),]$matrix[1],
                          tie = mean(data[which(data$matrix==mat),]$pvalue <= 0.05)))
  }

  return(results)
}
