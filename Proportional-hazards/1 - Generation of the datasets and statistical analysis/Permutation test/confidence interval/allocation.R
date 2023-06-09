########################################################################################
# Permutes intervention allocation
########################################################################################

#####  Arguments  #####
# dataset: dataset to analyse (data.frame)

####  Values ####
#results: dataset with permuted intervention allocation (data.frame)


allocation <- function(dataset){
 
  K <- max(dataset$cluster)
  allocation <- sample(1:K, K/2, replace = FALSE)
  
  #Re-assign the intervention and control group
  dataset$arm <- 0
  dataset[dataset$cluster %in% allocation, ]$arm <- 1
  
  return(dataset)
  
}
