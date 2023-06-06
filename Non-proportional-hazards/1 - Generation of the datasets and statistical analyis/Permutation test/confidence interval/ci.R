########################################################################################
# Permutation-based confidence interval construction for one dataset
########################################################################################

#####  Arguments  #####
# dataset: dataset to analyse (data.frame)
# matrix: working correlation matrix (character: "independence" for independent matrix)

####  Values ####
#results: estimations of the permutation-based confidence interval (data.frame)


ci <- function(dataset, matrix) {

  log <- capture.output(suppressMessages(fit_gee <- gee(pv ~ arm, 
                                                            data = dataset, 
                                                            id = cluster, 
                                                            family = gaussian, 
                                                            corstr = matrix)))
  

    ####### Initialization ####### 
    beta1_hat <-  coef(summary(fit_gee))["arm","Estimate"]
    init <- initialisation(dataset, matrix, beta1_hat)
    
    ####### Confidence interval ####### 
    results_low <- ci_permutation(dataset, matrix, init$m, init$k, init$low, nperm=5000, beta1_hat, bound="low")
    results_up <- ci_permutation(dataset, matrix, init$m, init$k, init$up, nperm=5000, beta1_hat, bound="up")

  
    return(data.frame(ci_low = results_low, ci_up = results_up))
}
