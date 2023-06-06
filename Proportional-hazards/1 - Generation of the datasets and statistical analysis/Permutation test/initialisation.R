########################################################################################
# Initialisation of the search procedure
########################################################################################

#####  Arguments  #####
# dataset: dataset to analyse (data.frame)
# matrix: working correlation matrix (character: "independence" for independent matrix)
# beta1_hat: point estimate of beta1 (difference in RMST) based on original dataset
# alpha: significance level of the test (float between 0 and 1)

####  Values ####
#results: initialisation for the search procedure (data.frame)


initialisation <- function(dataset, matrix, beta1_hat, alpha = 0.05) {

  m <- min(c(50, ceiling(0.3 * (4 - alpha) / alpha)))
  z <- qnorm(1 - (alpha / 2))
  k <- 2 * sqrt(2 * pi) * exp(z^2 / 2) / z
  
  #init lower and upper bounds 
  nperm_init <- ceiling((4 - alpha) / alpha) 
  dataset$beta1_hat <- beta1_hat
  permuted_beta1 <- replicate(nperm_init, initialisation_ci(dataset, matrix))
  t1 <- sort(permuted_beta1)[2]
  t2 <- sort(permuted_beta1)[nperm_init - 1]
  low <- beta1_hat - ((t2 - t1) / 2)
  up  <- beta1_hat + ((t2 - t1) / 2)

  return(data.frame(m = m, k = k, low = low, up = up))
}
