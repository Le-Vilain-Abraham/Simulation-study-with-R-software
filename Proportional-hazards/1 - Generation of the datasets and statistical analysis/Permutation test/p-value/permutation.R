########################################################################################
#  Analysis of one dataset with the permutation test
########################################################################################

#####  Arguments  #####
# dataset: dataset to analyse (data.frame)
# t_star: horizon time (float > 0)

####  Values ####
#results: estimations of the pseudo-values (data.frame)


permutation <- function(dataset, t_star) {
  
  ##################################################################################
  ############################# Pseudo-value regression ############################
  
  ############# Computation of pseudo-values for each individual #############
  data_pseudo <-cbind(dataset, 
                      pv = pseudomean(dataset$time,
                                      dataset$status,
                                      tmax = t_star))
  
  ############# Method accounting for clustering (independent matrix) #############
  fit_gee_ind <- gee(pv ~ arm, 
                     data = data_pseudo, 
                     id = cluster, 
                     family = gaussian, 
                     corstr = "independence")
  
  ############# Method accounting for clustering (exchangeable matrix) #############
  fit_gee_exc <- gee(pv ~ arm, 
                     data = data_pseudo, 
                     id = cluster, 
                     family = gaussian, 
                     corstr = "exchangeable")

  
  ##################################################################################
  ################################ Permutation test ################################
  #Allocations
  Per <- 5
  K <- max(dataset$cluster)
  
  if (K != 10) {
    nb_perm <- 0
    
    while (nb_perm < Per*2) {
      inter <- replicate(Per*2, sample(1:K, K/2, replace = FALSE))
      nb_perm <- nrow(unique(t(inter)))
    }
  }else{
    inter <- combn(1:10, 5)
  }
  

  ############# Method accounting for clustering (exchangeable matrix) #############
  if(fit_gee_exc$error == 0){ ### convergence
    
    if(K > 10){ # K > 10
      indic <- c()
      i <- 0
      results_permutation_exc <- c()
      while (length(indic) < Per){
        res_exc_i <- NA
          while (is.na(res_exc_i)){
            i <- i + 1
            res_exc_i <- permutation_test(data_pseudo, t_star, "exchangeable", inter[ , i]) 
          }
          results_permutation_exc <- c(results_permutation_exc , res_exc_i)
          indic <- c(indic, i)
      }
    }else{ # K = 10
      results_permutation_exc <- apply(inter, 2, function(x) permutation_test(data_pseudo, t_star, "exchangeable", x))
    }
    pvalue_exc <- mean(abs(results_permutation_exc) >= abs(coef(summary(fit_gee_exc))["arm", "Robust z"]), na.rm = T)
    
  }else{ ### no convergence
    pvalue_exc <- NA
  }

  ############# Method accounting for clustering (independent matrix) #############
  if(fit_gee_ind$error == 0){ ### convergence 
    
    if (K > 10){ # K > 10
      if(fit_gee_exc$error == 0){
        results_permutation_ind <- apply(inter[ , indic], 2, function(x) permutation_test(data_pseudo, t_star, "independence", x)) 
      } else {
        results_permutation_ind <- apply(inter[ , 1:Per], 2, function(x) permutation_test(data_pseudo, t_star, "independence", x)) 
      }
    }else{ # K = 10
      results_permutation_ind <- apply(inter, 2, function(x) permutation_test(data_pseudo, t_star, "independence", x)) 
    }
    pvalue_ind <- mean(abs(results_permutation_ind) >= abs(coef(summary(fit_gee_ind))["arm", "Robust z"]), na.rm = T)
    
  }else{ ### convergence problem
    pvalue_ind <- NA
  }
  
  
  ##################################################################################
  ##################################### Results ####################################
  results <- data.frame(matrix = c("ind", "exc"),
                        pvalue=c(pvalue_ind, pvalue_exc),
                        t_star = t_star)

  return(results)
}



