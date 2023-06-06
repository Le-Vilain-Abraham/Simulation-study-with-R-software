########################################################################################################################################################## 
##############################      Code used in "Restricted mean survival time to estimate an intervention effect     ################################### 
##############################                  in a cluster randomized trial" Le Vilain--Abraham et al.               ################################### 
##########################################################################################################################################################

#______________________________________________________________ Code for permutation test _______________________________________________________________#

#--------------------------------------------------------------------------------------------------------------------------------------------------------# 
#------------------------------------------------------ 2 - Estimation of the performance measures ------------------------------------------------------#  
#--------------------------------------------------------------------------------------------------------------------------------------------------------# 

# R functions 
##########################################################################################################################################################
setwd("~/your/path/to/Rfiles/") # set directory where you saved the R files with the necessary functions for the simulation
source("performance_measures.R")


# Estimation of the type I error rate
##########################################################################################################################################################
# set the directory and the name of the file where all the estimations of the pseudo-values were saved in step 1
dataset <- read.table("~/your/path/to/analysis/name_of_your_file.txt", sep = " ", dec = ".",  header=T)

#data pour lesquels ça a convergé
data <- data[which(is.na(data$pvalue)==FALSE),] 

results <- data.frame()

for(mat in c("ind", "exc")) {
  results <-rbind(results, 
                  cbind(matrix = data[which(data$matrix==mat),]$matrix[1],
                        tie = mean(data[which(data$matrix==mat),]$pvalue < 0.05)))
}


#calculer et stocker les résultats pour chaque scénario et chaque méthodes
pm_estimation_permutation(dataset)
       
