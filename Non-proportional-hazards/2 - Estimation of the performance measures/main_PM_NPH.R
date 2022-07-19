########################################################################################################################################################## 
##############################      Code used in "Restricted mean survival time to estimate an intervention effect     ################################### 
##############################                  in a cluster randomized trial" Le Vilain--Abraham et al.               ################################### 
##########################################################################################################################################################

#_____________________________________________________ Code for non-proportional hazards assumption _____________________________________________________#

#--------------------------------------------------------------------------------------------------------------------------------------------------------# 
#------------------------------------------------------ 2 - Estimation of the performance measures ------------------------------------------------------#  
#--------------------------------------------------------------------------------------------------------------------------------------------------------# 

# R functions 
##########################################################################################################################################################
setwd("~/your/path/to/Rfiles/") # set directory where you saved the R files with the functions needed for the simulation
source("pm_estimation_NPH.R")
source("performance_measures.R")
source("survival_function_NPH.R")
source("true_rmst_difference_NPH.R")

# Estimation of the performance measures 
##########################################################################################################################################################
# set the directory and the name of the file where all the estimations (difference in RMST, variance, 95% confidence interval) were saved in step 1
dataset <- read.table("~/your/path/to/analysis/name_of_your_file.txt", sep = " ", dec = ".",  header=T)

pm_estimation_NPH(dataset)