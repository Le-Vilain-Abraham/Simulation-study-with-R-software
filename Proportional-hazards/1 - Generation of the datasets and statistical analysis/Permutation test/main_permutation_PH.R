########################################################################################################################################################## 
##############################      Code used in "Restricted mean survival time to estimate an intervention effect     ################################### 
##############################                  in a cluster randomized trial" Le Vilain--Abraham et al.               ################################### 
##########################################################################################################################################################

#______________________________________________________________ Code for permutation test _______________________________________________________________#

#--------------------------------------------------------------------------------------------------------------------------------------------------------# 
#--------------------------------------------------------------------- 1 - Analysis ---------------------------------------------------------------------#  
#--------------------------------------------------------------------------------------------------------------------------------------------------------# 

# Packages
#######################################################################################
library(doParallel)
library(doRNG)

# Functions needed 
#######################################################################################
setwd("~/your/path/to/Rfiles/") # set directory where you saved the R files with the necessary functions
source("sim_permutation.R")
source("permutation.R")
source("permutation_test.R")

# Parameters
###########################################################################################################
#Parameter of the scenario
table_parameter <- data.frame("D" = 2,          #number of simulated dataset
                              "K" = 20,            #total number of clusters (should be pair)
                              "m" = 80,            #mean cluster size
                              "lambda" = 0.000016, #scale parameter of the Weibull distribution 
                              "rho" = 2,           #shape parameter of the Weibull distribution
                              "gamma" =  4.5,      #parameter of the gamma distribution of the frailty term
                              "beta"= log(0),    #intervention effect (beta)
                              "censoring" = 0.2,   #censoring rate (between 0 and 1)
                              "seed" = 1598)       #seed

# Horizon time (t*)
t_star <- 365 


# Analysis
###########################################################################################################
### Create file to save the results
#set directory where the estimations will be saved
setwd("~/your/path/to/results/") 

name.file <- paste("K=", table_parameter[ ,"K"], 
                   "-m=", table_parameter[ ,"m"], 
                   "-tau=", ifelse(table_parameter[ ,"gamma"]==0, 0 ,1/(1+2*table_parameter[ ,"gamma"])), 
                   "-HR=", exp(table_parameter[ ,"beta"]),
                   "-censoring=",table_parameter[ ,"censoring"], 
                   sep = "")

write.table(data.frame("d" = "d",
                       "K" = "K",
                       "m" = "m",
                       "HR" = "HR",
                       "tau" = "tau",
                       "censoring"="censoring",
                       "matrix" = "matrix",
                       "pvalue" = "pvalue",
                       "t_star" = "t_star"), 
           file= paste(name.file, ".txt", sep = ""), 
           sep = " ", dec = ".",
           col.names = FALSE, row.names = FALSE)


###Parallelisation
registerDoParallel(cores = 8) #set the number of cores
set.seed(table_parameter[ ,"seed"])
res <- foreach(d = 1: table_parameter[ ,"D"],
             .packages = c("pseudo", "gee", "survRM2", "foreach")) %dorng% sim_permutation(K = table_parameter[ ,"K"], 
                                                                                           m = table_parameter[ ,"m"], 
                                                                                           lambda = table_parameter[ ,"lambda"], rho = table_parameter[ ,"rho"], 
                                                                                           gamma = table_parameter[ ,"gamma"], 
                                                                                           beta = table_parameter[ ,"beta"], 
                                                                                           censoring = table_parameter[,"censoring"], 
                                                                                           t_star = t_star, 
                                                                                           d, 
                                                                                           name.file) 


