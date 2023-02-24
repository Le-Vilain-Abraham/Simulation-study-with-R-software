########################################################################################################################################################## 
##############################      Code used in "Restricted mean survival time to estimate an intervention effect     ################################### 
##############################                  in a cluster randomized trial" Le Vilain--Abraham et al.               ################################### 
##########################################################################################################################################################

#_______________________________________________________ Code for proportional-hazards assumption _______________________________________________________#

#--------------------------------------------------------------------------------------------------------------------------------------------------------# 
#----------------------------------------------------- 1 - Simulation of the datasets and analysis ------------------------------------------------------#  
#--------------------------------------------------------------------------------------------------------------------------------------------------------# 

# Packages
##########################################################################################################################################################
library(doParallel)
library(doRNG)

# Functions needed 
##########################################################################################################################################################
setwd("~/your/path/to/Rfiles/") # set directory where you saved the R files with the necessary functions
source("sim_PH.R")
source("generate_data_PH.R")
source("generate_cluster_PH.R")
source("RMST_pseudo.R")
source("RMST_KM.R")
source("variance_bootstrap.R")

# Parameters
##########################################################################################################################################################
#Parameter for the data generation
table_parameter <- data.frame("D" = 1000,          #number of simulated dataset
                              "K" = 10,            #total number of clusters (should be pair)
                              "m" = 80,            #mean cluster size
                              "lambda" = 0.000016, #scale parameter of the Weibull distribution 
                              "rho" = 2,           #shape parameter of the Weibull distribution
                              "gamma" =  4.5,      #parameter of the gamma distribution of the frailty term
                              "beta"= log(0.5),    #intervention effect (beta)
                              "censoring" = 0.2,   #censoring rate (between 0 and 1)
                              "seed" = 1598)       #seed

# Horizon time (t*)
t_star <- 365


# Generation of the data and analysis 
##########################################################################################################################################################
### Create a file to save data and a folder to save the datasets 
setwd("~/your/path/to/Simulated_datasets/") # set directory where the simulated datasets and the estimations will be saved
  
name.file <- paste("K=", table_parameter[ , "K"], 
                   "-m=", table_parameter[ , "m"], 
                   "-tau=", ifelse(table_parameter[, "gamma"] == 0, 0, 1/(1+2*table_parameter[ , "gamma"])), 
                   "-HR=", exp(table_parameter[ , "beta"]),
                   "-censoring=", table_parameter[ , "censoring"], 
                   sep = "")    
  
write.table(data.frame("d" = "d",
                       "K" = "K",
                       "m" = "m",
                       "HR" = "HR",
                       "tau" = "tau",
                       "censoring"="censoring",
                       "clustering" = "clustering", 
                       "delta.rmst" = "delta.rmst", 
                       "var" = "var", 
                       "ci.low" = "ci.low", 
                       "ci.up" = "ci.up",
                       "method" = "method",
                       "t_star" = "t_star"), 
             file = paste(name.file, ".txt", sep = ""), 
             sep = " ", dec = ".",
             col.names = FALSE, row.names = FALSE)

dir.create(name.file)

###Parallelisation
registerDoParallel(cores = 8) #set the number of cores
set.seed(table_parameter[,"seed"])
res <- foreach(d = 1:table_parameter[,"D"],
               .packages = c("pseudo", "gee", "survRM2", "foreach")) %dorng% sim_PH(K = table_parameter[ , "K"], 
                                                                                    m = table_parameter[ , "m"], 
                                                                                    lambda = table_parameter[ , "lambda"], rho = table_parameter[ , "rho"], 
                                                                                    gamma = table_parameter[ , "gamma"], 
                                                                                    beta = table_parameter[ , "beta"], 
                                                                                    censoring = table_parameter[ , "censoring"], 
                                                                                    t_star = t_star,
                                                                                    d, 
                                                                                    name.file) 
