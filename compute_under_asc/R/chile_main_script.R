
#install.packages(c("reticulate", "greta", "greta.gp"))
library(reticulate)
use_condaenv('r-reticulate', required = TRUE)
library(greta)
library(greta.gp)
library(curl)
library(here)
library(dplyr)
library(data.table)
library(tidyr)
library(padr)
library(countrycode)
# if tensorflow is not installed to a virtual
# environment/conda environment, run this command
# to get the right versions of tensorflow installed

#greta::install_tensorflow(method = "conda",
                          #version = "1.14.0",
                          #extra_packages = "tensorflow-probability==0.7")

# Temporal variation in reporting - bayesian model framework
# Fit gaussian process model using greta.gp to under-reporting estimates over time

# Set paths
setwd(here::here())

machine_path='your_path'

#source data processing and plotting scripts
source(paste(machine_path,'/R/chile_data_import.R',sep=""))
source(paste(machine_path,'/R/chile_scale_cfr.R',sep=""))
source(paste(machine_path,'/R/chile_case_convolution.R',sep=""))
source(paste(machine_path,'/R/run_bayesian_model.R',sep=""))

# Set parameters
mean <- 13
median <- 9.1

mu_cfr <- log(median)
sigma_cfr <- sqrt(2*(log(mean) - mu_cfr))

# Hospitalisation to death distribution
hospitalisation_to_death_truncated <- function(x) {
  plnorm(x + 1, mu_cfr, sigma_cfr) - plnorm(x, mu_cfr, sigma_cfr)
}

#--- Load and clean data using separate function
#jhu_data <- case_death_timeseries_function()
chile_data <- timeseries_cases_death() 
write.csv(chile_data,paste("your_path","chile_data_timeseries.csv",sep=""), row.names = FALSE) 


#--- inference is compute and memory heavy
#--- a HPC is used to run the inference for many countries/regions
#--- therefore we pick a single country here to run 
#--- we also only run it for the timeseries from September 2020
#--- as there are memory allocation issues when running it with a longer
#--- timeseries on standard computers

#--- choosing which country to run the full inference for

regions_age_stratified_cfr <- read.csv(file = paste(machine_path,'/R/chile_national_age_stratified_cfr.csv',sep=""))
#regions_age_stratified_cfr <- read.csv(file = paste(machine_path,'/R/regions_age_stratified_cfr_v2.csv',sep=""))
#iso_region <- 'Maule'

date_iterations <- read.csv(file = paste(machine_path,'/R/date_iter.csv',sep=""))

for (row in 1:nrow(regions_age_stratified_cfr)) {
  row=1
  iso_region <- as.character(regions_age_stratified_cfr[row, "nombre_region"])
  region_data <- chile_data %>%
    dplyr::filter(region == iso_region)

  cfr_baseline_region <- regions_age_stratified_cfr %>%
    dplyr::filter(nombre_region == iso_region) %>%
    pull(cfr_mid)
    
  cfr_range_high <- regions_age_stratified_cfr %>%
    dplyr::filter(nombre_region == iso_region) %>%
    pull(cfr_high)
  
  cfr_range_low <- regions_age_stratified_cfr %>%
    dplyr::filter(nombre_region == iso_region) %>%
    pull(cfr_low)
  
  cfr_range <- c(cfr_range_low,cfr_range_high)
  
  print(paste("Running for cycle for", iso_region,"/CFR:",cfr_range_low,cfr_baseline_region,cfr_range_high))
  
  for (row2 in 1:nrow(date_iterations)) {
    
    row2=6
    
    start_date_iter <- as.character(date_iterations[row2, "start_date"])
    end_date_iter <- as.character(date_iterations[row2, "end_date"])
    
    print(paste(iso_region,"CFR:",cfr_range_low,cfr_baseline_region,cfr_range_high,"from:",start_date_iter,"to:",end_date_iter))
    
    inference_data_chile <- cases_known_convolution(iso_region, region_data, cfr_baseline_region) %>%
        dplyr::filter(date >= start_date_iter) %>%
        dplyr::filter(date <= end_date_iter)
    
    write.csv(inference_data_chile,paste(machine_path,"/R/outputs/inference_",iso_region,"_",start_date_iter,"_",end_date_iter,".csv",sep=""), row.names = FALSE) 
    
    prediction <- run_bayesian_model(inference_data_chile,
                                     n_inducing = 5,
                                     cfr_baseline = cfr_baseline_region,
                                     cfr_range = cfr_range,
                                     verbose = TRUE)
    
    write.csv(prediction,paste(machine_path,"/R/outputs/prediction_",iso_region,"_",start_date_iter,"_",end_date_iter,".csv",sep=""), row.names = FALSE) 

    



  }
}

  
