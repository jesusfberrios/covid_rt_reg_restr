setwd(here::here())
library(here)
library(dplyr)

machine_path='your_path'
data_path='your_path'



source(paste(machine_path,"/R/chile_data_helper_functions.R",sep=""))

asymptomatic_mid <- 0.5
asymptomatic_low <- 0.1
asymptomatic_high <- 0.7

#--- assumptions about asymptomatic infections
asymptomatic_prop_mid     <- 0.5
asymptomatic_prop_low     <- 0.1
asymptomatic_prop_high    <- 0.7


#--- using the scaled traveller numbers and prevalence to calculate expected number of imported cases
#--- from all origin countries, to all destination countries
#original_ecdc_data <- ecdc_data_function()
#original_under_reporting_data <- under_reporting_data_function(original_ecdc_data)
#may_dates       <- seq(as.Date("2020-05-01"), as.Date("2020-05-31"), by="days")
#september_dates <- seq(as.Date("2020-09-01"), as.Date("2020-09-30"), by="days")

#Colectar ecdc data chile y under reporting data
ecdc_data <- read.csv(file = paste(data_path,'chile_ecdc_data.csv',sep=""))
under_reporting_data <- read.csv(file = paste(data_path,'chile_under_reporting_data.csv',sep=""))


# Escenario 2 [8May,17May]
# Escenario 3 [23May, 1Jun]
#Fechas de escenarios
chile_dates_s2       <- seq(as.Date("2020-05-08"), as.Date("2020-05-17"), by="days") 
chile_dates_s3       <- seq(as.Date("2020-05-23"), as.Date("2020-06-01"), by="days")

#region mnapping
region_mapping <- read.csv(file = paste(data_path,'region_mapping.csv',sep=""),sep=';')

#calculating prevalence Chile
prevalence_s2_all <- chile_dates_s2 %>%
    purrr::map(
        ~chile_global_prevalence_estimates_function(ecdc_data, under_reporting_data, .x)) 

prevalence_chile_s2 <- prevalence_s2_all %>%
    dplyr::bind_rows(.id = "region") %>%
    dplyr::group_by(cod_region) %>%
    dplyr::summarise(prevalence_mid = mean(prevalence_mid/n()),  # JF: sugiero dividir entre n(), originalmente esta entre 9
                     prevalence_low = mean(prevalence_low/n()),
                     prevalence_high = mean(prevalence_high/n())) %>%
    dplyr::left_join(region_mapping) %>%
    dplyr::select(region_origin_code = cod_region, region, prevalence_mid, prevalence_low, prevalence_high)

prevalence_s3_all <- chile_dates_s3 %>%
    purrr::map(
        ~chile_global_prevalence_estimates_function(ecdc_data, under_reporting_data, .x)) 

prevalence_chile_s3 <- prevalence_s3_all %>%
    dplyr::bind_rows(.id = "region") %>%
    dplyr::group_by(cod_region) %>%
    dplyr::summarise(prevalence_mid = mean(prevalence_mid/n()),
                     prevalence_low = mean(prevalence_low/n()),
                     prevalence_high = mean(prevalence_high/n())) %>% # JF: sugiero dividir entre n(), originalmente esta entre 9
    dplyr::left_join(region_mapping) %>%
    dplyr::select(region_origin_code = cod_region, region, prevalence_mid, prevalence_low, prevalence_high)


#Se lee el archivo de viajes
travel_data <- read.csv(file = paste(data_path,'all_travel_scenarios_definidos.csv',sep=""))

#-------------------- CALCULATING EXPECTED IMPORTS CHILE--------------------#
imported_cases_s2_pre_sum <- travel_data %>%
    dplyr::select(region_origin_code,region_destination_code,travel_scenario_2,travel_scenario_2sim=travel_scenario_sim) %>%
    dplyr::left_join(prevalence_chile_s2) %>%
    dplyr::select(region_origin_code,region_destination_code,
                  travel_scenario_2,travel_scenario_2sim,
                  prevalence_mid, prevalence_low, prevalence_high) %>%
    dplyr::group_by(region_destination_code) %>%
    dplyr::filter(!is.na(prevalence_mid)) %>% # OJOOOO, esto es para filtrar las regiones ORIGEN que salen con datos NA ya que no tienen prevalencia
    dplyr::mutate(
        expected_imported_cases_scenario_2_mid   = travel_scenario_2*prevalence_mid/0.3, ##OJOO se multiplican los viajes por 3
        expected_imported_cases_scenario_2_low   = travel_scenario_2*prevalence_low/0.3,
        expected_imported_cases_scenario_2_high  = travel_scenario_2*prevalence_high/0.3,
        expected_imported_cases_scenario_2sim_mid   = travel_scenario_2sim*prevalence_mid/0.3,
        expected_imported_cases_scenario_2sim_low   = travel_scenario_2sim*prevalence_low/0.3,
        expected_imported_cases_scenario_2sim_high  = travel_scenario_2sim*prevalence_high/0.3)

imported_cases_s3_pre_sum <- travel_data %>%
    dplyr::select(date_scenario_3,region_origin_code,region_destination_code,travel_scenario_3,travel_scenario_3sim=travel_scenario_sim) %>%
    dplyr::left_join(prevalence_chile_s3) %>%
    dplyr::select(date_scenario_3,region_origin_code,region_destination_code,
                  travel_scenario_3,travel_scenario_3sim,
                  prevalence_mid, prevalence_low, prevalence_high) %>%
    dplyr::group_by(region_destination_code) %>%
    dplyr::filter(!is.na(prevalence_mid)) %>% # OJOOOO, esto es para filtrar las regiones ORIGEN que salen con datos NA ya que no tienen prevalencia
    dplyr::mutate(
        expected_imported_cases_scenario_3_mid   = travel_scenario_3*prevalence_mid/0.3, ##OJOO se multiplican los viajes por 3
        expected_imported_cases_scenario_3_low   = travel_scenario_3*prevalence_low/0.3,
        expected_imported_cases_scenario_3_high  = travel_scenario_3*prevalence_high/0.3,
        expected_imported_cases_scenario_3sim_mid   = travel_scenario_3sim*prevalence_mid/0.3,
        expected_imported_cases_scenario_3sim_low   = travel_scenario_3sim*prevalence_low/0.3,
        expected_imported_cases_scenario_3sim_high  = travel_scenario_3sim*prevalence_high/0.3)

#--- removing unnecessary columns for smooth joining
imported_cases_s2_pre_sum_reduced <- imported_cases_s2_pre_sum %>% 
    dplyr::select(region_origin_code,
                  region_destination_code,
                  expected_imported_cases_scenario_2_mid,
                  expected_imported_cases_scenario_2_low,
                  expected_imported_cases_scenario_2_high,
                  expected_imported_cases_scenario_2sim_mid,
                  expected_imported_cases_scenario_2sim_low,
                  expected_imported_cases_scenario_2sim_high)

imported_cases_s3_pre_sum_reduced <- imported_cases_s3_pre_sum %>% 
    dplyr::select(region_origin_code,
                  region_destination_code,
                  expected_imported_cases_scenario_3_mid,
                  expected_imported_cases_scenario_3_low,
                  expected_imported_cases_scenario_3_high,
                  expected_imported_cases_scenario_3sim_mid,
                  expected_imported_cases_scenario_3sim_low,
                  expected_imported_cases_scenario_3sim_high)

#--- summing over all origin countries into each destination country and averaging over all days in the month
imported_cases_s2 <- imported_cases_s2_pre_sum_reduced %>%
    dplyr::summarise_at(.vars = vars(starts_with("expected_imported_cases_scenario_")),
                        .funs = function(x){sum(x, na.rm = T)/10}) %>% #Cambiamos la división a 10. antes era 31
    dplyr::arrange(region_destination_code)

imported_cases_s3 <- imported_cases_s3_pre_sum_reduced %>%
    dplyr::summarise_at(.vars = vars(starts_with("expected_imported_cases_scenario_")), #Cambiamos la división a 10. antes era 31
                        .funs = function(x){sum(x, na.rm = T)/10}) %>%
    dplyr::arrange(region_destination_code)

# #--- joining imported cases in May and September together
imported_cases <- imported_cases_s2 %>%
    dplyr::left_join(imported_cases_s3)


#-------------------- CALCULATING INCIDENCE --------------------#
#--- calculate all case data, adjusted for under-ascertainment
chile_adjusted_case_data <- under_reporting_data %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(new_cases_smoothed             = zoo::rollmean(cases, k = 7, fill = NA),
                  new_cases_adjusted_mid         = cases/estimate,
                  new_cases_adjusted_low         = cases/upper,
                  new_cases_adjusted_high        = cases/lower,
                  new_cases_adjusted_smooth_mid  = zoo::rollmean(new_cases_adjusted_mid, k = 7, fill = NA),
                  new_cases_adjusted_smooth_low  = zoo::rollmean(new_cases_adjusted_low, k = 7, fill = NA),
                  new_cases_adjusted_smooth_high = zoo::rollmean(new_cases_adjusted_high, k = 7, fill = NA)) %>%
    dplyr::mutate(cumulative_incidence_mid  = cumsum(new_cases_adjusted_mid)/(population*(1 - asymptomatic_mid)),
                  cumulative_incidence_low  = cumsum(new_cases_adjusted_low)/(population*(1 - asymptomatic_low)),
                  cumulative_incidence_high = cumsum(new_cases_adjusted_high)/(population*(1 - asymptomatic_high))) %>%
    dplyr::mutate(date_infection  = date - 9) %>%
    dplyr::mutate(cumulative_incidence_mid = dplyr::case_when(cumulative_incidence_mid >= 1 ~ 1,
                                                              cumulative_incidence_mid <= 0 ~ 0,
                                                              cumulative_incidence_mid > 0 & cumulative_incidence_mid < 1 ~ cumulative_incidence_mid)) %>%
    dplyr::mutate(cumulative_incidence_low = dplyr::case_when(cumulative_incidence_low > 1 ~ 1,
                                                              cumulative_incidence_low < 0 ~ 0,
                                                              cumulative_incidence_low > 0 & cumulative_incidence_low < 1 ~ cumulative_incidence_low)) %>%
    dplyr::mutate(cumulative_incidence_high = dplyr::case_when(cumulative_incidence_high > 1 ~ 1,
                                                               cumulative_incidence_high < 0 ~ 0,
                                                               cumulative_incidence_high > 0 & cumulative_incidence_high < 1 ~ cumulative_incidence_high)) %>%
    dplyr::ungroup() 


# calculating the incidence in each destination region in both scenarios
incidence_s2 <-  chile_adjusted_case_data %>%
    dplyr::group_by(cod_region) %>%
    dplyr::filter(date %in% chile_dates_s2) %>%
    dplyr::mutate(
        incidence_estimate_mid = new_cases_adjusted_mid/(1 - asymptomatic_prop_mid),
        incidence_estimate_low = new_cases_adjusted_low/(1 - asymptomatic_prop_low),
        incidence_estimate_high = new_cases_adjusted_high/(1 - asymptomatic_prop_high)) %>%
    dplyr::summarise(
        new_cases_adjusted_mean_mid  = mean(incidence_estimate_mid),
        new_cases_adjusted_mean_low  = mean(incidence_estimate_low),
        new_cases_adjusted_mean_high = mean(incidence_estimate_high)) %>%
    dplyr::mutate(region_destination_code = cod_region) %>%
    dplyr::select(region_destination_code,
                  new_cases_adjusted_mean_mid, 
                  new_cases_adjusted_mean_low, 
                  new_cases_adjusted_mean_high)

# calculating the incidence in each destination region in both scenarios
incidence_s3 <-  chile_adjusted_case_data %>%
    dplyr::group_by(cod_region) %>%
    dplyr::filter(date %in% chile_dates_s3) %>%
    dplyr::mutate(
        incidence_estimate_mid = new_cases_adjusted_mid/(1 - asymptomatic_prop_mid),
        incidence_estimate_low = new_cases_adjusted_low/(1 - asymptomatic_prop_low),
        incidence_estimate_high = new_cases_adjusted_high/(1 - asymptomatic_prop_high)) %>%
    dplyr::summarise(
        new_cases_adjusted_mean_mid  = mean(incidence_estimate_mid),
        new_cases_adjusted_mean_low  = mean(incidence_estimate_low),
        new_cases_adjusted_mean_high = mean(incidence_estimate_high)) %>%
    dplyr::mutate(region_destination_code = cod_region) %>%
    dplyr::select(region_destination_code,
                  new_cases_adjusted_mean_mid, 
                  new_cases_adjusted_mean_low, 
                  new_cases_adjusted_mean_high)

#-------------------- PUTTING IMPORTS AND INCIDENCE TOGETHER --------------------#
#--- calculating risk ratings in Scenario 2 and 3
# Parece que los risk ratings pasan a ser llamados imported_cases_scenario_xx_low/mid/hogh
imported_cases_and_incidence_together_s2 <- imported_cases_s2 %>%
    dplyr::left_join(incidence_s2)

imported_cases_and_incidence_together_s3 <- imported_cases_s3 %>%
    dplyr::left_join(incidence_s3)

#Casos importados e incidencias to csv
write.csv(imported_cases_and_incidence_together_s2,paste(data_path,"imported_cases_and_incidence_together_s2.csv",sep=""), row.names = FALSE) 
write.csv(imported_cases_and_incidence_together_s3,paste(data_path,"imported_cases_and_incidence_together_s3.csv",sep=""), row.names = FALSE) 


risk_ratings_s2 <- imported_cases_s2 %>%
    dplyr::left_join(incidence_s2) %>%
    dplyr::rename_at(.vars = vars(starts_with("expected")), 
                     .funs = function(x){sub(pattern = "expected_", replacement = "", x)}) %>%
    dplyr::mutate_at(.vars = vars(matches("imported.*\\_mid")),
                     .funs = list(~dplyr::na_if(./new_cases_adjusted_mean_mid, "Inf"))) %>%
    dplyr::mutate_at(.vars = vars(matches("imported.*\\_low")),
                     .funs = list(~dplyr::na_if(./new_cases_adjusted_mean_high, "Inf"))) %>%
    dplyr::mutate_at(.vars = vars(matches("imported.*\\_high")),
                     .funs = list(~dplyr::na_if(./new_cases_adjusted_mean_low, "Inf"))) %>%
    tidyr::drop_na()  %>%
    dplyr::mutate_at(.vars = vars(starts_with("imported")),
                     .funs = list(~pmin(pmax(.,0),1)))


risk_ratings_s3 <- imported_cases_s3 %>%
    dplyr::left_join(incidence_s3) %>%
    dplyr::rename_at(.vars = vars(starts_with("expected")), 
                     .funs = function(x){sub(pattern = "expected_", replacement = "", x)}) %>%
    dplyr::mutate_at(.vars = vars(matches("imported.*\\_mid")),
                     .funs = list(~dplyr::na_if(./new_cases_adjusted_mean_mid, "Inf"))) %>%
    dplyr::mutate_at(.vars = vars(matches("imported.*\\_low")),
                     .funs = list(~dplyr::na_if(./new_cases_adjusted_mean_high, "Inf"))) %>%
    dplyr::mutate_at(.vars = vars(matches("imported.*\\_high")),
                     .funs = list(~dplyr::na_if(./new_cases_adjusted_mean_low, "Inf"))) %>%
    tidyr::drop_na()  %>%
    dplyr::mutate_at(.vars = vars(starts_with("imported")),
                     .funs = list(~pmin(pmax(.,0),1)))

#Write risk ratings to csv
write.csv(risk_ratings_s2,paste(data_path,"risk_ratings_s2.csv",sep=""), row.names = FALSE) 
write.csv(risk_ratings_s3,paste(data_path,"risk_ratings_s3.csv",sep=""), row.names = FALSE) 



#--- labelling risk ratings in S2
imported_cases_and_incidence_together_labels_s2 <- 
    imported_cases_and_incidence_together_s2 %>%
    dplyr::mutate_at(.vars = vars(starts_with("imported")),
                     .funs = function(x){cut(x, breaks = c(0, 0.01, 0.1, 1),
                                             include.lowest = T, 
                                             labels = c("Green",
                                                        "Amber",
                                                        "Red"))}) %>%
    dplyr::rename(cod_reg = region_destination_code,
                  incidence_s2_mid  = new_cases_adjusted_mean_mid,
                  incidence_s2_low  = new_cases_adjusted_mean_low,
                  incidence_s2_high = new_cases_adjusted_mean_high)

#--- labelling risk ratings in S3
imported_cases_and_incidence_together_labels_s3 <- 
    imported_cases_and_incidence_together_s3 %>%
    dplyr::mutate_at(.vars = vars(starts_with("imported")),
                     .funs = function(x){cut(x, breaks = c(0, 0.01, 0.1, 1),
                                             include.lowest = T, 
                                             labels = c("Green",
                                                        "Amber",
                                                        "Red"))}) %>%
    dplyr::rename(cod_reg = region_destination_code,
                  incidence_s3_mid  = new_cases_adjusted_mean_mid,
                  incidence_s3_low  = new_cases_adjusted_mean_low,
                  incidence_s3_high = new_cases_adjusted_mean_high)




#################
#################
#################
#################
#################



#Se declaran proporciones de casos asintomaticos
#asymptomatic_estimate_mid <- 0.50
#asymptomatic_estimate_low <- 0.10
#asymptomatic_estimate_high <- 0.70
#Probando funcion chile_global_prevalence_estimates_function
date_arg <- '2020-05-08'
date_arg <- as.Date(date_arg)


new_case_estimates_recent <- ecdc_data %>%
dplyr::filter(date > date_arg - 10) %>% 
dplyr::filter(date <= date_arg ) %>%
dplyr::group_by(region, cod_region, population) %>%
dplyr::summarise(total_new_cases = sum(cases))

#o_new_case_estimates_recent <- original_ecdc_data %>%
#    dplyr::filter(date_arg - 9 < date) %>% 
#    dplyr::mutate(country = stringr::str_replace_all(country, "_", " ")) %>%
#    dplyr::mutate(iso_code = countrycode::countrycode(country, "country.name", "iso3c", custom_match = c('Kosovo' = 'RKS'))) %>%
#    dplyr::group_by(country, iso_code, population) %>%
#    dplyr::summarise(total_new_cases = sum(cases))

# turn off scientific notation
#options(scipen=999)

under_reporting_estimates_at_date <- under_reporting_data %>%
    dplyr::group_by(cod_region) %>%
    dplyr::filter(date == date_arg - 13)

most_recent_estimates_together <- under_reporting_estimates_at_date %>% 
    dplyr::left_join(new_case_estimates_recent) %>%
    dplyr::group_by(cod_region) %>%
    dplyr::mutate(prevalence_mid  = (total_new_cases/(estimate*asymptomatic_estimate_mid)/population),
                  prevalence_low  = (total_new_cases/(upper*(1 - asymptomatic_estimate_low))/population),
                  prevalence_high = (total_new_cases/(lower*(1 - asymptomatic_estimate_high))/population)) %>%
    dplyr::select(date, region, total_new_cases, estimate, lower, upper, population, prevalence_mid, prevalence_low, prevalence_high) %>%
    dplyr::arrange(region)
