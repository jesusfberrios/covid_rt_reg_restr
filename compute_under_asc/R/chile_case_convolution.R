#get time varying cfr data for a country
cases_known_convolution <- function(region_chile, data = timeseries_cases_death, cfr_baseline){ 
  mean <- 13
  #filter country data and adjust date
  region_data <- data %>% 
    dplyr::filter(region == region_chile) %>% 
    dplyr::mutate(date = date - mean) ## Se ajusta la fecha a la segun el promedio de Hosp to death
  
  #Filtrating dates when cumulative deaths are equal or over 10
  death_threshold_date <- region_data %>% 
    dplyr::mutate(death_cum_sum = new_deaths) %>% 
    dplyr::filter(death_cum_sum >= 10) %>% 
    dplyr::pull(date) %>% 
    min()
  
  
  #return adjusted date and reporting_estimate
  cfr <- scale_cfr_temporal(region_data) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(reporting_estimate = cfr_baseline/(100*ccfr)) %>% 
    dplyr::mutate(reporting_estimate = pmin(reporting_estimate, 1),
                  region = region_data$region,
                  date = region_data$date,
                  date_num = as.numeric(region_data$date),
                  deaths = region_data$new_deaths,
                  cases_known = cum_known_t) %>% 
    dplyr::filter(date >= death_threshold_date) %>% 
    dplyr::select(region, date, date_num, reporting_estimate, deaths, cases_known)
  
  return(cfr)
  
}

