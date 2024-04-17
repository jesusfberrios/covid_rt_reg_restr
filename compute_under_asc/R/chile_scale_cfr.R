scale_cfr_temporal <- function(timeseries_cases_death, delay_fun = hospitalisation_to_death_truncated){
  
  ### data_1_in = case_death_timeseries, delay_fun declared in main
  case_incidence <- timeseries_cases_death$cum_cases
  death_incidence <- timeseries_cases_death$new_deaths
  cumulative_known_t <- NULL # cumulative cases with known outcome at time tt ## Se inicia como null
  # Sum over cases up to time tt
  for(ii in 1:nrow(timeseries_cases_death)){
    known_i <- 0 # number of cases with known outcome at time ii
    for(jj in 0:(ii - 1)){
      known_jj <- (case_incidence[ii - jj]*delay_fun(jj))
      known_i <- known_i + known_jj
    }
    cumulative_known_t <- c(cumulative_known_t,known_i) # Tally cumulative known ### OUTPUT??
  }
  
  # naive CFR value
  b_tt <- sum(death_incidence)/sum(case_incidence) 
  # corrected CFR estimator
  p_tt <- (death_incidence/cumulative_known_t) %>% pmin(.,1)
  
  data.frame(nCFR = b_tt, 
             ccfr = p_tt,
             total_deaths = sum(death_incidence), 
             deaths = death_incidence,
             cum_known_t = round(cumulative_known_t), 
             total_cases = sum(case_incidence))
}


