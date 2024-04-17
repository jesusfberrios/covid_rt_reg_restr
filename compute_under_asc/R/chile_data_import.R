
timeseries_cases_death<- function()
{
  # We downloaded the data for cases from MINSAL github 
  
  cases_minsal_path <- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto13/CasosNuevosCumulativo.csv"
  
  # This csv file contains the new cases per day per region and a final column with the totals
  
  minsal_cases_raw <- data.table::fread(cases_minsal_path,header=TRUE) 
  
  
  
  minsal_cases_df  <- minsal_cases_raw %>%
    tidyr::pivot_longer(!c("Region"), 
                        names_to = "date", 
                        values_to = "cases") %>%
    dplyr::na_if("") %>%
    dplyr::rename(region = "Region") %>%
      dplyr::select(date, region, cases)
  
  
  # downloading and setting the path for deaths from MINSAL github
  
  minsal_deaths_path <- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto14/FallecidosCumulativo.csv"
  
  # En nuestro caso se sacaran los datos del github:
  
  minsal_deaths_raw <- data.table::fread(minsal_deaths_path, header=TRUE)
  
  minsal_deaths_df  <- minsal_deaths_raw %>%
    tidyr::pivot_longer(!c("Region"), 
                        names_to = "date", 
                        values_to = "deaths") %>%
    dplyr::na_if("") %>%
    
    dplyr::rename(region = "Region") %>%
       dplyr::select(date, region, deaths)
  
  minsal_data_conditional <- minsal_cases_df %>%
    dplyr::left_join(minsal_deaths_df) %>%
    
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    padr::pad() %>%
    dplyr::rename(new_cases = cases, new_deaths = deaths) %>%
    dplyr::mutate(new_cases = tidyr::replace_na(new_cases, 0),
                  new_deaths = tidyr::replace_na(new_deaths, 0)) %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(cum_deaths = sum(new_deaths)) %>%
    dplyr::filter(cum_deaths > 0) %>%
    dplyr::select(-cum_deaths) %>%
    dplyr::mutate(new_cases = dplyr::case_when(new_cases < 0 ~ 0,
                                               new_cases >= 0 ~ new_cases),
                  new_deaths = dplyr::case_when(new_deaths < 0 ~ 0,
                                                new_deaths >= 0 ~ new_deaths)) 
  minsal_data_prev <- minsal_data_conditional %>%
    dplyr::arrange(region, date) %>%
    dplyr::mutate(cum_cases = 0) %>%
    dplyr::mutate(cum_deaths = 0)
  
  minsal_data = data.frame(date=as.Date(as.character()),region=as.character(),new_cases=as.numeric(),new_deaths=as.numeric(),cum_cases=as.numeric(),cum_deaths=as.numeric())
  for (regions in unique(minsal_data_prev$region)){
  subs = filter(minsal_data_prev, region == regions) %>% arrange(date)
  minsal_data = rbind(minsal_data, data.frame(date=subs$date, region=regions, new_cases=subs$new_cases, new_deaths=subs$new_deaths, cum_cases=cumsum(subs$new_cases), cum_deaths=cumsum(subs$new_deaths)))
  }
  

  return(minsal_data)
}


  