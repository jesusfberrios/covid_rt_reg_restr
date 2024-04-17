install.packages("chilemapas")

age_stratified_cfr_covid <- read.csv(file = 'R/age_stratified_cfr_paper.csv')

population_comunas <- chilemapas::censo_2017_comunas
comunas_regiones_codes <- chilemapas::codigos_territoriales

population_regiones <- dplyr::left_join(population_comunas, comunas_regiones_codes, by = c("codigo_comuna"))%>%
  mutate(edad = dplyr::case_when(edad == "0 a 4" ~ "0-9",
                                 edad == "5 a 9" ~ "0-9",
                                 edad == "10 a 14" ~ "10-19",
                                 edad == "15 a 19" ~ "10-19",
                                 edad == "20 a 24" ~ "20-29",
                                 edad == "25 a 29" ~ "20-29",
                                 edad == "30 a 34" ~ "30-39",
                                 edad == "35 a 39" ~ "30-39",
                                 edad == "40 a 44" ~ "40-49",
                                 edad == "45 a 49" ~ "40-49",
                                 edad == "50 a 54" ~ "40-49",
                                 edad == "55 a 59" ~ "50-59",
                                 edad == "60 a 64" ~ "60-69",
                                 edad == "65 a 69" ~ "60-69",
                                 edad == "70 a 74" ~ "70-79",
                                 edad == "75 a 79" ~ "70-79",
                                 edad == "80 a 84" ~ "80+",
                                 edad == "85 a 89" ~ "80+",
                                 edad == "90 a 94" ~ "80+",
                                 edad == "95 a 99" ~ "80+",
                                 edad == "100 o mas" ~ "80+")) %>%
  group_by(nombre_region,edad) %>%
  summarise(population = sum(poblacion))

population_chile <- dplyr::left_join(population_comunas, comunas_regiones_codes, by = c("codigo_comuna"))%>%
  mutate(edad = dplyr::case_when(edad == "0 a 4" ~ "0-9",
                                 edad == "5 a 9" ~ "0-9",
                                 edad == "10 a 14" ~ "10-19",
                                 edad == "15 a 19" ~ "10-19",
                                 edad == "20 a 24" ~ "20-29",
                                 edad == "25 a 29" ~ "20-29",
                                 edad == "30 a 34" ~ "30-39",
                                 edad == "35 a 39" ~ "30-39",
                                 edad == "40 a 44" ~ "40-49",
                                 edad == "45 a 49" ~ "40-49",
                                 edad == "50 a 54" ~ "40-49",
                                 edad == "55 a 59" ~ "50-59",
                                 edad == "60 a 64" ~ "60-69",
                                 edad == "65 a 69" ~ "60-69",
                                 edad == "70 a 74" ~ "70-79",
                                 edad == "75 a 79" ~ "70-79",
                                 edad == "80 a 84" ~ "80+",
                                 edad == "85 a 89" ~ "80+",
                                 edad == "90 a 94" ~ "80+",
                                 edad == "95 a 99" ~ "80+",
                                 edad == "100 o mas" ~ "80+"))%>%
  cbind(country = c('Chile'))%>%
  group_by(country,edad) %>%
  summarise(population = sum(poblacion))

chile_regions_age_adjusted_cfr <- population_regiones %>%
  dplyr::group_by(nombre_region) %>%
  summarise(cfr_mid  = weighted.mean(age_stratified_cfr_covid$cfr_mid, population)*100,
            cfr_low  = weighted.mean(age_stratified_cfr_covid$cfr_low, population)*100,
            cfr_high = weighted.mean(age_stratified_cfr_covid$cfr_high, population)*100)

chile_national_age_adjusted_cfr <- population_chile %>%
  group_by(country) %>%
  summarise(cfr_mid  = weighted.mean(age_stratified_cfr_covid$cfr_mid, population)*100,
            cfr_low  = weighted.mean(age_stratified_cfr_covid$cfr_low, population)*100,
            cfr_high = weighted.mean(age_stratified_cfr_covid$cfr_high, population)*100)

write.csv(chile_regions_age_adjusted_cfr,'R/regions_age_stratified_cfr.csv', row.names = FALSE)
write.csv(chile_national_age_adjusted_cfr,'R/chile_national_age_stratified_cfr.csv', row.names = FALSE)

age_stratified_cfr_covid