
path_raw_data <- "C:/Cloud/OneDrive - Emory University/data/NFHS/NFHS5 Papers/worsening despite wash/tables/sampling uncertainty table 1"

nfhs3 <- haven::read_dta(paste0(path_raw_data,"/nfhs3_raw.dta"))

nfhs4 <- haven::read_dta(paste0(path_raw_data,"/nfhs4_raw.dta"))

library(srvyr)

nfhs3_svydesign <- nfhs3 %>% as_survey_design(ids=psu, strata = strata,  weight = weight,
                                               nest=TRUE,
                                               variance="YG",pps="brewer",
                                    variables = c(stunting,wasting,underweight,overweight,rural,region,residence))

nfhs4_svydesign <- nfhs4 %>% as_survey(ids=psu, strata = strata,  weight = weight,
                                       nest=TRUE,
                                       variance="YG",pps="brewer",
                                       variables = c(stunting,wasting,underweight,overweight,residence,region,district))


# https://stats.stackexchange.com/questions/159204/how-to-calculate-the-standard-error-of-a-proportion-using-weighted-data
# https://stackoverflow.com/questions/41426055/r-how-to-calculate-proportion-using-survey-package

# NFHS-3-------------
nfhs3_national <- nfhs3_svydesign %>%
  summarize_at(vars(stunting,wasting,underweight,overweight),~survey_mean(., vartype = "ci",na.rm=TRUE))

nfhs3_residence <- nfhs3_svydesign %>%
  group_by(residence) %>% 
  summarize_at(vars(stunting,wasting,underweight,overweight),~survey_mean(., vartype = "ci",na.rm=TRUE))


nfhs3_state <- nfhs3_svydesign %>%
  group_by(region) %>% 
  summarize_at(vars(stunting,wasting,underweight,overweight),~survey_mean(., vartype = "ci",na.rm=TRUE))


# NFHS-4 ------------
# https://stackoverflow.com/questions/55975478/problems-due-to-having-too-many-single-psus-at-stage-one
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
nfhs4_national <- nfhs4_svydesign %>%
  summarize_at(vars(stunting,wasting,underweight,overweight),~survey_mean(., vartype = "ci",na.rm=TRUE))


nfhs4_residence <- nfhs4_svydesign %>%
  group_by(residence) %>% 
  summarize_at(vars(stunting,wasting,underweight,overweight),~survey_mean(., vartype = "ci",na.rm=TRUE))


nfhs4_state <- nfhs4_svydesign %>%
  group_by(region) %>% 
  summarize_at(vars(stunting,wasting,underweight,overweight),~survey_mean(., vartype = "ci",na.rm=TRUE))


nfhs4_district <- nfhs4_svydesign %>%
  group_by(region,district) %>% 
  summarize_at(vars(stunting,wasting,underweight,overweight),~survey_mean(., vartype = "ci",na.rm=TRUE))


# nfhs_summary <- readRDS(paste0(path_raw_data,"/nfhs3 and 4 outcome summary.RDS"))
# nfhs_summary[[6]] <- nfhs3_residence
# nfhs_summary[[7]] <- nfhs4_residence


nfhs_summary <- list(nfhs3_national,
     nfhs3_state,
     nfhs4_national,
     nfhs4_state,
     nfhs4_district,
     nfhs3_residence,
     nfhs4_residence)

saveRDS(nfhs_summary,paste0(path_raw_data,"/nfhs3 and 4 outcome summary.RDS"))


# Saving output ---------
library(tidyverse)
nfhs_output <- readRDS(paste0(path_raw_data,"/nfhs3 and 4 outcome summary.RDS")) %>% 
  purrr::map(.,function(x) x %>% 
        dplyr::mutate(stunting = paste0(round_d(stunting*100,1)," (",round_d(stunting_low*100,1),", ",round_d(stunting_upp*100,1),")"),
               underweight = paste0(round_d(underweight*100,1)," (",round_d(underweight_low*100,1),", ",round_d(underweight_upp*100,1),")"),
               wasting = paste0(round_d(wasting*100,1)," (",round_d(wasting_low*100,1),", ",round_d(wasting_upp*100,1),")"),
               overweight = paste0(round_d(overweight*100,1)," (",round_d(overweight_low*100,1),", ",round_d(overweight_upp*100,1),")")
               ) %>% 
        dplyr::select(one_of(c("region","district","residence")),stunting,underweight,wasting,overweight))
  
  
district_names <- haven::read_dta(paste0(path_repo,"/data/district all indicators wide_2021-01-13.dta")) %>% 
  dplyr::filter(round == "NFHS-4") %>% 
  dplyr::select(sdistri_nfhs5to4,nfhs5_state,nfhs4_district)

state_names_nfhs3 <- readxl::read_excel(paste0(path_nfhs5_mapping,"/mapping.xlsx"),sheet="nfhs_3") %>% 
  dplyr::select(state,nfhs_3) %>% 
  rename(state_name = state)

state_names_nfhs4 <- readxl::read_excel(paste0(path_nfhs5_mapping,"/mapping.xlsx"),sheet="nfhs_4") %>% 
  dplyr::select(state_name,v024_nfhs4)

nfhs_output[[1]] %>% 
  write.csv(.,paste0(path_raw_data,"/table 1 nfhs3 national.csv"),row.names = FALSE)

nfhs_output[[2]] %>% 
  dplyr::left_join(state_names_nfhs3,
            by=c("region"="nfhs_3")) %>% 
  write.csv(.,paste0(path_raw_data,"/table 1 nfhs3 state.csv"),row.names = FALSE)

nfhs_output[[3]] %>% 
  write.csv(.,paste0(path_raw_data,"/table 1 nfhs4 national.csv"),row.names = FALSE)

nfhs_output[[4]] %>% 
  dplyr::left_join(state_names_nfhs4,
            by=c("region"="v024_nfhs4")) %>% 
  write.csv(.,paste0(path_raw_data,"/table 1 nfhs4 state.csv"),row.names = FALSE)

nfhs_output[[5]] %>% 
  dplyr::left_join(district_names,
            by=c("district"="sdistri_nfhs5to4")) %>% 
  write.csv(.,paste0(path_raw_data,"/table 1 nfhs4 district.csv"),row.names = FALSE)

nfhs_output[[6]] %>% 
  mutate(residence = dplyr::case_when(residence == 1 ~ "Urban",
                               residence == 2 ~ "Rural")) %>% 
  write.csv(.,paste0(path_raw_data,"/table 1 nfhs3 residence.csv"),row.names = FALSE)

nfhs_output[[7]] %>% 
  mutate(residence = dplyr::case_when(residence == 1 ~ "Urban",
                               residence == 2 ~ "Rural")) %>% 
  write.csv(.,paste0(path_raw_data,"/table 1 nfhs4 residence.csv"),row.names = FALSE)

