require(haven)
require(tidyverse)

population_vars <- readxl::read_excel("data/mapping_ext.xlsx",sheet="ncm_variables") %>% 
  rename("selected" = iapr7adt) %>% 
  dplyr::select(new_var,selected) %>% 
  dplyr::filter(!is.na(selected))

n5_population <- read_dta(paste0(path_dhs_folder,"/IA/IAPR7ADT/IAPR7AFL.dta"),col_select = population_vars$selected) %>% 
  rename_with(~ population_vars$new_var[which(population_vars$selected == .x)], .cols = population_vars$selected) %>% 
  # dplyr::filter(hv102 == 1) %>% 
  mutate(S08 = case_when(m_water == 99 ~ NA_real_,
                         m_water %in% c(10:15,21,31,41,51,61:73) ~ 1,
                         m_water %in% c(20,30,32,40,42,43,96) ~ 0,
                         TRUE ~ NA_real_),
         S09 = case_when(m_toilet == 99 ~ NA_real_,
                         m_sharetoilet == 1 ~ 0, # Shared toilet = unimproved
                         m_toilet %in% c(10,11:13,15,20:22,41,51) ~ 1, # improved
                         m_toilet %in% c(14,23,30,42,43,44,96) ~ 0, # Unimproved
                         m_toilet == 31 ~ 0, # Open defecation
                         TRUE ~ NA_real_),
         
         S07 = case_when(m_electricity == 9 ~ NA_real_,
                         TRUE ~ as.numeric(m_electricity))
         
  ) %>% 
  distinct(state,psu,hhno,.keep_all=TRUE) %>% 
  
  
  mutate(state_df = factor(state,labels=attr(state,"labels") %>% 
                             attr(.,"names") %>% str_replace(.,"\\[[a-z]+\\]\\s","")) %>% as.character(.)) %>% 
  mutate(weight = m_nmembers*weight/(10^6)) %>% 
  mutate(state_df = case_when(state_df == "ladakh" ~ "jammu & kashmir",
                              district %in% c(494,495) ~ "daman and diu",
                              district == 496 ~ "dadra and nagar haveli",
                              TRUE ~ state_df)) %>% 
  
  left_join(v024 %>% 
              dplyr::select(statecode,v024_nfhs5),
            by = c("state_df" = "v024_nfhs5")) %>% 
  left_join(sdist %>% 
              dplyr::select(DHSCLUST,REGCODE,DHSREGCO),
            by=c("psu" = "DHSCLUST","district" = "DHSREGCO")) %>% 
  rename(district_df = REGCODE)

saveRDS(n5_population,paste0(path_nfhs_malnutrition_paper,"/working/n5_population.RDS"))



