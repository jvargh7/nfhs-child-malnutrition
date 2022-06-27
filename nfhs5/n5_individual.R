require(haven)
require(tidyverse)

individual_vars <- readxl::read_excel("data/mapping_ext.xlsx",sheet="ncm_variables") %>% 
  rename("selected" = iair7adt) %>% 
  dplyr::select(new_var,selected) %>% 
  dplyr::filter(!is.na(selected))

n5_individual <- read_dta(paste0(path_dhs_folder,"/IA/IAIR7ADT/IAIR7AFL.dta"),col_select = individual_vars$selected) %>% 
  rename_with(~ individual_vars$new_var[which(individual_vars$selected == .x)], .cols = individual_vars$selected) %>% 
  mutate(
    S14 = case_when(m_eduyr %in% c(9:20) | m_literacy %in% c(1,2) ~ 1,
                    TRUE ~ 0),
    S16 = case_when(m_eduyr %in% c(10:20) ~ 1,
                    m_eduyr %in% c(0:9) ~ 0,
                    m_eduyr >= 97 ~ NA_real_,
                    TRUE ~ NA_real_),
    S20 = case_when(
      m_cohabitation == 99 ~ NA_real_,
      m_age < 20 | m_age > 24 ~ NA_real_,
      m_age %in% c(20:24) & is.na(m_cohabitation) ~ 0,
      m_age %in% c(20:24) & m_cohabitation %in% c(0:18) ~ 0,
      m_age %in% c(20:24) & m_cohabitation %in% c(19:24) ~ 1,
      TRUE ~ NA_real_
    )
    
    
  ) %>% 
  
  
  mutate(state_df = factor(state,labels=attr(state,"labels") %>% 
                             attr(.,"names") %>% str_replace(.,"\\[[a-z]+\\]\\s","")) %>% as.character(.)) %>% 
  mutate(weight = weight/(10^6)) %>% 
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

saveRDS(n5_individual,paste0(path_nfhs_malnutrition_paper,"/working/n5_individual.RDS"))
