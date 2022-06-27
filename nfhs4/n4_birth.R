require(haven)
require(tidyverse)

birth_vars <- readxl::read_excel("data/mapping_ext.xlsx",sheet="ncm_variables") %>% 
  rename("selected" = iabr74dt) %>% 
  dplyr::select(new_var,selected) %>% 
  dplyr::filter(!is.na(selected))

n4_birth <- read_dta(paste0(path_dhs_folder,"/IA/IABR74DT/IABR74FL.dta"),col_select = birth_vars$selected) %>% 
  rename_with(~ birth_vars$new_var[which(birth_vars$selected == .x)], .cols = birth_vars$selected) %>% 
  mutate(
    age = m_interview - c_dob
    
  ) %>% 
  dplyr::filter(age <= 59) %>% 
  mutate(S04 = case_when(c_sex == 2 ~ 1,
                         c_sex == 1 ~ 0,
                         TRUE ~ NA_real_)) %>% 
  
  
  mutate(state_df = factor(state,labels=attr(state,"labels") %>% 
                             attr(.,"names") %>% str_replace(.,"\\[[a-z]+\\]\\s",""))) %>% 
  mutate(weight = weight/(10^6)) %>% 
  left_join(v024 %>% 
              dplyr::select(statecode,v024_nfhs4),
            by = c("state_df" = "v024_nfhs4"))  %>% 
  mutate(district_df = district)

saveRDS(n4_birth,paste0(path_nfhs_malnutrition_paper,"/working/n4_birth.RDS"))
