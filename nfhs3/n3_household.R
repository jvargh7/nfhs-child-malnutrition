require(haven)
require(tidyverse)

household_vars <- readxl::read_excel("data/mapping_ext.xlsx",sheet="ncm_variables") %>% 
  rename("selected" = iahr52dt) %>% 
  dplyr::select(new_var,selected) %>% 
  dplyr::filter(!is.na(selected))

n3_household <- read_dta(paste0(path_dhs_folder,"/IA/IAHR52DT/IAHR52FL.dta"),col_select = household_vars$selected) %>% 
  rename_with(~ household_vars$new_var[which(household_vars$selected == .x)], .cols = household_vars$selected) %>% 
  mutate(
    S12 = case_when(m_insurance %in% c(8,9) ~ NA_real_,
                    TRUE ~ as.numeric(m_insurance)),
    S10 = case_when(m_fuel %in% c(95,99) ~ NA_real_,
                    m_fuel %in% c(1:4) ~ 1,
                    m_fuel %in% c(5:11,96) ~ 0,
                    TRUE ~ NA_real_)
    
    
  ) %>% 
  
  
  mutate(state_df = factor(state,labels=attr(state,"labels") %>% 
                             attr(.,"names") %>% str_replace(.,"\\[[a-z]+\\]\\s",""))) %>% 
  mutate(weight = weight/(10^6)) %>% 
  left_join(v024 %>% 
              dplyr::select(statecode,v024_nfhs3),
            by = c("state_df" = "v024_nfhs3"))

saveRDS(n3_household,paste0(path_nfhs_malnutrition_paper,"/working/n3_household.RDS"))
