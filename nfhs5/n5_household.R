require(haven)
require(tidyverse)

household_vars <- readxl::read_excel("data/mapping_ext.xlsx",sheet="ncm_variables") %>% 
  rename("selected" = iahr7adt) %>% 
  dplyr::select(new_var,selected) %>% 
  dplyr::filter(!is.na(selected))

n5_household <- read_dta(paste0(path_dhs_folder,"/IA/IAHR7ADT/IAHR7AFL.dta"),col_select = household_vars$selected) %>% 
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

saveRDS(n5_household,paste0(path_nfhs_malnutrition_paper,"/working/n5_household.RDS"))
