require(haven)
require(tidyverse)

birth_vars <- readxl::read_excel("data/mapping_ext.xlsx",sheet="ncm_variables") %>% 
  rename("selected" = iabr7adt) %>% 
  dplyr::select(new_var,selected) %>% 
  dplyr::filter(!is.na(selected))

n5_birth <- read_dta(paste0(path_dhs_folder,"/IA/IABR7ADT/IABR7AFL.dta"),col_select = birth_vars$selected) %>% 
  rename_with(~ birth_vars$new_var[which(birth_vars$selected == .x)], .cols = birth_vars$selected) %>% 
  mutate(
    age = m_interview - c_dob
    
  ) %>% 
  dplyr::filter(age <= 59) %>% 
  mutate(S04 = case_when(c_sex == 2 ~ 1,
                         c_sex == 1 ~ 0,
                         TRUE ~ NA_real_)) %>% 
  
  
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

saveRDS(n5_birth,paste0(path_nfhs_malnutrition_paper,"/working/n5_birth.RDS"))
