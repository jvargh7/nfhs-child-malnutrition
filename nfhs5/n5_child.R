require(haven)
require(tidyverse)

child_vars <- readxl::read_excel("data/mapping_ext.xlsx",sheet="ncm_variables") %>% 
  rename("selected" = iapr7adt_child) %>% 
  dplyr::select(new_var,selected) %>% 
  dplyr::filter(!is.na(selected))

n5_child <- read_dta(paste0(path_dhs_folder,"/IA/IAPR7ADT/IAPR7AFL.dta"),col_select = child_vars$selected) %>% 
  rename_with(~ child_vars$new_var[which(child_vars$selected == .x)], .cols = child_vars$selected) %>% 
  dplyr::filter(!is.na(c_age)) %>% 
  
  dplyr::filter(c_age < 60) %>% 
  mutate(
    S81 = case_when(c_haz >= 9996 ~ NA_real_,
                    is.na(c_haz) ~ NA_real_,
                    c_haz < -200 ~ 1,
                    TRUE ~ 0),
    S82 = case_when(c_whz >= 9996 ~ NA_real_,
                    is.na(c_whz) ~ NA_real_,
                    c_whz < -200 ~ 1,
                    TRUE ~ 0),
    S83 = case_when(c_whz >= 9996 ~ NA_real_,
                    is.na(c_whz) ~ NA_real_,
                    c_whz < -300 ~ 1,
                    TRUE ~ 0),
    S84 = case_when(c_waz >= 9996 ~ NA_real_,
                    is.na(c_waz) ~ NA_real_,
                    c_waz < -200 ~ 1,
                    TRUE ~ 0),
    S85 = case_when(c_whz >= 9996 ~ NA_real_,
                    is.na(c_whz) ~ NA_real_,
                    c_whz > 200 ~ 1,
                    TRUE ~ 0),
    S92 = case_when(c_hb >= 250 | c_hb < 30 ~ NA_real_,
                    c_age < 6 ~ NA_real_,
                    is.na(c_hb) ~ NA_real_,
                    c_hb < 110 ~ 1,
                    TRUE ~ 0)
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

saveRDS(n5_child,paste0(path_nfhs_malnutrition_paper,"/working/n5_child.RDS"))
