ids = c("S81","S84",
        "S92",
        "S82",
        "S85",
        
        
        "S14","S16","S20",
        "S04","S09","S08",
        "S07","S10","S12")


# changed_ids = c("S92",
#                 "S09","S08",
#                 "S14","S11")

# Use state (wide format) -----------
state_df <- indicators %>% 
  dplyr::filter(level %in% c("State"),variable %in% ids) %>% 
  bind_rows(
    {.} %>% 
      dplyr::filter(survey == "NFHS3",statecode == "AP") %>% 
      mutate(statecode = "TG")) %>% 
  dplyr::select(level,variable,statecode,survey,est) %>% 
  pivot_wider(names_from=c("survey"),values_from=c("est")) %>% 
  mutate(change_N3to4 = (NFHS4 - NFHS3)/diff3to4,
         change_N4to5 = (NFHS5 - NFHS4)/diff4to5) %>% 
  # dplyr::filter(!is.na(change_N3to4)) %>% 
  arrange(level)

urban_df <- indicators %>% 
  dplyr::filter(level %in% c("State_Urban"),variable %in% ids) %>% 
  bind_rows(
    {.} %>% 
      dplyr::filter(survey == "NFHS3",statecode == "AP") %>% 
      mutate(statecode = "TG")) %>% 
  dplyr::select(level,variable,statecode,survey,est) %>% 
  pivot_wider(names_from=c("survey"),values_from=c("est")) %>% 
  mutate(change_N3to4 = (NFHS4 - NFHS3)/diff3to4,
         change_N4to5 = (NFHS5 - NFHS4)/diff4to5) %>% 
  # dplyr::filter(!is.na(change_N3to4)) %>% 
  arrange(level)

rural_df <- indicators %>% 
  dplyr::filter(level %in% c("State_Rural"),variable %in% ids) %>% 
  bind_rows(
    {.} %>% 
      dplyr::filter(survey == "NFHS3",statecode == "AP") %>% 
      mutate(statecode = "TG")) %>% 
  dplyr::select(level,variable,statecode,survey,est) %>% 
  pivot_wider(names_from=c("survey"),values_from=c("est")) %>% 
  mutate(change_N3to4 = (NFHS4 - NFHS3)/diff3to4,
         change_N4to5 = (NFHS5 - NFHS4)/diff4to5) %>% 
  # dplyr::filter(!is.na(change_N3to4)) %>% 
  arrange(level)

district_df <- indicators %>% 
  dplyr::filter(level == "District",survey %in% c("NFHS4","NFHS5")) %>% 
  dplyr::select(level,district_df,variable,survey,est) %>% 
  pivot_wider(names_from = survey,values_from=est)


# States -----------
bind_rows(state_df,
          rural_df,
          urban_df,
          district_df) %>% 
  mutate(# increase5 is used in Discussion
         increase5 = case_when(
                               NFHS5  > NFHS4 + 5 ~ 1,
                               is.na(NFHS5)|is.na(NFHS4) ~ NA_real_,
                               TRUE ~ 0),
        increase = case_when(NFHS5 > NFHS4 ~ 1,
                         is.na(NFHS5)|is.na(NFHS4) ~ NA_real_,
                         TRUE ~ 0),
         count_nfhs5 = case_when(is.na(NFHS5) ~ 0,
                                 TRUE ~ 1),
         count_nfhs4 = case_when(is.na(NFHS4) ~ 0,
                                 TRUE ~ 1),
         # decrease5 = case_when(
         #                       NFHS5 + 5 < NFHS4 ~ 1,
         #                       is.na(NFHS5)|is.na(NFHS4) ~ NA_real_,
         #                       TRUE ~ 0),
         decrease7 = case_when(
                               NFHS5 + 7 < NFHS4 ~ 1,
                               is.na(NFHS5)|is.na(NFHS4) ~ NA_real_,
                               TRUE ~ 0),
         
         decrease = case_when(NFHS5 < NFHS4 ~ 1,
                              is.na(NFHS5)|is.na(NFHS4) ~ NA_real_,
                              TRUE ~ 0)) %>% 
  group_by(level,variable) %>% 
  summarize_at(vars(increase,increase5,decrease7, decrease,count_nfhs5,count_nfhs4),funs(total = sum(.,na.rm=TRUE),non_na = sum(!is.na(.)))) %>% 
  dplyr::select(level,variable,decrease7_total,increase_total,increase5_total,decrease_total,count_nfhs5_total,count_nfhs4_total) %>%
  write_csv(.,"paper/change_states.csv")



