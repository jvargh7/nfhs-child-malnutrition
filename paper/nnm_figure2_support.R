version = "2021-01-13"
nnm_ids = c("S81","S84",
            "S92",
            "S82",
            "S83", 
            "S85")
read_dta(paste0(papers_path,"/worsening despite wash/state all indicators wide_",version,".dta")) %>% 
  mutate(variable = case_when(round == "NFHS-4" ~ "nfhs4s_total",
                              round == "NFHS-3" ~ "nfhs3s_total",
                              TRUE ~ "nfhs5s_total")) %>% 
  pivot_longer(cols=-one_of("nfhs5_state","variable","round","region"),names_to ="ID",values_to="value") %>% 
  
  dplyr::select(-round,-region) %>% 
  pivot_wider(names_from = "variable",values_from="value") %>% 
  left_join(readxl::read_excel(paste0(extracts_path,"/mapping.xlsx"),sheet="nfhs5_state") %>% 
              dplyr::select(ID,nfhs5s_description) %>% 
              distinct(ID,.keep_all=TRUE) %>% 
              dplyr::select(ID,nfhs5s_description),
            by = c("ID")) %>% 
  dplyr::filter(ID %in% nnm_ids) %>% 
  dplyr::filter(!is.na(nfhs5s_total),!is.na(nfhs4s_total),!is.na(nfhs3s_total)) %>% 
  mutate(ann3to4 = (nfhs3s_total-nfhs4s_total)/10,
         ann4to5 = (nfhs4s_total-nfhs5s_total)/4) %>% 
  mutate(increase5 = case_when(ID == "S47" & (nfhs4s_total  > nfhs3s_total + 250) ~ 1,
                               nfhs4s_total  > nfhs3s_total + 5 ~ 1,
                               is.na(nfhs4s_total)|is.na(nfhs3s_total) ~ NA_real_,
                               TRUE ~ 0),
         count_nfhs4 = case_when(is.na(nfhs4s_total) ~ 0,
                                 TRUE ~ 1),
         count_nfhs3 = case_when(is.na(nfhs3s_total) ~ 0,
                                 TRUE ~ 1),
         decrease5 = case_when(ID == "S47" & (nfhs4s_total + 250 < nfhs3s_total) ~ 1,
                               nfhs4s_total + 5 < nfhs3s_total ~ 1,
                               is.na(nfhs4s_total)|is.na(nfhs3s_total) ~ NA_real_,
                               TRUE ~ 0),
         increase = case_when(nfhs4s_total > nfhs3s_total ~ 1,
                              is.na(nfhs4s_total)|is.na(nfhs3s_total) ~ NA_real_,
                              TRUE ~ 0),
         decrease = case_when(nfhs4s_total < nfhs3s_total ~ 1,
                              is.na(nfhs4s_total)|is.na(nfhs3s_total) ~ NA_real_,
                              TRUE ~ 0)) %>% 
  group_by(ID,nfhs5s_description) %>% 
  summarize_at(vars(increase5,increase,decrease5,decrease,count_nfhs4,count_nfhs3),funs(total = sum(.,na.rm=TRUE),non_na = sum(!is.na(.)))) %>% 
  dplyr::select(ID,nfhs5s_description,increase5_total,decrease5_total,increase_total,decrease_total,count_nfhs4_total,count_nfhs3_total) %>%
  write.csv(.,paste0(papers_path,"/worsening despite wash/tables/st2_states nfhs3.csv"),row.names = FALSE)
