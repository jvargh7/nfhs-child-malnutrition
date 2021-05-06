library(tidyverse)
path_raw_data <- "C:/Cloud/OneDrive - Emory University/data/NFHS/NFHS5 Papers/worsening despite wash/tables/sampling uncertainty table 1"

nfhs_se <- readRDS(paste0(path_raw_data,"/nfhs3 and 4 outcome summary.RDS")) %>% 
  purrr::map(.,function(x) x %>% 
               mutate_at(vars(matches("(stunting|underweight|wasting|overweight)")),function(x) x*100) %>% 
               dplyr::mutate(stunting_se = (stunting - stunting_low)/1.96,
                             underweight_se = (underweight - underweight_low)/1.96,
                             wasting_se = (wasting - wasting_low)/1.96,
                             overweight_se = (overweight - overweight_low)/1.96
               ) %>% 
               dplyr::select(one_of(c("region","district","residence")),stunting,underweight,wasting,overweight,contains("_se")) %>% 
               tidyr::pivot_longer(cols=-one_of(c("region","district","residence"))))


# Check order in Table 1 with what is there in nfhs_se -------

nfhs5_national = data.frame(name = paste0(c("stunting","underweight","wasting","overweight"),rep(c("","_se"),each=4)),
                            value = c(36,34.3,21.7,3.8,
                                      nfhs_se[[3]][5:8,2] %>% unlist() %>% as.numeric()))

nfhs5_rural = data.frame(name = paste0(c("stunting","underweight","wasting","overweight"),rep(c("","_se"),each=4)),
                         value = c(38,36.2,22,3.4,
                                   nfhs_se[[7]] %>% 
                                     dplyr::filter(residence == 2,str_detect(name,pattern="_se")) %>% 
                                     dplyr::select(value) %>% 
                                     pull()))

nfhs5_urban = data.frame(name = paste0(c("stunting","underweight","wasting","overweight"),rep(c("","_se"),each=4)),
                            value = c(32,30.1,20.6,4.8,
                                      nfhs_se[[7]] %>% 
                                        dplyr::filter(residence == 1,str_detect(name,pattern="_se")) %>% 
                                        dplyr::select(value) %>% 
                                        pull()))


# NATIONAL ------

nfhs_all_national <- bind_rows(nfhs_se[[1]] %>% 
                             mutate(round = 3),
                           nfhs_se[[3]] %>% 
                             mutate(round = 4),
                           nfhs5_national %>% 
                             mutate(round = 5)) %>% 
  mutate(estimate = case_when(str_detect(name,"_se") ~ "se",
                              TRUE ~ "mean"),
         outcome = str_replace(name,"_se","")) %>% 
  dplyr::select(-name) %>% 
  pivot_wider(names_from = c(estimate,round),values_from="value") %>% 
  mutate(diff_mean_4v3 = mean_4 - mean_3,
         diff_mean_5v4 = mean_5 - mean_4,
         diff_se_4v3 = sqrt(se_4^2 + se_3^2),
         diff_se_5v4 = sqrt(se_5^2 + se_4^2)
         ) %>% 
  dplyr::select(outcome,starts_with("diff")) %>% 
  mutate(diff_lci_4v3 = diff_mean_4v3 - 1.96*diff_se_4v3,
         diff_uci_4v3 = diff_mean_4v3 + 1.96*diff_se_4v3,
         diff_lci_5v4 = diff_mean_5v4 - 1.96*diff_se_5v4,
         diff_uci_5v4 = diff_mean_5v4 + 1.96*diff_se_5v4
         ) %>% 
  dplyr::select(-contains("_se_v")) %>% 
  mutate_at(vars(contains("4v3")),function(x) x/9) %>% 
  mutate_at(vars(contains("5v4")),function(x) x/4) %>% 
  mutate(change_4v3 = paste0(round_d(diff_mean_4v3,1)," (",round_d(diff_lci_4v3,1),", ",round_d(diff_uci_4v3,1),")"),
         change_5v4 = paste0(round_d(diff_mean_5v4,1)," (",round_d(diff_lci_5v4,1),", ",round_d(diff_uci_5v4,1),")")
         )  %>% 
  mutate(z_diff_change = (diff_mean_4v3 - diff_mean_5v4)/(sqrt(diff_se_4v3^2 + diff_se_5v4^2)))

# RURAL ------

nfhs_all_rural <- bind_rows(nfhs_se[[6]] %>% 
                             dplyr::filter(residence == 2) %>% 
                             mutate(round = 3),
                           nfhs_se[[7]] %>% 
                             dplyr::filter(residence == 2) %>% 
                             mutate(round = 4),
                           nfhs5_rural %>% 
                             mutate(round = 5)) %>% 
  mutate(estimate = case_when(str_detect(name,"_se") ~ "se",
                              TRUE ~ "mean"),
         outcome = str_replace(name,"_se","")) %>% 
  dplyr::select(-name,-residence) %>% 
  pivot_wider(names_from = c(estimate,round),values_from="value") %>% 
  mutate(diff_mean_4v3 = mean_4 - mean_3,
         diff_mean_5v4 = mean_5 - mean_4,
         diff_se_4v3 = sqrt(se_4^2 + se_3^2),
         diff_se_5v4 = sqrt(se_5^2 + se_4^2)
  ) %>% 
  dplyr::select(outcome,starts_with("diff")) %>% 
  mutate(diff_lci_4v3 = diff_mean_4v3 - 1.96*diff_se_4v3,
         diff_uci_4v3 = diff_mean_4v3 + 1.96*diff_se_4v3,
         diff_lci_5v4 = diff_mean_5v4 - 1.96*diff_se_5v4,
         diff_uci_5v4 = diff_mean_5v4 + 1.96*diff_se_5v4
  ) %>% 
  dplyr::select(-contains("_se_v")) %>% 
  mutate_at(vars(contains("4v3")),function(x) x/9) %>% 
  mutate_at(vars(contains("5v4")),function(x) x/4) %>% 
  mutate(change_4v3 = paste0(round_d(diff_mean_4v3,1)," (",round_d(diff_lci_4v3,1),", ",round_d(diff_uci_4v3,1),")"),
         change_5v4 = paste0(round_d(diff_mean_5v4,1)," (",round_d(diff_lci_5v4,1),", ",round_d(diff_uci_5v4,1),")")
  )  %>% 
  mutate(z_diff_change = (diff_mean_4v3 - diff_mean_5v4)/(sqrt(diff_se_4v3^2 + diff_se_5v4^2)))

# URBAN ------

nfhs_all_urban <- bind_rows(nfhs_se[[6]] %>% 
                              dplyr::filter(residence == 1) %>% 
                              mutate(round = 3),
                            nfhs_se[[7]] %>% 
                              dplyr::filter(residence == 1) %>% 
                              mutate(round = 4),
                            nfhs5_urban %>% 
                              mutate(round = 5))%>% 
  mutate(estimate = case_when(str_detect(name,"_se") ~ "se",
                              TRUE ~ "mean"),
         outcome = str_replace(name,"_se","")) %>% 
  dplyr::select(-name,-residence) %>% 
  pivot_wider(names_from = c(estimate,round),values_from="value") %>% 
  mutate(diff_mean_4v3 = mean_4 - mean_3,
         diff_mean_5v4 = mean_5 - mean_4,
         diff_se_4v3 = sqrt(se_4^2 + se_3^2),
         diff_se_5v4 = sqrt(se_5^2 + se_4^2)
  ) %>% 
  dplyr::select(outcome,starts_with("diff")) %>% 
  mutate(diff_lci_4v3 = diff_mean_4v3 - 1.96*diff_se_4v3,
         diff_uci_4v3 = diff_mean_4v3 + 1.96*diff_se_4v3,
         diff_lci_5v4 = diff_mean_5v4 - 1.96*diff_se_5v4,
         diff_uci_5v4 = diff_mean_5v4 + 1.96*diff_se_5v4
  ) %>% 
  dplyr::select(-contains("_se_v")) %>% 
  mutate_at(vars(contains("4v3")),function(x) x/9) %>% 
  mutate_at(vars(contains("5v4")),function(x) x/4) %>% 
  mutate(change_4v3 = paste0(round_d(diff_mean_4v3,1)," (",round_d(diff_lci_4v3,1),", ",round_d(diff_uci_4v3,1),")"),
         change_5v4 = paste0(round_d(diff_mean_5v4,1)," (",round_d(diff_lci_5v4,1),", ",round_d(diff_uci_5v4,1),")")
  )  %>% 
  mutate(z_diff_change = (diff_mean_4v3 - diff_mean_5v4)/(sqrt(diff_se_4v3^2 + diff_se_5v4^2)))

# SAVE --------

write.csv(nfhs_all_national,paste0(path_raw_data,"/table 1 annualized national.csv"),row.names = FALSE)
write.csv(nfhs_all_urban,paste0(path_raw_data,"/table 1 annualized urban.csv"),row.names = FALSE)
write.csv(nfhs_all_rural,paste0(path_raw_data,"/table 1 annualized rural.csv"),row.names = FALSE)


