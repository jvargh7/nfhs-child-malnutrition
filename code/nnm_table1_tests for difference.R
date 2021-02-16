version = "2021-01-13"
nnm_ids = c("S81","S84",
            "S92",
            "S82",
            "S83", 
            "S85",
            
            
            "S14","S16","S20",
            "S04","S09","S08",
            "S07","S10","S12")
state_df <- read_dta(paste0(papers_path,"/worsening despite wash/state all indicators wide_",version,".dta")) %>% 
  mutate(variable = case_when(round == "NFHS-4" ~ "nfhs4s_total",
                              round == "NFHS-3" ~ "nfhs3s_total",
                              TRUE ~ "nfhs5s_total")) %>% 
  mutate(S20 = case_when(is.na(S20) ~ NA_real_,
                         TRUE ~ 100-S20)) %>% 
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
  mutate(ann3to4 = (nfhs3s_total-nfhs4s_total)/9,
         ann4to5 = (nfhs4s_total-nfhs5s_total)/4)


results_t1 <- map_dfr(unique(state_df$ID),
# results_table1 <- map_dfr(c("S81","S84"),
                         function(x){
                           before <- state_df[state_df$ID==x,]$ann3to4;
                           after <- state_df[state_df$ID==x,]$ann4to5;
                           test_res = wilcox.test(before,after,paired=TRUE);
                           
                           out <- data.frame(
                             ID = x,
                             statistic = test_res$statistic[[1]],
                             p_value = test_res$p.value[[1]],
                             nobs = length(before)
                           );
                           return(out)
                           
                           
                         })

indicator_nfhs5s <- readxl::read_excel(paste0(extracts_path,"/mapping.xlsx"),sheet="nfhs5_state")

results_t1 %>% 
  left_join(indicator_nfhs5s %>% 
              dplyr::select(ID,nfhs5s_description),
            by = c("ID")) %>% 
  mutate(res = paste0(statistic,", p ",case_when(p_value < 0.01 ~ "<0.01",
                                               TRUE ~ paste0("= ",round(p_value,2))))) %>% 
  # arrange(ID) %>% 
  write.csv(.,paste0(papers_path,"/worsening despite wash/tables/wilcoxon signed rank total.csv"))


# RURAL --------------------
rural_df <- read_dta(paste0(papers_path,"/worsening despite wash/state rural indicators wide_",version,".dta")) %>% 
  mutate(variable = case_when(round == "NFHS-4" ~ "nfhs4s_rural",
                              round == "NFHS-3" ~ "nfhs3s_rural",
                              TRUE ~ "nfhs5s_rural")) %>% 
  mutate(S20 = case_when(is.na(S20) ~ NA_real_,
                         TRUE ~ 100-S20)) %>% 
  pivot_longer(cols=-one_of("nfhs5_state","variable","round","region"),names_to ="ID",values_to="value") %>% 
  
  dplyr::select(-round,-region) %>% 
  pivot_wider(names_from = "variable",values_from="value") %>% 
  left_join(readxl::read_excel(paste0(extracts_path,"/mapping.xlsx"),sheet="nfhs5_state") %>% 
              dplyr::select(ID,nfhs5s_description) %>% 
              distinct(ID,.keep_all=TRUE) %>% 
              dplyr::select(ID,nfhs5s_description),
            by = c("ID")) %>% 
  dplyr::filter(ID %in% nnm_ids) %>% 
  dplyr::filter(!is.na(nfhs5s_rural),!is.na(nfhs4s_rural),!is.na(nfhs3s_rural)) %>% 
  mutate(ann3to4 = (nfhs3s_rural-nfhs4s_rural)/9,
         ann4to5 = (nfhs4s_rural-nfhs5s_rural)/4)


results_t1R <- map_dfr(unique(rural_df$ID),
                      # results_table1 <- map_dfr(c("S81","S84"),
                      function(x){
                        before <- rural_df[rural_df$ID==x,]$ann3to4;
                        after <- rural_df[rural_df$ID==x,]$ann4to5;
                        test_res = wilcox.test(before,after,paired=TRUE);
                        
                        out <- data.frame(
                          ID = x,
                          statistic = test_res$statistic[[1]],
                          p_value = test_res$p.value[[1]],
                          nobs = length(before)
                        );
                        return(out)
                        
                        
                      })

indicator_nfhs5s <- readxl::read_excel(paste0(extracts_path,"/mapping.xlsx"),sheet="nfhs5_state")

results_t1R %>% 
  left_join(indicator_nfhs5s %>% 
              dplyr::select(ID,nfhs5s_description),
            by = c("ID")) %>% 
  mutate(res = paste0(statistic,", p ",case_when(p_value < 0.01 ~ "<0.01",
                                                 TRUE ~ paste0("= ",round(p_value,2))))) %>% 
  # arrange(ID) %>% 
  write.csv(.,paste0(papers_path,"/worsening despite wash/tables/wilcoxon signed rank rural.csv"))


# URBAN --------------------
urban_df <- read_dta(paste0(papers_path,"/worsening despite wash/state urban indicators wide_",version,".dta")) %>% 
  mutate(variable = case_when(round == "NFHS-4" ~ "nfhs4s_urban",
                              round == "NFHS-3" ~ "nfhs3s_urban",
                              TRUE ~ "nfhs5s_urban")) %>% 
  mutate(S20 = case_when(is.na(S20) ~ NA_real_,
                         TRUE ~ 100-S20)) %>% 
  pivot_longer(cols=-one_of("nfhs5_state","variable","round","region"),names_to ="ID",values_to="value") %>% 
  
  dplyr::select(-round,-region) %>% 
  pivot_wider(names_from = "variable",values_from="value") %>% 
  left_join(readxl::read_excel(paste0(extracts_path,"/mapping.xlsx"),sheet="nfhs5_state") %>% 
              dplyr::select(ID,nfhs5s_description) %>% 
              distinct(ID,.keep_all=TRUE) %>% 
              dplyr::select(ID,nfhs5s_description),
            by = c("ID")) %>% 
  dplyr::filter(ID %in% nnm_ids) %>% 
  dplyr::filter(!is.na(nfhs5s_urban),!is.na(nfhs4s_urban),!is.na(nfhs3s_urban)) %>% 
  mutate(ann3to4 = (nfhs3s_urban-nfhs4s_urban)/9,
         ann4to5 = (nfhs4s_urban-nfhs5s_urban)/4)


results_t1U <- map_dfr(unique(urban_df$ID),
                       # results_table1 <- map_dfr(c("S81","S84"),
                       function(x){
                         before <- urban_df[urban_df$ID==x,]$ann3to4;
                         after <- urban_df[urban_df$ID==x,]$ann4to5;
                         test_res = wilcox.test(before,after,paired=TRUE);
                         
                         out <- data.frame(
                           ID = x,
                           statistic = test_res$statistic[[1]],
                           p_value = test_res$p.value[[1]],
                           nobs = length(before)
                         );
                         return(out)
                         
                         
                       })

indicator_nfhs5s <- readxl::read_excel(paste0(extracts_path,"/mapping.xlsx"),sheet="nfhs5_state")

results_t1U %>% 
  left_join(indicator_nfhs5s %>% 
              dplyr::select(ID,nfhs5s_description),
            by = c("ID")) %>% 
  mutate(res = paste0(statistic,", p ",case_when(p_value < 0.01 ~ "<0.01",
                                                 TRUE ~ paste0("= ",round(p_value,2))))) %>% 
  # arrange(ID) %>% 
  write.csv(.,paste0(papers_path,"/worsening despite wash/tables/wilcoxon signed rank urban.csv"))