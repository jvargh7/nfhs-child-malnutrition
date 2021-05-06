library(tidyverse)
path_se_table2 <- "C:/Cloud/OneDrive - Emory University/data/NFHS/NFHS5 Papers/worsening despite wash/tables/sampling uncertainty table 2"

# Internal ---------
nfhs3_sheets <- readxl::excel_sheets(paste0(path_se_table2,"/nfhs3_development_state_se.xlsx"))
nfhs4_sheets <- readxl::excel_sheets(paste0(path_se_table2,"/nfhs4_development_state_district_se.xlsx"))

district_names <- haven::read_dta(paste0(path_repo,"/data/district all indicators wide_2021-01-13.dta")) %>% 
  dplyr::filter(round == "NFHS-4") %>% 
  dplyr::select(sdistri_nfhs5to4,nfhs5_state,nfhs4_district)

state_names_nfhs3 <- readxl::read_excel(paste0(path_nfhs5_mapping,"/mapping.xlsx"),sheet="nfhs_3") %>% 
  dplyr::select(state,nfhs_3) %>% 
  rename(state_name = state)

state_names_nfhs4 <- readxl::read_excel(paste0(path_nfhs5_mapping,"/mapping.xlsx"),sheet="nfhs_4") %>% 
  dplyr::select(state_name,v024_nfhs4)

# NFHS- 3------------------
nfhs3_table2_se <- map_dfr(nfhs3_sheets[-4],
                     function(x){
                       readxl::read_excel(paste0(path_se_table2,"/nfhs3_development_state_se.xlsx"),sheet = x,skip = 3,
                                          col_names = c("subpop","mean","se","lci","uci","subpop_2","var","eq","state_abb","state_name")) %>% 
                         mutate(variable = x)
                       
                       
                     }) %>% 
  mutate(summary = paste0(round_d(mean*100,1)," (",round_d(lci*100,1),", ",round_d(uci*100,1),")")) %>% 
  bind_rows(readxl::read_excel(paste0(path_se_table2,"/nfhs3_development_state_se.xlsx"),sheet = "srb",skip = 3,
                               col_names = c("subpop","mean","se","lci","uci","subpop_2","var","eq","state_abb","state_name","b1","b2","srb","srb_lci","srb_uci")) %>%
              mutate_at(vars(starts_with("srb")),~as.numeric(.)) %>% 
              mutate(mean = srb,
                     lci = srb_lci,
                     uci = srb_uci) %>% 
              mutate(variable = "srb",
                     summary = paste0(round_d(mean,1)," (",round_d(lci,1),", ",round_d(uci,1),")"))) %>% 
  dplyr::filter(str_detect(subpop,pattern = "subpop")) %>% 
  dplyr::select(subpop,summary,variable) %>% 
  mutate(subpop = str_replace(subpop,"[(_|a-z)]+","") %>% as.numeric(.)) %>% 
    pivot_wider(names_from="variable",values_from="summary")

# NFHS-4 STATE --------

nfhs4s_sheets <- nfhs4_sheets[str_detect(nfhs4_sheets,"state")]

nfhs4s_table2_se <- map_dfr(nfhs4s_sheets[-4],
                           function(x){
                             readxl::read_excel(paste0(path_se_table2,"/nfhs4_development_state_district_se.xlsx"),sheet = x,skip = 3,
                                                col_names = c("subpop","mean","se","lci","uci","subpop_2","var","eq","state_name")) %>% 
                               mutate(variable = x) %>% 
                               mutate_at(vars(mean:uci),~as.numeric(.))
                             
                             
                           }) %>% 
  mutate(summary = paste0(round_d(mean*100,1)," (",round_d(lci*100,1),", ",round_d(uci*100,1),")")) %>% 
  bind_rows(readxl::read_excel(paste0(path_se_table2,"/nfhs4_development_state_district_se.xlsx"),sheet = "srb_state",skip = 3,
                               col_names = c("subpop","mean","se","lci","uci","subpop_2","var","eq","state_name","b1","b2","b3","b4","srb","srb_lci","srb_uci")) %>%
              mutate_at(vars(starts_with("srb")),~as.numeric(.)) %>% 
              mutate(mean = srb,
                     lci = srb_lci,
                     uci = srb_uci) %>% 
              mutate(variable = "srb",
                     summary = paste0(round_d(mean,1)," (",round_d(lci,1),", ",round_d(uci,1),")"))) %>% 
  dplyr::filter(!is.na(state_name)) %>% 
  dplyr::select(state_name,summary,variable) %>% 
  pivot_wider(names_from="variable",values_from="summary")


# NFHS-4 DISTRICT ---------

nfhs4d_sheets <- nfhs4_sheets[str_detect(nfhs4_sheets,"district")]

nfhs4d_table2_se <- map_dfr(nfhs4d_sheets[-4],
                            function(x){
                              readxl::read_excel(paste0(path_se_table2,"/nfhs4_development_state_district_se.xlsx"),sheet = x,skip = 3,
                                                 col_names = c("subpop","mean","se","lci","uci","subpop_2","var","eq","district_name")) %>% 
                                dplyr::filter(!is.na(district_name)) %>% 
                                mutate(variable = x,
                                       sdistri = 1:n()) %>% 
                                mutate_at(vars(mean:uci),~as.numeric(.))
                              
                              
                            }) %>% 
  mutate(summary = paste0(round_d(mean*100,1)," (",round_d(lci*100,1),", ",round_d(uci*100,1),")")) %>% 
  bind_rows(readxl::read_excel(paste0(path_se_table2,"/nfhs4_development_state_district_se.xlsx"),sheet = "srb_district",skip = 3,
                               col_names = c("subpop","mean","se","lci","uci","subpop_2","var","eq","district_name","b1","b2","b4","srb","srb_lci","srb_uci")) %>%
              mutate_at(vars(starts_with("srb")),~as.numeric(.)) %>% 
              dplyr::filter(!is.na(district_name)) %>% 
              mutate(mean = srb,
                     lci = srb_lci,
                     uci = srb_uci) %>% 
              mutate(variable = "srb",
                     sdistri = 1:n(),
                     summary = paste0(round_d(mean,1)," (",round_d(lci,1),", ",round_d(uci,1),")"))) %>% 
  dplyr::filter(!is.na(district_name)) %>% 
  dplyr::select(sdistri,summary,variable) %>% 
  pivot_wider(names_from="variable",values_from="summary")


# SAVE ----------------

nfhs3_table2_se %>% 
  dplyr::left_join(state_names_nfhs3 %>% 
                     arrange(nfhs_3) %>% 
                     mutate(subpop = 1:n()),
                   by=c("subpop"="subpop")) %>% 
  write.csv(.,paste0(path_se_table2,"/table 2 nfhs3 state.csv"),row.names = FALSE)


nfhs4s_table2_se %>% 
  write.csv(.,paste0(path_se_table2,"/table 2 nfhs4 state.csv"),row.names = FALSE)

nfhs4d_table2_se %>% 
  dplyr::left_join(readxl::read_excel(paste0(path_nfhs5_mapping,"/mapping.xlsx"),sheet="nfhs_4d"),
                   by=c("sdistri"="sdistri")) %>% 
  write.csv(.,paste0(path_se_table2,"/table 2 nfhs4 district.csv"),row.names = FALSE)
