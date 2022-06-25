ids = c("S81","S84",
        "S92",
        "S82",
        "S83", 
        "S85",
        
        
        "S14","S16","S20",
        "S04","S09","S08",
        "S07","S10","S12")

version = "2021-01-13"

# changed_ids = c("S92",
#                 "S09","S08",
#                 "S14","S11")

# Use state (wide format) -----------
state_df <- read_dta(paste0(papers_path,"/worsening despite wash/state all indicators wide_",version,".dta")) %>% 
  pivot_longer(cols=-one_of("nfhs5_state","variable","round","region"),names_to ="ID",values_to="value") %>% 
  dplyr::select(-region,-round) %>% 
  pivot_wider(names_from = "variable",values_from="value") %>% 
  left_join(readxl::read_excel(paste0(extracts_path,"/mapping.xlsx"),sheet="nfhs5_state") %>% 
              dplyr::select(ID,nfhs5s_description) %>% 
              distinct(ID,.keep_all=TRUE) %>% 
              dplyr::select(ID,nfhs5s_description),
            by = c("ID")) %>%
  dplyr::select(ID,nfhs5s_description,
                nfhs4s_total,
                nfhs5_state,nfhs5s_total) %>% 
  dplyr::filter(ID %in% c(ids,"S07","S08","S09"))

urban_df <- read_dta(paste0(papers_path,"/worsening despite wash/state urban indicators wide_",version,".dta")) %>% 
  pivot_longer(cols=-one_of("nfhs5_state","variable","round","region"),names_to ="ID",values_to="value") %>% 
  dplyr::select(-region,-round) %>% 
  dplyr::filter(!nfhs5_state %in% c("Andaman & Nicobar Islands","Dadra & Nagar Haveli and Daman & Diu",
                                    "Lakshadweep","Ladakh","Jammu & Kashmir")) %>% 
  pivot_wider(names_from = "variable",values_from="value") %>% 
  left_join(readxl::read_excel(paste0(extracts_path,"/mapping.xlsx"),sheet="nfhs5_state") %>% 
              dplyr::select(ID,nfhs5s_description) %>% 
              distinct(ID,.keep_all=TRUE) %>% 
              dplyr::select(ID,nfhs5s_description),
            by = c("ID")) %>%
  dplyr::select(ID,nfhs5s_description,
                nfhs4s_urban,
                nfhs5_state,nfhs5s_urban) %>% 
  dplyr::filter(ID %in% ids)

rural_df <- read_dta(paste0(papers_path,"/worsening despite wash/state rural indicators wide_",version,".dta")) %>% 
  pivot_longer(cols=-one_of("nfhs5_state","variable","round","region"),names_to ="ID",values_to="value") %>% 
  dplyr::select(-region,-round) %>% 
  dplyr::filter(!nfhs5_state %in% c("Andaman & Nicobar Islands","Dadra & Nagar Haveli and Daman & Diu",
                                    "Lakshadweep","Ladakh","Jammu & Kashmir")) %>% 
  
  pivot_wider(names_from = "variable",values_from="value") %>% 
  left_join(readxl::read_excel(paste0(extracts_path,"/mapping.xlsx"),sheet="nfhs5_state") %>% 
              dplyr::select(ID,nfhs5s_description) %>% 
              distinct(ID,.keep_all=TRUE) %>% 
              dplyr::select(ID,nfhs5s_description),
            by = c("ID")) %>%
  dplyr::select(ID,nfhs5s_description,
                nfhs4s_rural,
                nfhs5_state,nfhs5s_rural) %>% 
  dplyr::filter(ID %in% ids)

district_df <- read_dta(paste0(papers_path,"/worsening despite wash/district all indicators wide_",version,".dta")) %>% 
  mutate(variable = case_when(round == "NFHS-4" ~ "nfhs4d_total",
                              TRUE ~ "nfhs5d_total")) %>% 
  pivot_longer(cols=-one_of("nfhs5_state","nfhs4_district","variable","round","sdistri_nfhs5to4"),names_to ="ID",values_to="value") %>% 

  dplyr::select(-round) %>% 
  pivot_wider(names_from = "variable",values_from="value") %>% 
  left_join(readxl::read_excel(paste0(extracts_path,"/mapping.xlsx"),sheet="nfhs5_district") %>% 
              dplyr::select(ID,nfhs5d_description) %>% 
              distinct(ID,.keep_all=TRUE) %>% 
              dplyr::select(ID,nfhs5d_description),
            by = c("ID")) %>% 
  dplyr::filter(ID %in% c(ids,"S07","S08","S09"))


# state_pc <- read_dta(paste0(papers_path,"/worsening despite wash/state development score.dta"))
# district_pc <- read_dta(paste0(papers_path,"/worsening despite wash/district development score.dta"))

# States -----------
state_df %>% 
  mutate(increase5 = case_when(ID == "S47" & (nfhs5s_total  > nfhs4s_total + 250) ~ 1,
                               nfhs5s_total  > nfhs4s_total + 5 ~ 1,
                               is.na(nfhs5s_total)|is.na(nfhs4s_total) ~ NA_real_,
                               TRUE ~ 0),
         
         count_nfhs5 = case_when(is.na(nfhs5s_total) ~ 0,
                                 TRUE ~ 1),
         count_nfhs4 = case_when(is.na(nfhs4s_total) ~ 0,
                                 TRUE ~ 1),
         decrease5 = case_when(ID == "S47" & (nfhs5s_total + 250 < nfhs4s_total) ~ 1,
                               nfhs5s_total + 5 < nfhs4s_total ~ 1,
                               is.na(nfhs5s_total)|is.na(nfhs4s_total) ~ NA_real_,
                               TRUE ~ 0),
         decrease7 = case_when(ID == "S47" & (nfhs5s_total + 250 < nfhs4s_total) ~ 1,
                               nfhs5s_total + 7 < nfhs4s_total ~ 1,
                               is.na(nfhs5s_total)|is.na(nfhs4s_total) ~ NA_real_,
                               TRUE ~ 0),
         increase = case_when(nfhs5s_total > nfhs4s_total ~ 1,
                              is.na(nfhs5s_total)|is.na(nfhs4s_total) ~ NA_real_,
                              TRUE ~ 0),
         decrease = case_when(nfhs5s_total < nfhs4s_total ~ 1,
                              is.na(nfhs5s_total)|is.na(nfhs4s_total) ~ NA_real_,
                              TRUE ~ 0)) %>% 
  group_by(ID,nfhs5s_description) %>% 
  summarize_at(vars(increase5,increase,decrease5,decrease7, decrease,count_nfhs5,count_nfhs4),funs(total = sum(.,na.rm=TRUE),non_na = sum(!is.na(.)))) %>% 
  dplyr::select(ID,nfhs5s_description,increase5_total,decrease5_total,decrease7_total,increase_total,decrease_total,count_nfhs5_total,count_nfhs4_total) %>%
  write.csv(.,paste0(papers_path,"/worsening despite wash/tables/st2_states.csv"),row.names = FALSE)



# Districts --------

district_df %>% 
  mutate(increase5 = case_when(ID == "S47" & (nfhs5d_total  > nfhs4d_total + 250) ~ 1,
                               nfhs5d_total  > nfhs4d_total + 5 ~ 1,
                               is.na(nfhs5d_total)|is.na(nfhs4d_total) ~ NA_real_,
                               TRUE ~ 0),
         decrease5 = case_when(ID == "S47" & (nfhs5d_total + 250 < nfhs4d_total) ~ 1,
                               nfhs5d_total + 5 < nfhs4d_total ~ 1,
                               is.na(nfhs5d_total)|is.na(nfhs4d_total) ~ NA_real_,
                               TRUE ~ 0),
         decrease7 = case_when(ID == "S47" & (nfhs5d_total + 250 < nfhs4d_total) ~ 1,
                               nfhs5d_total + 7 < nfhs4d_total ~ 1,
                               is.na(nfhs5d_total)|is.na(nfhs4d_total) ~ NA_real_,
                               TRUE ~ 0),
         count_nfhs5 = case_when(is.na(nfhs5d_total) ~ 0,
                                 TRUE ~ 1),
         count_nfhs4 = case_when(is.na(nfhs4d_total) ~ 0,
                                 TRUE ~ 1),
         increase = case_when(nfhs5d_total > nfhs4d_total ~ 1,
                              is.na(nfhs5d_total)|is.na(nfhs4d_total) ~ NA_real_,
                              TRUE ~ 0),
         decrease = case_when(nfhs5d_total < nfhs4d_total ~ 1,
                              is.na(nfhs5d_total)|is.na(nfhs4d_total) ~ NA_real_,
                              TRUE ~ 0))  %>% 
  group_by(ID,nfhs5d_description) %>% 
  summarize_at(vars(increase5,increase,decrease5,decrease7,decrease,count_nfhs5,count_nfhs4),funs(total = sum(.,na.rm=TRUE),non_na = sum(!is.na(.)))) %>% 
  dplyr::select(ID,nfhs5d_description,increase5_total,decrease5_total,decrease7_total,increase_total,decrease_total,count_nfhs5_total,count_nfhs4_total) %>% 
  write.csv(.,paste0(papers_path,"/worsening despite wash/tables/st2_districts.csv"),row.names = FALSE)



# Urban ------------

urban_df %>% 
  dplyr::filter(!nfhs5_state %in% c("Dadra & Nagar Haveli and Daman & Diu","Ladakh","Jammu & Kashmir")) %>% 
  mutate(increase5 = case_when(ID == "S47" & (nfhs5s_urban  > nfhs4s_urban + 250) ~ 1,
                               nfhs5s_urban  > nfhs4s_urban + 5 ~ 1,
                               is.na(nfhs5s_urban)|is.na(nfhs4s_urban) ~ NA_real_,
                               TRUE ~ 0),
         decrease5 = case_when(ID == "S47" & (nfhs5s_urban + 250 < nfhs4s_urban) ~ 1,
                               nfhs5s_urban + 5 < nfhs4s_urban ~ 1,
                               is.na(nfhs5s_urban)|is.na(nfhs4s_urban) ~ NA_real_,
                               TRUE ~ 0),
         decrease7 = case_when(ID == "S47" & (nfhs5s_urban + 250 < nfhs4s_urban) ~ 1,
                               nfhs5s_urban + 7 < nfhs4s_urban ~ 1,
                               is.na(nfhs5s_urban)|is.na(nfhs4s_urban) ~ NA_real_,
                               TRUE ~ 0),
         count_nfhs5 = case_when(is.na(nfhs5s_urban) ~ 0,
                                 TRUE ~ 1),
         count_nfhs4 = case_when(is.na(nfhs4s_urban) ~ 0,
                                 TRUE ~ 1),
         increase = case_when(nfhs5s_urban > nfhs4s_urban ~ 1,
                              is.na(nfhs5s_urban)|is.na(nfhs4s_urban) ~ NA_real_,
                              TRUE ~ 0),
         decrease = case_when(nfhs5s_urban < nfhs4s_urban ~ 1,
                              is.na(nfhs5s_urban)|is.na(nfhs4s_urban) ~ NA_real_,
                              TRUE ~ 0)) %>% 
  group_by(ID,nfhs5s_description) %>% 
  summarize_at(vars(increase5,increase,decrease5,decrease7,decrease,count_nfhs5,count_nfhs4),funs(total = sum(.,na.rm=TRUE),non_na = sum(!is.na(.)))) %>% 
  dplyr::select(ID,nfhs5s_description,increase5_total,decrease5_total,decrease7_total,increase_total,decrease_total,count_nfhs5_total,count_nfhs4_total) %>%
  write.csv(.,paste0(papers_path,"/worsening despite wash/tables/st2_urban.csv"),row.names = FALSE) 



# Rural -------

rural_df %>% 
  dplyr::filter(!nfhs5_state %in% c("Dadra & Nagar Haveli and Daman & Diu","Ladakh","Jammu & Kashmir")) %>% 
  mutate(increase5 = case_when(ID == "S47" & (nfhs5s_rural  > nfhs4s_rural + 250) ~ 1,
                               nfhs5s_rural  > nfhs4s_rural + 5 ~ 1,
                               is.na(nfhs5s_rural)|is.na(nfhs4s_rural) ~ NA_real_,
                               TRUE ~ 0),
         decrease5 = case_when(ID == "S47" & (nfhs5s_rural + 250 < nfhs4s_rural) ~ 1,
                               nfhs5s_rural + 5 < nfhs4s_rural ~ 1,
                               is.na(nfhs5s_rural)|is.na(nfhs4s_rural) ~ NA_real_,
                               TRUE ~ 0),
         decrease7 = case_when(ID == "S47" & (nfhs5s_rural + 250 < nfhs4s_rural) ~ 1,
                               nfhs5s_rural + 7 < nfhs4s_rural ~ 1,
                               is.na(nfhs5s_rural)|is.na(nfhs4s_rural) ~ NA_real_,
                               TRUE ~ 0),
         count_nfhs5 = case_when(is.na(nfhs5s_rural) ~ 0,
                                 TRUE ~ 1),
         count_nfhs4 = case_when(is.na(nfhs4s_rural) ~ 0,
                                 TRUE ~ 1),
         increase = case_when(nfhs5s_rural > nfhs4s_rural ~ 1,
                              is.na(nfhs5s_rural)|is.na(nfhs4s_rural) ~ NA_real_,
                              TRUE ~ 0),
         decrease = case_when(nfhs5s_rural < nfhs4s_rural ~ 1,
                              is.na(nfhs5s_rural)|is.na(nfhs4s_rural) ~ NA_real_,
                              TRUE ~ 0)) %>% 
  group_by(ID,nfhs5s_description) %>% 
  summarize_at(vars(increase5,increase,decrease5,decrease7,decrease,count_nfhs5,count_nfhs4),funs(total = sum(.,na.rm=TRUE),non_na = sum(!is.na(.)))) %>% 
  dplyr::select(ID,nfhs5s_description,increase5_total,decrease5_total,decrease7_total,increase_total,decrease_total,count_nfhs5_total,count_nfhs4_total) %>%
  write.csv(.,paste0(papers_path,"/worsening despite wash/tables/st2_rural.csv"),row.names = FALSE)


