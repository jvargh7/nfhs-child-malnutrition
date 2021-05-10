library(tidyverse)
path_se_table2 <- "C:/Cloud/OneDrive - Emory University/data/NFHS/NFHS5 Papers/worsening despite wash/tables/sampling uncertainty table 2"

table2 = read.table(col.names = c("var_desc","nfhs3","nfhs4","nfhs5"),text='Women who are literate (%)	59.6 (58.22-60.94)	70.5 (70.08-70.87)  	75.0
Women with 10 or more years of schooling	23.8 (22.65-25.00)	 36.3 (35.83-36.80)	41.4
Women age 20-24 years unmarried before age 18 years (%)**	56.0 (54.24-57.70)	70.8 (70.14-71.54)	72.1
Sex ratio at birth for children born in the last five years (females per 1,000 males)	912.0 (882.16 – 942.65)	921.5 (905.28-937.91)	937.1
Population living in households that use an improved sanitation facility	34.8 (33.09-36.46)	52.7 (52.07-53.31)	69.8
Population living in households with an improved drinking-water source  (%)	89.5 (88.42-90.66)	94.8 (94.6-95.09))	94.4
Population living in households with electricity	71.1 (69.35-72.96)	89.1 (88.75-89.40)	97.7
Households using clean fuel for cooking (%)	26.9 (25.09-28.68)	43.4 (42.84-44.03)	62.7
Households with any usual member covered under a health insurance or financing scheme (%)	5.8 (5.24-6.30)	29.5 (29.10 – 30.00)	33.3
',sep="\t") %>% 
  mutate_at(vars(starts_with("nfhs")),~as.character(.)) %>% 
  pivot_longer(cols=-var_desc,names_to="round",values_to="value") %>% 
  separate(col=value,into=c("mean","lci","uci"),sep = c("(\\(|\\-|\\–|\\))")) %>% 
  mutate_at(vars(mean:uci),~as.numeric(trimws(.))) %>% 
  mutate(round = str_replace(round,"nfhs","") %>% as.numeric(.)) %>% 
  mutate(se = abs((mean - lci)/1.96)) %>% 
  dplyr::select(var_desc,round,mean,se) %>% 
  mutate(se = zoo::na.locf(se)) %>% 
  pivot_longer(cols=-one_of(c("round","var_desc")), names_to="estimate",values_to="value") %>% 
  pivot_wider(names_from = c(estimate,round),values_from="value") %>% 
  mutate(diff_mean_4v3 = mean_4 - mean_3,
         diff_mean_5v4 = mean_5 - mean_4,
         diff_se_4v3 = sqrt(se_4^2 + se_3^2),
         diff_se_5v4 = sqrt(se_5^2 + se_4^2)) %>% 
  dplyr::select(var_desc,starts_with("diff")) %>% 
  mutate(diff_lci_4v3 = diff_mean_4v3 - 1.96*diff_se_4v3,
         diff_uci_4v3 = diff_mean_4v3 + 1.96*diff_se_4v3,
         diff_lci_5v4 = diff_mean_5v4 - 1.96*diff_se_5v4,
         diff_uci_5v4 = diff_mean_5v4 + 1.96*diff_se_5v4) %>% 
  dplyr::select(-contains("_se_v")) %>% 
  mutate_at(vars(contains("4v3")),function(x) x/9) %>% 
  mutate_at(vars(contains("5v4")),function(x) x/4) %>% 
  mutate(change_4v3 = paste0(round_d(diff_mean_4v3,1)," (",round_d(diff_lci_4v3,1),", ",round_d(diff_uci_4v3,1),")"),
         change_5v4 = paste0(round_d(diff_mean_5v4,1)," (",round_d(diff_lci_5v4,1),", ",round_d(diff_uci_5v4,1),")")) %>% 
  mutate(z_diff_change = (diff_mean_4v3 - diff_mean_5v4)/(sqrt(diff_se_4v3^2 + diff_se_5v4^2)))


# Test for differences in annualized change --------


# SAVE --------

write.csv(table2,paste0(path_se_table2,"/table 2 annualized national.csv"),row.names = FALSE)          
