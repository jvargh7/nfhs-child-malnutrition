require(tidyverse)

source("analysis/indicators_consolidated.R")
source("C:/code/external/functions/nhst/z_test.R")
prevalence <- indicators %>% 
  dplyr::filter(level %in% c("India","Rural","Urban"),variable %in% c("S81","S82","S84","S85")) %>% 
  dplyr::select(level,variable,survey,est_ci) %>% 
  pivot_wider(names_from=survey,values_from=est_ci) %>% 
  arrange(level)

annualized_change <- indicators %>% 
  dplyr::filter(level %in% c("India","Rural","Urban"),variable %in% c("S81","S82","S84","S85")) %>% 
  dplyr::select(level,variable,survey,est,std_err) %>% 
  pivot_wider(names_from=c("survey"),values_from=c("est","std_err")) %>% 
  mutate(change_N3to4 = (est_NFHS4 - est_NFHS3)/diff3to4,
         change_N4to5 = (est_NFHS5 - est_NFHS4)/diff4to5,
         std_err_N3to4 = sqrt((1/diff3to4^2)*(std_err_NFHS3^2 + std_err_NFHS4^2)),
         std_err_N4to5 = sqrt((1/diff4to5^2)*(std_err_NFHS4^2 + std_err_NFHS5^2))) %>% 
  mutate(change_ci_N3to4 = paste0(round(change_N3to4,1)," (",
                            round(change_N3to4 - 1.96*std_err_N3to4,1),", ",
                            round(change_N3to4 + 1.96*std_err_N3to4,1),")"),
         change_ci_N4to5 = paste0(round(change_N4to5,1)," (",
                                  round(change_N4to5 - 1.96*std_err_N4to5,1),", ",
                                  round(change_N4to5 + 1.96*std_err_N4to5,1),")"),
         test_change = z_test(change_N3to4,change_N4to5,std_err_N3to4,std_err_N4to5,output = "z_p")
         ) %>% 
  dplyr::select(level,variable,change_ci_N3to4,change_ci_N4to5,test_change) %>% 
  arrange(level)

tests_differences <- indicators %>% 
  dplyr::filter(level %in% c("State","State_Urban","State_Rural"),variable %in% c("S81","S82","S84","S85")) %>% 
  bind_rows(
            {.} %>% 
              dplyr::filter(survey == "NFHS3",statecode == "AP") %>% 
              mutate(statecode = "TG")) %>% 
  
  dplyr::select(level,statecode,variable,survey,est) %>% 
  pivot_wider(names_from=survey,values_from=est) %>% 
  arrange(level) %>% 
  dplyr::filter(complete.cases(.)) %>% 
  mutate(change_N3to4 = (NFHS4-NFHS3)/diff3to4,
         change_N4to5 = (NFHS5-NFHS4)/diff4to5) %>% 
  group_by(level,variable) %>% 
  summarize(diff_change = mean(change_N4to5 - change_N3to4),
            test_output = wilcox.test(change_N3to4,change_N4to5,paired=TRUE)$statistic[[1]],
            test_pvalue = wilcox.test(change_N3to4,change_N4to5,paired=TRUE)$p.value[[1]],
            n = n())

poshan <- indicators %>% 
  dplyr::filter(level %in% c("India","Rural","Urban"),variable %in% c("S81","S82","S84","S85")) %>% 
  dplyr::select(level,variable,survey,est) %>% 
  pivot_wider(names_from=survey,values_from=est) %>% 
  arrange(level) %>% 
  mutate(target = case_when(variable %in% c("S85") ~ NA_real_,
                            TRUE ~ round(NFHS4 - 2*6,1))) %>% 
  dplyr::select(level,variable,target)


table_prevalence <- prevalence %>% 
  left_join(annualized_change,
            by=c("level","variable")) %>% 
  left_join(tests_differences %>% 
              dplyr::select(level,variable,test_pvalue) %>% 
              mutate(test_pvalue = p_val_format(test_pvalue)) %>% 
              mutate(level = case_when(level == "State" ~ "India",
                                       level == "State_Urban" ~ "Urban",
                                       level == "State_Rural" ~ "Rural")),
            by = c("level","variable")) %>% 
  left_join(poshan ,
            by = c("level","variable"))

write_csv(table_prevalence,"paper/table_prevalence and change for growth indicators.csv")
