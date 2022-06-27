require(tidyverse)

indicators <- purrr::map_dfr(c("birth","child","individual","household","population"),
                      function(i){
                        readr::read_csv(paste0("analysis/",i,"_indicators.csv"))
                      }) %>% 
  mutate(survey = case_when(str_detect(survey,"n3") ~ "NFHS3",
                            str_detect(survey,"n4") ~ "NFHS4",
                            str_detect(survey,"n5") ~ "NFHS5")) %>% 
  mutate_at(vars(est,lci,uci,std_err),~.*100) %>% 
  mutate(est_ci = paste0(round(est,1)," (",
                         round(lci,1),", ",
                         round(uci,1),")"))
                        