library(oaxaca)
version = "2021-01-13"
district_wide = read_dta(paste0(papers_path,"/worsening despite wash/district all indicators wide_", version,".dta")) %>% 
  mutate(group = case_when(round == "NFHS-5" ~ 0,
                           TRUE ~ 1))

nnm_ids = c("S81","S84",
            "S92",
            "S82",
            "S83", 
            "S85",
            
            
            "S14","S16","S20",
            "S04","S09","S08",
            "S07","S10","S12")

district_wide %>% group_by(round) %>% summarize_at(vars(one_of("S57","S66","S76")),~sum(is.na(.)))

n_outcomes = 5

results_t3 <- map_dfr(nnm_ids[-c(1:6)],
                      # results_t3 <- map_dfr(nnm_ids[c(10:12)],
                      # results_t3 <- map_dfr(c("S07","S08","S09","S14"),
                      function(x){
                        res_S81 = oaxaca(as.formula(paste0("S81 ~ ",x,"|group")),data=district_wide,group.weights = 0.5);
                        res_S82 = oaxaca(as.formula(paste0("S82 ~ ",x,"|group")),data=district_wide,group.weights = 0.5);
                        res_S84 = oaxaca(as.formula(paste0("S84 ~ ",x,"|group")),data=district_wide,group.weights = 0.5);
                        res_S85 = oaxaca(as.formula(paste0("S85 ~ ",x,"|group")),data=district_wide,group.weights = 0.5);
                        res_S92 = oaxaca(as.formula(paste0("S92 ~ ",x,"|group")),data=district_wide,group.weights = 0.5);
                        
                        out = bind_rows(
                          summary(res_S81)$twofold$overall[3,] %>% t() %>% data.frame() %>% mutate(outcome = "Stunting"),
                          summary(res_S82)$twofold$overall[3,] %>% t() %>% data.frame() %>% mutate(outcome = "Wasting"),
                          summary(res_S84)$twofold$overall[3,] %>% t() %>% data.frame() %>% mutate(outcome = "Underweight"),
                          summary(res_S85)$twofold$overall[3,] %>% t() %>% data.frame() %>% mutate(outcome = "Overweight"),
                          summary(res_S92)$twofold$overall[3,] %>% t() %>% data.frame() %>% mutate(outcome = "Anemia")
                        ) %>% 
                          mutate(ID = rep(c(x),times=n_outcomes)) %>% 
                          # mutate_if(is.numeric,~round(.,1)) %>% 
                          # mutate(explained = paste0(coef.explained.," pm ",se.explained.),
                          #        unexplained = paste0(coef.unexplained.," pm ",se.unexplained.)) %>% 
                          mutate(explained = paste0(round(coef.explained.,1)," (",
                                                    round(coef.explained. - 1.96*se.explained.,1),", ",
                                                    round(coef.explained. + 1.96*se.explained.,1),")"),
                                 unexplained = paste0(round(coef.unexplained.,1)," (",
                                                      round(coef.unexplained. - 1.96*se.unexplained.,1),", ",
                                                      round(coef.unexplained. + 1.96*se.unexplained.,1),")")) %>% 
                          dplyr::select(ID,outcome,explained,unexplained,
                                        coef.explained.,se.explained.,
                                        coef.unexplained.,se.unexplained.);
                        
                        return(out)
                        
                        
                        
                        
                      })

indicator_nfhs5s <- readxl::read_excel(paste0(extracts_path,"/mapping.xlsx"),sheet="nfhs5_state")

results_t3 %>% 
  dplyr::select(-coef.explained.,-se.explained.,
                -coef.unexplained.,-se.unexplained.) %>% 
  left_join(indicator_nfhs5s %>% 
              dplyr::select(ID,nfhs5s_description),
            by = c("ID")) %>%
  dplyr::filter(ID != "Intercept") %>% 
  mutate(nfhs5s_description = case_when(ID == "nl" ~ "Night lights index",
                                        TRUE ~ nfhs5s_description)) %>% 
  arrange(outcome) %>% 
  write.csv(.,paste0(papers_path,"/worsening despite wash/tables/blinder_oaxaca bivariate.csv"))
