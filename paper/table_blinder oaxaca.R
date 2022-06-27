library(oaxaca)
source("analysis/indicators_consolidated.R")
development_score <- read_csv("analysis/development score.csv")
outcomes <- indicators %>% 
  dplyr::filter(level %in% c("District","State"),survey %in% c("NFHS4","NFHS5"),variable %in% c("S81","S82","S84","S85","S92")) %>% 
  dplyr::select(level,survey,statecode,district_df,variable,est) %>% 
  pivot_wider(names_from=variable,values_from=est)

oaxaca_df <- left_join(development_score,
                       outcomes,
                       by=c("level","survey","statecode","district_df")) %>% 
  dplyr::filter(level == "District") %>% 
  mutate(group = case_when(survey == "NFHS4" ~ 1,
                           survey == "NFHS5" ~ 0))

x_ids = c("pc1","S14","S16","S20",
            "S04","S09","S08",
            "S07","S10","S12")

n_outcomes = 5
results_t3 <- map_dfr(x_ids,
                      
                      function(x){
                        res_S81 = oaxaca(as.formula(paste0("S81 ~ ",x,"|group")),data=oaxaca_df,group.weights = 0.5);
                        res_S82 = oaxaca(as.formula(paste0("S82 ~ ",x,"|group")),data=oaxaca_df,group.weights = 0.5);
                        res_S84 = oaxaca(as.formula(paste0("S84 ~ ",x,"|group")),data=oaxaca_df,group.weights = 0.5);
                        res_S85 = oaxaca(as.formula(paste0("S85 ~ ",x,"|group")),data=oaxaca_df,group.weights = 0.5);
                        res_S92 = oaxaca(as.formula(paste0("S92 ~ ",x,"|group")),data=oaxaca_df,group.weights = 0.5);
                        
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

indicator_nfhs5s <- readxl::read_excel("data/mapping_ext.xlsx",sheet="nfhs5_state")

results_t3 %>% 
  dplyr::select(-coef.explained.,-se.explained.,
                -coef.unexplained.,-se.unexplained.) %>% 
  left_join(indicator_nfhs5s %>% 
              dplyr::select(ID,nfhs5s_description),
            by = c("ID")) %>%
  dplyr::filter(ID != "Intercept") %>% 
  mutate(nfhs5s_description = case_when(ID == "S20rev" ~ "Married before 18",
                                        ID == "pc1" ~ "Human development score",
                                        TRUE ~ nfhs5s_description)) %>% 
  arrange(outcome) %>% 
  write.csv(.,"analysis/blinder_oaxaca bivariate.csv")
