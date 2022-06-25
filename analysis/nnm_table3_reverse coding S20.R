library(oaxaca)
version = "2021-01-13"

district_pc <- read_dta(paste0(papers_path,"/worsening despite wash/district development score_2021-01-22.dta")) %>% 
  pivot_longer(cols=contains("pc1"),names_to = "round",values_to="development_score") %>% 
  mutate(round = case_when(round == "nfhs4d_pc1" ~ "NFHS-4",
                           round == "nfhs5d_pc1" ~ "NFHS-5",
                           TRUE ~ NA_character_))

district_wide = read_dta(paste0(papers_path,"/worsening despite wash/district all indicators wide_", version,".dta")) %>% 
  mutate(group = case_when(round == "NFHS-5" ~ 0,
                           TRUE ~ 1)) %>%
  mutate(S20 = case_when(is.na(S20) ~ NA_real_,
                         TRUE ~ 100-S20)) %>% 
  left_join(district_pc,by=c("nfhs5_state","nfhs4_district","round"))

n_outcomes = 5

results_t3c <- map_dfr(c("S20"),
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


results_t3c %>% 
  dplyr::select(-coef.explained.,-se.explained.,
                -coef.unexplained.,-se.unexplained.) %>% 
  write.csv(.,paste0(papers_path,"/worsening despite wash/tables/blinder_oaxaca bivariate S20.csv"))
