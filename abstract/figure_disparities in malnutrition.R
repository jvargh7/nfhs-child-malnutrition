source("analysis/indicators_consolidated.R")

district_df <- indicators %>% 
  dplyr::filter(level == "District",survey %in% c("NFHS4","NFHS5")) %>% 
  dplyr::filter(variable %in% c("S84","S85")) %>% 
  dplyr::select(district_df,variable,survey,est) %>% 
  pivot_wider(names_from = survey,values_from=est) %>% 
  mutate(difference = NFHS5 - NFHS4) %>% 
  dplyr::select(district_df,variable,difference) %>%
  pivot_wider(names_from=variable,values_from=difference)

cor.test(district_df$S84,district_df$S85)

table(district_df$S84>0,district_df$S85>0)

ggplot(data=district_df,aes(x=S84,y=S85)) +
  geom_point() +
  xlab("Underweight") +
  ylab("Overweight") +
  scale_x_continuous(limits=c(-20,20)) +
  scale_y_continuous(limits=c(-20,20)) +
  theme_bw()
