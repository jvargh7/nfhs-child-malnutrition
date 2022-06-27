require(srvyr)
source("analysis/indicator_summary function.R")

dfs <- c("n3_individual","n4_individual","n5_individual")
individual_vars <- c("S14","S16","S20")

individual_indicators <- map_dfr(dfs,function(df){
  
  paste0("Running ",df)
  individual_df <- readRDS(paste0(path_nfhs_malnutrition_paper,"/working/",df,".RDS"))
  indicator_summary(individual_df,df,individual_vars) %>% 
    return(.)
  
})

map_dfr(paste0(individual_vars,"$"),
        function(v){
          svymean_wide2long(individual_indicators,
                            prefix = v,
                            id_cols = c("level","statecode","survey","district_df"),
                            type = "ci")
          
        }) %>% 
  write_csv(.,paste0("analysis/individual_indicators.csv"))


