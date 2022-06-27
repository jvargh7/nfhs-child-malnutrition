require(srvyr)
source("analysis/indicator_summary function.R")

dfs <- c("n3_population","n4_population","n5_population")
population_vars <- c("S07","S08","S09")

population_indicators <- map_dfr(dfs,function(df){
  
  paste0("Running ",df)
  population_df <- readRDS(paste0(path_nfhs_malnutrition_paper,"/working/",df,".RDS"))
  indicator_summary(population_df,df,population_vars) %>% 
    return(.)
  
})

map_dfr(paste0(population_vars,"$"),
        function(v){
          svymean_wide2long(population_indicators,
                            prefix = v,
                            id_cols = c("level","statecode","survey","district_df"),
                            type = "ci")
          
        }) %>% 
  write_csv(.,paste0("analysis/population_indicators.csv"))

