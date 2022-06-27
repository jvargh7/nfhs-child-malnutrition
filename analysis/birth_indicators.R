require(srvyr)
source("analysis/indicator_summary function.R")

dfs <- c("n3_birth","n4_birth","n5_birth")
birth_vars <- c("S04")
birth_indicators <- map_dfr(dfs,function(df){
  
  paste0("Running ",df)
  birth_df <- readRDS(paste0(path_nfhs_malnutrition_paper,"/working/",df,".RDS"))
  indicator_summary(birth_df,df,birth_vars) %>% 
    return(.)
  
})

map_dfr(paste0(birth_vars,"$"),
        function(v){
          svymean_wide2long(birth_indicators,
                            prefix = v,
                            id_cols = c("level","statecode","survey","district_df"),
                            type = "ci")
          
        }) %>% 
  write_csv(.,paste0("analysis/birth_indicators.csv"))

