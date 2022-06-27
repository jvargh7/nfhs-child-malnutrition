require(srvyr)
source("analysis/indicator_summary function.R")

dfs <- c("n3_child","n4_child","n5_child")
child_vars <- c("S81","S82","S83","S84","S85","S92")


child_indicators <- map_dfr(dfs,function(df){
  
  paste0("Running ",df)
  child_df <- readRDS(paste0(path_nfhs_malnutrition_paper,"/working/",df,".RDS"))
  indicator_summary(child_df,df,child_vars) %>% 
    return(.)
  
})

map_dfr(paste0(child_vars,"$"),
        function(v){
          svymean_wide2long(child_indicators,
                            prefix = v,
                            id_cols = c("level","statecode","survey","district_df"),
                            type = "ci")
          
        }) %>% 
  write_csv(.,paste0("analysis/child_indicators.csv"))


