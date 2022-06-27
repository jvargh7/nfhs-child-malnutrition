require(srvyr)
source("analysis/indicator_summary function.R")

dfs <- c("n3_household","n4_household","n5_household")
household_vars <- c("S12","S10")

household_indicators <- map_dfr(dfs,function(df){
  
  paste0("Running ",df)
  household_df <- readRDS(paste0(path_nfhs_malnutrition_paper,"/working/",df,".RDS"))
  indicator_summary(household_df,df,household_vars) %>% 
    return(.)
  
})

map_dfr(paste0(household_vars,"$"),
        function(v){
          svymean_wide2long(household_indicators,
                            prefix = v,
                            id_cols = c("level","statecode","survey","district_df"),
                            type = "ci")
          
        }) %>% 
  write_csv(.,paste0("analysis/household_indicators.csv"))

