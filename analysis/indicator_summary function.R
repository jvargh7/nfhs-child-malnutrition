indicator_summary <- function(dataset,name_df,summary_vars){
  
  india <- dataset %>%
    as_survey_design(ids = psu,strata = state,
                     weight = weight,
                     nest = FALSE,
                     variance = "YG",pps = "brewer")  %>%
    summarize_at(vars(one_of(summary_vars)),~survey_mean(.,vartype="ci",na.rm=TRUE)) %>% 
    mutate(level = "India");
  
  state <- dataset %>%
    as_survey_design(ids = psu,strata = state,
                     weight = weight,
                     nest = FALSE,
                     variance = "YG",pps = "brewer")  %>%
    group_by(statecode) %>%
    summarize_at(vars(one_of(summary_vars)),~survey_mean(.,vartype="ci",na.rm=TRUE)) %>% 
    mutate(level = "State");
  
  urban <- dataset %>%
    dplyr::filter(residence == 1) %>% 
    as_survey_design(ids = psu,strata = state,
                     weight = weight,
                     nest = FALSE,
                     variance = "YG",pps = "brewer")  %>%
    summarize_at(vars(one_of(summary_vars)),~survey_mean(.,vartype="ci",na.rm=TRUE)) %>% 
    mutate(level = "Urban");
  
  rural <- dataset %>%
    dplyr::filter(residence == 2) %>% 
    as_survey_design(ids = psu,strata = state,
                     weight = weight,
                     nest = FALSE,
                     variance = "YG",pps = "brewer")  %>%
    summarize_at(vars(one_of(summary_vars)),~survey_mean(.,vartype="ci",na.rm=TRUE)) %>% 
    mutate(level = "Rural");
  
  state_urban <- dataset %>%
    dplyr::filter(residence == 1) %>% 
    as_survey_design(ids = psu,strata = state,
                     weight = weight,
                     nest = FALSE,
                     variance = "YG",pps = "brewer")  %>%
    group_by(statecode) %>%
    summarize_at(vars(one_of(summary_vars)),~survey_mean(.,vartype="ci",na.rm=TRUE)) %>% 
    mutate(level = "State_Urban");
  
  state_rural <- dataset %>%
    dplyr::filter(residence == 2) %>% 
    as_survey_design(ids = psu,strata = state,
                     weight = weight,
                     nest = FALSE,
                     variance = "YG",pps = "brewer")  %>%
    group_by(statecode) %>%
    summarize_at(vars(one_of(summary_vars)),~survey_mean(.,vartype="ci",na.rm=TRUE)) %>% 
    mutate(level = "State_Rural");
  
  district <- data.frame(level = "District");
  
  if(!str_detect(name_df,"n3_")){
    district <- dataset %>%
      as_survey_design(ids = psu,strata = state,
                       weight = weight,
                       nest = FALSE,
                       variance = "YG",pps = "brewer")  %>%
      group_by(district_df) %>%
      summarize_at(vars(one_of(summary_vars)),~survey_mean(.,vartype="ci",na.rm=TRUE)) %>% 
      mutate(level = "District")};
  
  bind_rows(india,
            state,
            urban,
            rural,
            state_urban,
            state_rural,
            district) %>%
    mutate(survey = name_df) %>% 
    return(.)
  
}


