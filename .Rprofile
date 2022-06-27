round_d <- function(x,d=2){
  return(sprintf(paste0("%0.",d,"f"),round(x,d)))
  
}

path_repo <- "C:/code/external/nfhs-child-malnutrition"
path_nfhs5_mapping <- "C:/Cloud/OneDrive - Emory University/data/NFHS/NFHS5 Mapping"
path_nfhs_malnutrition_paper <- "C:/Cloud/OneDrive - Emory University/Papers/NFHS5 Worsening Despite Wash"
path_dhs_folder <- "C:/Cloud/OneDrive - Emory University/data/dhs_program"
options(scipen=999)

v024 <- readxl::read_excel("data/mapping_ext.xlsx",sheet="v024")
sdist <- readr::read_csv("C:/code/external/nfhs5_on_map2016/data/psu_on_map2016.csv")

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

source(list.files("C:/code/external/functions/survey",full.names = TRUE))

diff3to4 = 10
diff4to5 = 5

p_val_format = function(p_val){
  case_when(
    # p_val < 0.001 ~ paste0("<0.001"),
    p_val < 0.01 ~ paste0("p<0.01"),
    TRUE ~ paste0("p=",p_val %>% round(.,2)))}
