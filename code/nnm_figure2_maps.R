library(sp)
library(rgdal)
library(tmap)
path_shape_files <- "C:/Cloud/OneDrive - Emory University/data/NFHS/NFHS4 Factsheets/maps"
shape_df <-  readOGR(paste0(path_shape_files,"/sdr_subnational_boundaries_2020-12-28/shps"),"sdr_subnational_boundaries2")
bound_df <- readOGR(paste0(path_shape_files,"/maps-master/States"),"Admin2")



tmap_plot <- function(id,tmap_title){
  
  # palette="RdYlGn"
  version = "2021-01-13"
  title = tmap_title
  # title = LETTERS[1]
  papers_path <- "C:/Cloud/OneDrive - Emory University/data/NFHS/NFHS5 Papers"
  
  district_df <- read_dta(paste0(papers_path,"/worsening despite wash/district all indicators wide_",version,".dta")) %>% 
    mutate(variable = case_when(round == "NFHS-4" ~ "nfhs4d_total",
                                TRUE ~ "nfhs5d_total")) %>% 
    pivot_longer(cols=-one_of("nfhs5_state","nfhs4_district","variable","round","sdistri_nfhs5to4"),names_to ="ID",values_to="value") %>% 
    
    dplyr::select(-round) %>% 
    pivot_wider(names_from = "variable",values_from="value") %>% 
    left_join(readxl::read_excel(paste0(extracts_path,"/mapping.xlsx"),sheet="nfhs5_district") %>% 
                dplyr::select(ID,nfhs5d_description) %>% 
                distinct(ID,.keep_all=TRUE) %>% 
                dplyr::select(ID,nfhs5d_description),
              by = c("ID"))
  
  state_df <- read_dta(paste0(papers_path,"/worsening despite wash/state all indicators wide_",version,".dta")) %>% 
    mutate(variable = case_when(round == "NFHS-4" ~ "nfhs4s_total",
                                round == "NFHS-3" ~ "nfhs3s_total",
                                TRUE ~ "nfhs5s_total")) %>% 
    pivot_longer(cols=-one_of("nfhs5_state","variable","round","region"),names_to ="ID",values_to="value") %>% 
    
    dplyr::select(-round,-region) %>% 
    pivot_wider(names_from = "variable",values_from="value") %>% 
    left_join(readxl::read_excel(paste0(extracts_path,"/mapping.xlsx"),sheet="nfhs5_state") %>% 
                dplyr::select(ID,nfhs5s_description) %>% 
                distinct(ID,.keep_all=TRUE) %>% 
                dplyr::select(ID,nfhs5s_description),
              by = c("ID"))
  
  
  ut_data = state_df %>% 
    dplyr::filter(ID == id,nfhs5_state %in% c("Lakshadweep","Chandigarh")) %>% 
    mutate(status = case_when(ID == "S47" & (nfhs5s_total  > nfhs4s_total + 250) ~ 0,
                              nfhs5s_total  > nfhs4s_total ~ 0,
                              ID == "S47" & (nfhs5s_total + 250 < nfhs4s_total) ~ 2,
                              nfhs5s_total + 7 < nfhs4s_total ~ 2,
                              !is.na(nfhs5s_total) | !is.na(nfhs4s_total) ~ 1,
                              TRUE ~ NA_real_)) %>% 
    rename(nfhs5d_total = nfhs5s_total,
           nfhs4d_total = nfhs4s_total) %>% 
    mutate(sdistri_nfhs5to4 = case_when(nfhs5_state == "Lakshadweep" ~ 587,
                                        nfhs5_state == "Chandigarh" ~ 55,
                                        TRUE ~ NA_real_))
  
  district_data = district_df %>% 
    dplyr::filter(ID == id) %>% 
    mutate(status = case_when(ID == "S47" & (nfhs5d_total  > nfhs4d_total + 250) ~ 0,
                              nfhs5d_total  > nfhs4d_total ~ 0,
                              ID == "S47" & (nfhs5d_total + 250 < nfhs4d_total) ~ 2,
                              nfhs5d_total + 7 < nfhs4d_total ~ 2,
                              !is.na(nfhs5d_total) | !is.na(nfhs4d_total) ~ 1,
                              TRUE ~ NA_real_)) %>% 
    bind_rows(ut_data) %>% 
    mutate(status = factor(status,levels=c(0:2),labels=c("Increased","Decreased 0-7 pp","Decreased > 7pp")))
  
    
  
  shape_df2 <- sp::merge(shape_df,district_data[,c("sdistri_nfhs5to4","status")],
                         by.x="REGCODE",by.y="sdistri_nfhs5to4",all.x=TRUE)
  
  # https://gis.stackexchange.com/questions/310485/how-to-i-limit-the-x-and-y-axes-with-tmap-in-r
  a <- tm_shape(shape_df2,ext=1.2) + 
    tm_borders() + tm_fill(title= "",
                           col="status",
                           palette="RdYlGn",
                           # breaks=d_breaks,
                           # midpoint = NA,
                           textNA="Data not available",
                           colorNA = "white") + 
    tm_shape(bound_df) + tm_borders(col="black") + 
    tm_text(text="ST_NM",col="black",size=0.5,remove.overlap = TRUE)+
    tm_legend(legend.position = c("right","top"),
              legend.outside=FALSE,
              legend.just=c("left","top"))+ 
    tm_xlab("") +
    tm_ylab("")  
  # https://stackoverflow.com/questions/32890762/how-to-manipulate-tmap-legend
  a <- a + tm_layout(toupper(str_replace(title,"_","-")),title.size = 2,
                     legend.text.size = 1.5,
                     legend.title.size = 1.5)
  png_name <- paste0(tmap_title,
                     "7pp.png")
  tmap_save(a,paste0(papers_path,"/worsening despite wash/figures/",png_name),height=2300/300)


}

tmap_plot(id="S81",tmap_title = "A")
tmap_plot(id="S82",tmap_title = "B")
tmap_plot(id="S84",tmap_title = "C")
tmap_plot(id="S92",tmap_title = "E")
tmap_plot(id="S85",tmap_title = "D")




