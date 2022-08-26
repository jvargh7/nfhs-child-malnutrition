require(sp)
require(rgdal)
require(tmap)
require(tidyverse)
path_shape_files <- "C:/Cloud/OneDrive - Emory University/data/NFHS/NFHS4 Factsheets/maps"
shape_df <-  readOGR(paste0(path_shape_files,"/sdr_subnational_boundaries_2020-12-28/shps"),"sdr_subnational_boundaries2")
shape_df@data <- shape_df@data %>% 
  mutate(REGCODE = case_when(REGNAME == "Hamirpur" & OTHREGNA == "Uttar Pradesh" ~ 168,
                             TRUE ~ REGCODE))
  
bound_df <- readOGR(paste0(path_shape_files,"/maps-master/States"),"Admin2")

source("analysis/indicators_consolidated.R")


tmap_plot <- function(id,tmap_title = "A"){

  title = tmap_title

  district_df <- indicators %>% 
    dplyr::filter(level == "District",survey %in% c("NFHS4","NFHS5")) %>% 
    dplyr::filter(variable == id) %>% 
    dplyr::select(district_df,survey,est) %>% 
    pivot_wider(names_from = survey,values_from=est) %>% 
    mutate(status = case_when(
                              NFHS5  > NFHS4 ~ 0,
                              NFHS5 + 7 < NFHS4 ~ 2,
                              !is.na(NFHS5) | !is.na(NFHS4) ~ 1,
                              TRUE ~ NA_real_)) %>% 
    mutate(status = factor(status,levels=c(0:2),labels=c("Increased","Decreased 0-7 PP","Decreased > 7 PP")))
  
    
  
  shape_df2 <- sp::merge(shape_df,district_df[,c("district_df","status")],
                         by.x="REGCODE",by.y="district_df",all.x=TRUE)

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
  tmap_save(a,paste0(path_nfhs_malnutrition_paper,"/figures/",png_name),height=2300/300)


}

tmap_plot(id="S81",tmap_title = "A")
tmap_plot(id="S82",tmap_title = "B")
tmap_plot(id="S84",tmap_title = "C")
tmap_plot(id="S92",tmap_title = "E")
tmap_plot(id="S85",tmap_title = "D")




