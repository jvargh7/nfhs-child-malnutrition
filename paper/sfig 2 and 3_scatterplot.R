library(tidyverse)
nnm_ids = c("S81",
            "S82",
            "S84", 
            "S85",
            
            
            "S14","S16","S20",
            # "S04",
            "S09","S08",
            "S07","S10","S12")
state_df <- haven::read_dta(paste0(path_repo,"/data/state all indicators wide_2021-01-13.dta")) %>% 
  mutate(variable = case_when(round == "NFHS-4" ~ "nfhs4s_total",
                              round == "NFHS-3" ~ "nfhs3s_total",
                              TRUE ~ "nfhs5s_total")) %>% 
  mutate(S20 = case_when(is.na(S20) ~ NA_real_,
                         TRUE ~ 100-S20)) %>% 
  pivot_longer(cols=-one_of("nfhs5_state","variable","round","region"),names_to ="ID",values_to="value") %>% 
  
  dplyr::select(-round,-region) %>% 
  pivot_wider(names_from = "variable",values_from="value") %>% 
  left_join(readxl::read_excel(paste0(path_nfhs5_mapping,"/mapping.xlsx"),sheet="nfhs5_state") %>% 
              dplyr::select(ID,nfhs5s_description) %>% 
              distinct(ID,.keep_all=TRUE) %>% 
              dplyr::select(ID,nfhs5s_description),
            by = c("ID")) %>% 
  dplyr::filter(ID %in% nnm_ids) %>% 
  dplyr::filter(!is.na(nfhs5s_total),!is.na(nfhs4s_total),!is.na(nfhs3s_total)) %>% 
  mutate(ann3to4 = (nfhs4s_total-nfhs3s_total)/9,
         ann4to5 = (nfhs5s_total-nfhs4s_total)/4)


# SFIG 2----------
library(ggpubr)
plot_list_sfig2 <- list()
for(id in nnm_ids[1:4]){
  
  plot_list_sfig2[[id]] <- state_df %>% 
    dplyr::filter(ID == id) %>% 
    ggplot(data=.,aes(x=ann3to4,y=ann4to5))+
    geom_point() +
    geom_abline(intercept=c(0,0),slope=1) +
    # ggtitle(id) +
    scale_x_continuous(limits=c(-2,2)) +
    scale_y_continuous(limits=c(-2.5,2.5)) +
    theme_bw() +
    xlab("Change 2006-15 (% p.a.)") +
    ylab("Change 2015-19 (% p.a.)") 
  
  
}

ggarrange(labels = LETTERS[1:4],
          nrow=2,
          ncol=2,plotlist = plot_list_sfig2)


# SFIG 3----------
library(ggpubr)
plot_list_sfig3 <- list()
for(id in nnm_ids[5:12]){
  
  plot_list_sfig3[[id]] <- state_df %>% 
    dplyr::filter(ID == id) %>% 
    ggplot(data=.,aes(x=ann3to4,y=ann4to5))+
    geom_point() +
    geom_abline(intercept=c(0,0),slope=1) +
    # ggtitle(id) +
    scale_x_continuous(limits=c(0,10)) +
    scale_y_continuous(limits=c(-10,10)) +
    theme_bw() +
    xlab("Change 2006-15 (% p.a.)") +
    ylab("Change 2015-19 (% p.a.)") 
  
  
}

ggarrange(labels = LETTERS[1:8],
          nrow=2,
          ncol=4,plotlist = plot_list_sfig3)
