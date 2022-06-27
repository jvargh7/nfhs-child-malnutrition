library(tidyverse)
source("analysis/indicators_consolidated.R")

nnm_ids = c("S81",
            "S82",
            "S84", 
            "S85",
            
            
            "S14","S16","S20",
            "S04",
            "S09","S08",
            "S07","S10","S12")
annualized_change <- indicators %>% 
  dplyr::filter(level %in% c("State"),variable %in% nnm_ids) %>% 
  bind_rows(
    {.} %>% 
      dplyr::filter(survey == "NFHS3",statecode == "AP") %>% 
      mutate(statecode = "TG")) %>% 
  dplyr::select(level,variable,statecode,survey,est) %>% 
  pivot_wider(names_from=c("survey"),values_from=c("est")) %>% 
  mutate(change_N3to4 = (NFHS4 - NFHS3)/diff3to4,
         change_N4to5 = (NFHS5 - NFHS4)/diff4to5) %>% 
  dplyr::filter(!is.na(change_N3to4)) %>% 
  arrange(level)


# SFIG 2----------
library(ggpubr)
plot_list_sfig2 <- list()
for(id in nnm_ids[1:4]){
  
  plot_list_sfig2[[id]] <- annualized_change %>% 
    dplyr::filter(variable == id) %>% 
    ggplot(data=.,aes(x=change_N3to4,y=change_N4to5))+
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
          ncol=2,plotlist = plot_list_sfig2) %>% 
  ggsave(.,filename = paste0(path_nfhs_malnutrition_paper,"/figures/scatterplot annualized outcomes.png"))


# SFIG 3----------
library(ggpubr)
plot_list_sfig3 <- list()
for(id in nnm_ids[5:13]){
  
  plot_list_sfig3[[id]] <- annualized_change %>% 
    dplyr::filter(variable == id) %>% 
    ggplot(data=.,aes(x=change_N3to4,y=change_N4to5))+
    geom_point() +
    geom_abline(intercept=c(0,0),slope=1) +
    # ggtitle(id) +
    scale_x_continuous(limits=c(0,10)) +
    scale_y_continuous(limits=c(-10,10)) +
    theme_bw() +
    xlab("Change 2006-15 (% p.a.)") +
    ylab("Change 2015-19 (% p.a.)") 
  
  
}

ggarrange(labels = LETTERS[1:9],
          nrow=3, 
          ncol=3,plotlist = plot_list_sfig3) %>% 
  ggsave(.,filename = paste0(path_nfhs_malnutrition_paper,"/figures/scatterplot annualized development.png"),width=8,height=8)
