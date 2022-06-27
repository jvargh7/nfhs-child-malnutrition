source("analysis/indicators_consolidated.R")
require(ggplot2)
require(ggpubr)
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




# SFIG 4----------
plot_list_sfig4 <- list()
for(id in nnm_ids[1:4]){
  
  plot_list_sfig4[[id]] <- annualized_change %>% 
    dplyr::filter(variable == id) %>%
    pivot_longer(cols=contains("change"),names_to="var",values_to="annualized") %>% 
    dplyr::select(statecode,var,annualized) %>% 
    mutate(var = case_when(var == "change_N3to4" ~ "NFHS-3 to NFHS-4",
                              var == "change_N4to5" ~ "NFHS-4 to NFHS-5",
                              TRUE ~ NA_character_)) %>% 
    ggplot(data=.,aes(y=annualized,group=var,fill=var))+
    geom_boxplot() +
    geom_hline(aes(yintercept=0),col="red",linetype=2) +
    # ggtitle(id) +
    scale_y_continuous(limits=c(-2.5,2.5)) +
    theme_bw() +
    xlab("") +
    ylab("Annualized change (% p.a.)") +
    scale_fill_discrete(name="")
  
  
}

ggarrange(labels = LETTERS[1:4],
          nrow=2,
          ncol=2,plotlist = plot_list_sfig4,
          common.legend=TRUE) %>% 
  ggsave(.,filename = paste0(path_nfhs_malnutrition_paper,"/figures/boxplot annualized outcomes.png"))


# SFIG 5----------
plot_list_sfig5 <- list()
for(id in nnm_ids[5:13]){
  
  plot_list_sfig5[[id]] <- annualized_change %>% 
    dplyr::filter(variable == id) %>%
    pivot_longer(cols=contains("change"),names_to="var",values_to="annualized") %>% 
    dplyr::select(statecode,var,annualized) %>% 
    mutate(var = case_when(var == "change_N3to4" ~ "NFHS-3 to NFHS-4",
                           var == "change_N4to5" ~ "NFHS-4 to NFHS-5",
                           TRUE ~ NA_character_)) %>% 
    ggplot(data=.,aes(y=annualized,group=var,fill=var))+
    geom_boxplot() +
    geom_hline(aes(yintercept=0),col="red",linetype=2) +
    # ggtitle(id) +
    scale_y_continuous(limits=c(-2.5,2.5)) +
    theme_bw() +
    xlab("") +
    ylab("Annualized change (% p.a.)") +
    scale_fill_discrete(name="")
  
  
}

ggarrange(labels = LETTERS[1:9],
          nrow=3,
          ncol=3,plotlist = plot_list_sfig5,
          common.legend=TRUE) %>% 
  ggsave(.,filename = paste0(path_nfhs_malnutrition_paper,"/figures/boxplot annualized development.png"),width=8,height=8)

