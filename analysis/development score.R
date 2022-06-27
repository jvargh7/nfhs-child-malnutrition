source("analysis/indicators_consolidated.R")
ds_ids = c("S14","S16","S20",
           "S04","S09","S08",
           "S07","S10","S12")

pca_obj <- indicators %>%
  dplyr::filter(level == "District",survey == "NFHS4",variable %in% ds_ids) %>% 
  dplyr::filter(!is.na(est)) %>% 
  dplyr::select(district_df,variable,est) %>% 
  pivot_wider(names_from=variable,values_from=est) %>%
  dplyr::select(-district_df) %>% 
  prcomp(.,scale. = TRUE)

saveRDS(pca_obj,"analysis/pca_obj.RDS")
# View(pca_obj$x)

nfhs4d_pc <- indicators %>%
  dplyr::filter(level == "District",survey == "NFHS4",variable %in% ds_ids) %>% 
  dplyr::filter(!is.na(est)) %>% 
  dplyr::select(district_df,variable,est) %>% 
  pivot_wider(names_from=variable,values_from=est) %>% 
  predict(pca_obj,.)

nfhs4s_pc <- indicators %>%
  dplyr::filter(level == "State",survey == "NFHS4",variable %in% ds_ids) %>% 
  dplyr::filter(!is.na(est)) %>% 
  dplyr::select(statecode,variable,est) %>% 
  pivot_wider(names_from=variable,values_from=est) %>% 
  predict(pca_obj,.)

nfhs5d_pc <- indicators %>%
  dplyr::filter(level == "District",survey == "NFHS5",variable %in% ds_ids) %>% 
  dplyr::filter(!is.na(est)) %>% 
  dplyr::select(district_df,variable,est) %>% 
  pivot_wider(names_from=variable,values_from=est) %>% 
  predict(pca_obj,.)

nfhs5s_pc <- indicators %>%
  dplyr::filter(level == "State",survey == "NFHS5",variable %in% ds_ids) %>% 
  dplyr::filter(!is.na(est)) %>% 
  dplyr::select(level,survey,statecode,variable,est) %>% 
  pivot_wider(names_from=variable,values_from=est) %>% 
  predict(pca_obj,.)


nfhs4d <- indicators %>%
  dplyr::filter(level == "District",survey == "NFHS4",variable %in% ds_ids) %>% 
  dplyr::filter(!is.na(est)) %>% 
  dplyr::select(level,survey,district_df,variable,est) %>% 
  pivot_wider(names_from=variable,values_from=est) %>% 
  mutate(pc1 = nfhs4d_pc[,1]*-1)

nfhs4s <- indicators %>%
  dplyr::filter(level == "State",survey == "NFHS4",variable %in% ds_ids) %>% 
  dplyr::filter(!is.na(est)) %>% 
  dplyr::select(level,survey,statecode,variable,est) %>% 
  pivot_wider(names_from=variable,values_from=est) %>% 
  mutate(pc1 = nfhs4s_pc[,1]*-1)

nfhs5d <- indicators %>%
  dplyr::filter(level == "District",survey == "NFHS5",variable %in% ds_ids) %>% 
  dplyr::filter(!is.na(est)) %>% 
  dplyr::select(level,survey,district_df,variable,est) %>% 
  pivot_wider(names_from=variable,values_from=est) %>% 
  mutate(pc1 = nfhs5d_pc[,1]*-1)

nfhs5s <- indicators %>%
  dplyr::filter(level == "State",survey == "NFHS5",variable %in% ds_ids) %>% 
  dplyr::filter(!is.na(est)) %>% 
  dplyr::select(level,survey,statecode,variable,est) %>% 
  pivot_wider(names_from=variable,values_from=est) %>% 
  mutate(pc1 = nfhs5s_pc[,1]*-1)

bind_rows(nfhs4d,
          nfhs5d,
          nfhs4s,
          nfhs5s) %>% 
  write_csv(.,"analysis/development score.csv")


