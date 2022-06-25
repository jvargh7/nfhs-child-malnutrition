
ds_ids = c("S14","S16","S20",
           "S04","S09","S08",
           "S07","S10","S12")

version = "2021-01-13"
pca_obj <- read_dta(paste0(papers_path,"/worsening despite wash/district all indicators wide_",version,".dta"))  %>% 
  dplyr::filter(round == "NFHS-4") %>% 
  dplyr::select(nfhs5_state,nfhs4_district,one_of(ds_ids)) %>%
  dplyr::select(-nfhs5_state,-nfhs4_district) %>% 
  prcomp(.,scale. = TRUE)

# View(pca_obj$x)

nfhs4_d <- read_dta(paste0(papers_path,"/worsening despite wash/district all indicators wide_",version,".dta"))  %>% 
  dplyr::filter(round == "NFHS-4") %>% 
  dplyr::select(nfhs5_state,nfhs4_district,one_of(ds_ids)) %>%
  dplyr::select(-nfhs5_state,-nfhs4_district) %>% 
  predict(pca_obj,.)

nfhs4_s <- read_dta(paste0(papers_path,"/worsening despite wash/state all indicators wide_",version,".dta"))  %>% 
  dplyr::filter(round == "NFHS-4") %>% 
  dplyr::select(nfhs5_state,one_of(ds_ids)) %>%
  dplyr::select(-nfhs5_state) %>% 
  predict(pca_obj,.)

nfhs5_d <- read_dta(paste0(papers_path,"/worsening despite wash/district all indicators wide_",version,".dta"))  %>% 
  dplyr::filter(round == "NFHS-5") %>% 
  dplyr::select(nfhs5_state,nfhs4_district,one_of(ds_ids)) %>%
  dplyr::select(-nfhs5_state,-nfhs4_district) %>% 
  predict(pca_obj,.)

nfhs5_s <- read_dta(paste0(papers_path,"/worsening despite wash/state all indicators wide_",version,".dta"))  %>% 
  dplyr::filter(round == "NFHS-5") %>% 
  dplyr::select(nfhs5_state,one_of(ds_ids)) %>%
  dplyr::select(-nfhs5_state) %>% 
  predict(pca_obj,.)


state_pc <- read_dta(paste0(papers_path,"/worsening despite wash/state all indicators wide_",version,".dta"))  %>% 
  dplyr::filter(round == "NFHS-4") %>% 
  dplyr::select(nfhs5_state) %>% 
  mutate(nfhs4s_pc1 = nfhs4_s[,1]*-1,
         nfhs5s_pc1 = nfhs5_s[,1]*-1
         )

district_pc <- read_dta(paste0(papers_path,"/worsening despite wash/district all indicators wide_",version,".dta"))  %>% 
  dplyr::filter(round == "NFHS-4") %>% 
  dplyr::select(nfhs5_state,nfhs4_district) %>%
  mutate(nfhs4d_pc1 = nfhs4_d[,1]*-1,
         nfhs5d_pc1 = nfhs5_d[,1]*-1
  )

write_dta(state_pc ,paste0(papers_path,"/worsening despite wash/state development score_",Sys.Date(),".dta"),version=12)
write_dta(district_pc ,paste0(papers_path,"/worsening despite wash/district development score_",Sys.Date(),".dta"),version=12)
