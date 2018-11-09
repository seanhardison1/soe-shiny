
V <- c("bottom temp anomaly in situ")
E <- c("all")


V1 <- c("bottom temp anomaly in situ")
E1 <- c("GOM")
k1 <- 0

nolag <- soe %>%
  dplyr::filter(Var %in% V) %>% 
  {if (any(!E %in% 'all'))
    dplyr::filter(., EPU %in% E) else .} %>% 
  group_by(Var, EPU) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  filter(count >= 10,
         !str_detect(Var,"reference"),
         !is.na(Value)) %>%
  tidyr::unite(., Var, Var = c("Var","EPU"), sep = " ")



lag1 <- soe %>%
  dplyr::filter(Var %in% V1) %>%
  {if (any(!'all' %in% E1))
    dplyr::filter(., EPU %in% E1) else .} %>%
  group_by(Var, EPU) %>%
  mutate(count = n()) %>%
  do(lag_series(., k = k1)) %>%
  ungroup() %>%
  mutate(Var = paste(.$Var,"lag",k1)) %>%
  filter(count >= 10,
         !str_detect(Var,"reference")) %>%
  tidyr::unite(., Var, Var = c("Var","EPU"), sep = " ") #%>% 
 # filter(str_detect(Var, E1))




if (E == "" || V == "") {
  int_corr <- nolag
} else {
  int_corr <- rbind(lag1, nolag)  
}

env_wide <- int_corr %>% 
  dplyr::select(-count) %>%
  tidyr::spread(., Var, Value) %>% 
  dplyr::select(-Time) 

env_wide <- env_wide[complete.cases(env_wide),]

#Find correlation matrix and order by magnitude of first principal component
env_cor <- cor(env_wide)
env_order <- corrMatOrder(env_cor, order = "FPC")
env_out <- env_cor[env_order, env_order] #reorder


d3heatmap(env_out)
