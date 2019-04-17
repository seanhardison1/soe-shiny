load(file.path(clean.dir,"1977_2017_SLI_Calfin_Pseudo_Ctyp.rdata"))
library(dplyr)
library(stringr)
library(tidyr)

zoo_anom <- Zooplankton_Primary_Prod %>% tidyr::gather(.,Var,Value,-year) %>% 
  dplyr::rename(Time = year) %>% 
  separate(.,Var,c("Var","EPU"), sep = "\\.") %>% 
  mutate(EPU, EPU = plyr::mapvalues(EPU, from = c("gbk","gom","mab","scs"),
                                    to = c("GB","GOM","MAB","SCS")),
         Units = "Anomaly") 

save(zoo_anom, file = file.path(clean.dir, "zooplankton_anomaly.Rdata"))


load(file.path(clean.dir, "Commerical_data_pull_19.Rdata"))

commercial <- commercial %>% dplyr::rename(EPU = Region) %>% dplyr::select(-Source)
save(commercial, file = file.path(clean.dir, "comdat_2019.Rdata"))


t <- matrix(data = rnorm(100), nrow = 10)
env_cor <-  cor(t)
env_order <- corrMatOrder(env_cor, order = "FPC")
env_out <- env_cor[env_order, env_order] #reorder

out <- soe %>%
  dplyr::filter(Var %in% c("bottom salinity anomaly in situ","nao index")) %>% 
  # {if (!input$EPU %in% 'all')
  #   dplyr::filter(., EPU %in% input$EPU) else .} %>% 
  group_by(Var, EPU) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  dplyr::filter(count >= 10,
                !str_detect(Var,"reference"),
                !is.na(Value)) %>%
  tidyr::unite(., Var, Var = c("Var","EPU"), sep = " ")

env_wide <- out %>%
  dplyr::select(-count) %>%
  # group_by(Var) %>% 
  # mutate(id=1:n()) %>% 
  tidyr::spread(., Var, Value, fill = NA) %>%
  dplyr::select(-Time)



#Get data
load(file.path(clean.dir, "SOE.data.Rdata"))

CF <- SOE.data %>% filter(stringr::str_detect(Var, "condition"))
ztrans <- function(x){
  meanx <- mean(x, na.rm = T)
  sdx   <- sd(  x, na.rm = T)
  z     <- (x - meanx) / sdx
  return(z)
}
#Processing
CF_norm <- CF %>%
  group_by(Var) %>% 
  mutate(Value = ztrans(Value)) #Normalize


save(CF_norm, file = file.path(clean.dir,"condition_factor.Rdata"))
