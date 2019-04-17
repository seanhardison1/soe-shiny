# Code to prepare data for Indicator Visualization App. Should not be run by app.
library(dplyr)
library(magrittr)
library(stringr)

# Data directory
clean.dir <- here::here("data")

# Load all
invisible(lapply(file.path(clean.dir,as.list(list.files(clean.dir))),load,environment()))

zoo %<>% dplyr::select(-sd.high, -sd.low, -Season) %>%
  dplyr::rename(EPU = epu) %>% 
  mutate(Units = "log N")

soe <- rbind(hp_bycatch,NE_LME_nutrients,
             ocean_sal_insitu, ocean_temp_insitu, 
             slopewater,
             stratification,wind_clean,zoo,
             zoo_anom, commercial,
             mean_sml_per_lrg_anom,
             species_sml_per_lrg)



#NAO data is already normalized 
nao <- nao %>% dplyr::select(-Units)
CF_norm <- CF_norm %>% dplyr::select(-Units) %>% ungroup() %>% as.data.frame()
soe <- soe %>% mutate(EPU, EPU = plyr::mapvalues(EPU, from = "SCS", to = "SS"))
# sli <- zoo_anom %>% filter(Var == "SLI") %>% dplyr::select(-Units)
# soe <- soe %>% filter(Var != "SLI")

# Get variable names
var_ids <- sort(as.factor(na.omit(unique(soe$Var))))
epu_ids <- sort(as.factor(na.omit(unique(soe$EPU))))

# Normalize and difference
get_dat <- function(field){
  
  group <- soe %>% dplyr::filter(Var == field)
  print(field)
  out <- NULL
  for (i in unique(group$EPU)){
    
    Value <- group %>% dplyr::filter(EPU == i) %>% dplyr::pull(Value)
    Time <- group %>% dplyr::filter(EPU == i) %>% dplyr::pull(Time)
    
    if (all(is.na(as.numeric(Value))) | sd(Value, na.rm = T) == 0 | length(Value) < 20){
      Value <- NA #Assign as NA if not including
    } else {
      
      Value <- Value[1:length(Time)]
      Value <- (Value-mean(Value, na.rm = TRUE))/sd(Value, na.rm = TRUE) #normalize
      
      #interpolate missing values
      if (any(is.na(Value))){
        Value <- approx(Time, Value, n = length(Value))$y
      }
      
      #test for stationarity with Augmented Dickey Fuller
      adf <- suppressWarnings(tseries::adf.test(Value)$p.value)
      
      if (adf > 0.05){ #if non-stationary take first difference and use residuals
        mod <- arima(Value, order = c(0,1,0))
        Value <- resid(mod)
        
        adf2 <- suppressWarnings(tseries::adf.test(Value)$p.value) #check again for stationarity
        
        if (field != "SLI"){
          if (adf2 > 0.05){
            Value <- NA #if still non-stationary, make NA for exclusion
            
          } else if (is.na(adf)){
          Value <- NA
          } 
        }
      }
        
      
    }
    
    interm <- data.frame(Var = field,
                         Value = as.numeric(Value),
                         Time = Time,
                         EPU = i)
    
    assign('out', rbind(out, interm))
  }
  return(out)
}


soe <- lapply(var_ids, get_dat) #apply normalization and differencing function
soe <- soe %>%
  do.call(rbind,.) %>% #list to df
  rbind(., nao) %>% #Bind in NAO again
  rbind(.,CF_norm) %>%
  rbind(.,mean_cf) %>% 
  #rbind(., sli) %>% 
  filter(!is.na(Value)) %>% 
  filter(!str_detect(Var,"Other|other|OTHER")) %>% 
  filter(EPU != "OTHER")

save(soe, file = file.path(clean.dir,"soe_clean_2019.Rdata"))
