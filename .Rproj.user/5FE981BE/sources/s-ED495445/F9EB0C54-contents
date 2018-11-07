#load_data


# Data directory
clean.dir <- here("data")

# Load all
invisible(lapply(file.path(clean.dir,as.list(list.files(clean.dir))),load,environment()))

# Programmatic bind
soe <- NULL
for (i in names(as.list(.GlobalEnv))){
  if (any(class(get(i)) %in% "data.frame") & i != "soe"){
    assign("soe",rbind(soe, get(i)))
  }
}

#NAO data is already normalized - remove it
nao <- soe %>% filter(Var == "nao index") %>% select(-Units)
soe <- soe %>% filter(Var != "nao index")

# Get variable names
var_ids <- sort(as.factor(na.omit(unique(soe$Var))))
epu_ids <- sort(as.factor(na.omit(unique(soe$EPU))))

# Normalize and difference
get_dat <- function(field){
  
  group <- soe %>% dplyr::filter(Var == field)
  
  out <- NULL
  for (i in unique(group$EPU)){
    
    Value <- group %>% filter(EPU == i) %>% pull(Value)
    Time <- group %>% filter(EPU == i) %>% pull(Time)
    
    if (all(is.na(as.numeric(Value))) | sd(Value, na.rm = T) == 0){
      Value <- NA #Assign as NA if not including
    } else {
      
      Value <- Value[1:length(Time)]
      Value <- (Value-mean(Value, na.rm = TRUE))/sd(Value, na.rm = TRUE) #normalize
      
      #interpolate missing values
      if (any(is.na(Value))){
        Value <- approx(Time, Value, n = length(Value))$y
      }
      
      #test for stationarity with Augmented Dickey Fuller
      adf <- suppressWarnings(adf.test(Value)$p.value)
      
      if (adf > 0.05){ #if non-stationary take first difference and use residuals
        mod <- arima(Value, order = c(0,1,0))
        Value <- resid(mod)
        
        adf2 <- suppressWarnings(adf.test(Value)$p.value) #check again for stationarity
        if (adf2 > 0.05){
          Value <- NA #if still non-stationary, make NA for exclusion
          
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
  rbind(., nao) #Bind in NAO again

# Get normalized variable names


#App selection parameters
var_ids <- sort(unique(soe[!is.na(soe$Value),]$Var))
epu_ids <- c("all","GOM","GB","MAB")
