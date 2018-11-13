
#load_data
clean.dir <- here::here("data")
load(file.path(clean.dir,"soe_clean_2019.Rdata"))

#App selection parameters
var_ids <- sort(unique(soe[!is.na(soe$Value),]$Var))
epu_ids <- c("all","GOM","GB","MAB")

#Function for lagging series
lag_series <- function(df, k) {
  
  series <- df$Value
  
  if (k>0) { #If lag > 0, add NA to vector; trim end to keep length constant
    out <- c(rep(NA, k), series)[1 : length(series)] 
  }
  else { #If k < 0, c() NAs to end of vector to pad; start vec at -k+1 
    out <- c(series[(-k+1) : length(series)], rep(NA, -k))
  }
  
  df$Value <- out #Replace value column in og df
  return(df)
}
