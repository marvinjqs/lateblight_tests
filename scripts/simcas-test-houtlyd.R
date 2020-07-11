pkgs = c("maptools", "data.table", "tidyr", "dplyr")
# install.packages(pkgs)
lapply(pkgs, library, character.only = TRUE)



calculate_hhr <- function(climdata, lon, lat) {
  
  #########################
  
  date_df <- as.Date(climdata[[1]], format = "%Y-%m-%d")
  tn <- as.numeric(climdata[[2]])
  tx <- as.numeric(climdata[[3]])
  tavg <- (tn+tx)/2
  hrn <- as.numeric(climdata[[5]])
  hrx <- as.numeric(climdata[[6]])
  hravg <- (hrn+hrx)/2
  tdew <- tavg - ( (100 - hravg) / 5)
  rain <- as.numeric(climdata[[4]])
  
  
  
  # Calculate hourly for sunrise and sunset
  
  crds <- matrix(c(lon[[1]], lat[[1]]), ncol = 2)
  
  sunrise <- sunriset(crds, as.POSIXct(date_df),
                      proj4string=CRS("+proj=longlat +datum=WGS84"),
                      direction=c("sunrise"), POSIXct.out= F)
  
  sunset <- sunriset(crds, as.POSIXct(date_df),
                     proj4string=CRS("+proj=longlat +datum=WGS84"),
                     direction=c("sunset"), POSIXct.out= F)
  
  
  df_format_hhr <- data.frame(date_df[-c(length(date_df),1)], tn[-c(length(tn),1)], tn[-c(1:2)], 
                              tx[-c(length(tx),1)], tx[-c(length(tx), length(tx) - 1)], 
                              tdew[-c(length(tdew),1)],
                              sunrise[-c(length(sunrise),1)], sunset[-c(length(sunset),1)], 
                              sunset[-c(length(sunset),length(sunset) - 1)])
  
  names(df_format_hhr) <- c("date", "tmin", "tminnext", 
                            "tmax", "tmaxold", "tdew",
                            "sunrise", "sunset", "sunset old")
  
  print(sunset)
  #########################
  
  # Tn temp min, Tx temp max, To sunset , Tp temp min next
  # Hn sunrise, Hx h temp max, Ho h sunset , Hp h temp min next
  # date <- df_in_hhr[[1]]
  
  #####
  
  model_temp_hr <- function(df_in_hhr){
    
    Tn <- as.numeric(df_in_hhr[[2]])
    Tp <- as.numeric(df_in_hhr[[3]])
    Tx <- as.numeric(df_in_hhr[[4]])
    Hn <- as.numeric(df_in_hhr[[7]]) * 24
    Ho <- as.numeric(df_in_hhr[[8]]) * 24
    tdew <- as.numeric(df_in_hhr[[6]])
    
    #####
    
    Tp_old <- Tn
    Hp_old <- Hn + 24
    Tx_old <- as.numeric(df_in_hhr[[5]])
    Ho_old <- as.numeric(df_in_hhr[[9]]) * 24
    To_old <- Tx_old - 0.39*(Tx_old - Tp_old)
    
    # Parameters for model 
    To <- Tx - 0.39*(Tx - Tp)
    Hp <- Hn + 24
    Hx <- Ho - 4
    
    alpha <- Tx - Tn
    r <- Tx - To
    beta1 <- (Tp - To) / sqrt(Hp - Ho)
    beta2 <- (Tp_old - To_old) / sqrt(Hp_old - Ho_old)
    
    t <- 1:24
    T_model <- 0
    
    for(i in 1:24) {
      
      if(t[i] > Hn & t[i] <= Hx){
        
        T_model[i] <- Tn + alpha * ( ((t[i]-Hn) / (Hx-Hn)) * (pi/2) )
        
      }
      
      else if(t[i] > Hx & t[i] <= Ho){
        
        T_model[i] <- To + r * sin( (pi/2) + ( ((t[i]-Hx)/4) * (pi/2) ) )
        
      }
      
      else if( t[i] > Ho & t[i] <= 24 ){
        
        T_model[i] <- To + beta1 * sqrt( (t[i] - Ho) )
        
      }
      
      else if (t[i] >= 1 & t[i] <= Hn){
        
        T_model[i] <- To_old + beta2 * sqrt( (t[i]+24) - Ho_old) 
        
      }
      
      
      else {
        T_model[i] <- "Error"
        
      }
      
    }
    
    
    # Buck formula for es and e (kPa)
    
    es <- 0.61121 * exp( ( (18.678 - (T_model/234.5)) * T_model) / (257.14 + T_model))
    e <- 0.61121 * exp( ( (18.678 - (tdew/234.5)) * tdew) / (257.14 + tdew))
    
    # hr limit > 90 
    hr <- (e/es) * 100
    
    
    return(hr)
  }
  
  df_hrlim_hours <- apply(df_format_hhr, 1 , function(x) model_temp_hr(x))
  
  
  
  return(df_hrlim_hours)
  
}


setwd("C:/Users/Asus/Desktop/")
df <- read.csv("BD-HUANUCO-Original.csv", header = T, stringsAsFactors = F, sep = ",")

df$date <- seq(
  from = as.POSIXct("2001-02-10 1", tz = "UTC"),
  by = "hour",
  length.out = nrow(df)
)  

df$date_d <- format(df$date, format = "%Y-%m-%d")
prom_d_tn <- aggregate(Temp_C ~ date_d , df, min)
prom_d_tx <- aggregate(Temp_C ~ date_d , df, max)
prom_d_pp <- aggregate(Rain.mm. ~ date_d , df, sum)
prom_d_hrn <- aggregate(RH ~ date_d , df, min)
prom_d_hrx <- aggregate(RH ~ date_d , df, max)


df_diario <- data.frame(prom_d_tn, prom_d_tx[,2], prom_d_pp[,2], prom_d_hrn[,2], prom_d_hrx[,2])
names(df_diario) <- c("date", "temp_n", "temp_x", "pp", "hr_n", "hr_x")


input_runsimcast <- calculate_hhr(df_diario, -76.0883, -9.74444)


library(reshape)
library(openxlsx)
df_final <- melt(input_runsimcast, var='hr_hourly')

write.xlsx(df_final, "SIMCAST-p1.xlsx", row.names = F)











