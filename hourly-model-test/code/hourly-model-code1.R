##################################
# MODELADO DE DATOS HORARIOS DE TEMP Y HR 
# INPUT : Temp diaria, HR diaria
# OUTPUT : Temp horaria, HR horaria
# @MarvinQuispeSedano

##################################

pkgs = c("maptools", "data.table", "tidyr", "dplyr", "plyr")
# install.packages(pkgs)
lapply(pkgs, library, character.only = TRUE)

# Funcion para calcular datos horarios
calculate_hhr <- function(climdata, lon, lat) {
  
  #########################
  
  date_df <- as.Date(climdata[[1]], format = "%Y-%m-%d")
  tn <- as.numeric(climdata[[2]])
  tx <- as.numeric(climdata[[3]])
  tavg <- (tn+tx)/2
  #hrn <- as.numeric(climdata[[5]])
  #hrx <- as.numeric(climdata[[6]])
  #hravg <- (hrn+hrx)/2
  hravg <- as.numeric(climdata[[5]])
  tdew <- tavg - ( (100 - hravg) / 5)
  #tdew <- as.numeric(climdata[[5]])
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
    hr[hr>100] <- 100
    
    df_temp_hr <- data.frame(hr, T_model)
    names(df_temp_hr) <- c("hr","temp")
    
    return(df_temp_hr)
  }
  
  df_hrlim_hours <- apply(df_format_hhr, 1 , function(x) model_temp_hr(x))
  
  
  
  return(df_hrlim_hours)
  
}

# Lectura de base de datos diarios
setwd("C:/Users/Asus/Desktop/CIP-SIMCAST/")
df <- read.csv("sanramon-data.csv", header = T, stringsAsFactors = F, 
               sep = ";", na.strings = "---")

date_paste <- paste(df[,1], df[,2])

df$date_or <- as.POSIXct(date_paste, format = "%d/%m/%Y %H:%M")
df$date_d <- format(df$date_or, format = "%Y-%m-%d")
df$date_h <- format(df$date_or, format = "%Y-%m-%d %H")

prom_d_tn <- aggregate(Temp.min ~ date_d , df, min)
prom_d_tx <- aggregate(Temp.max ~ date_d , df, max)
prom_d_pp <- aggregate(Rain ~ date_d , df, sum)
prom_d_hr <- aggregate(Hum ~ date_d , df, mean)

prom_h_tn <- aggregate(Temp.min ~ date_h , df, min)
prom_h_tx <- aggregate(Temp.max ~ date_h , df, max)
prom_h_hr <- aggregate(Hum ~ date_h , df, mean)


# Combinar los dataframes de valores diarios y eliminar valores NA

df_diario <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                    list(prom_d_tn, prom_d_tx, prom_d_pp, prom_d_hr))

df_diario_final <- df_diario[complete.cases(df_diario), ]

names(df_diario_final) <- c("date", "temp_n", "temp_x", "pp", "hr")

# Combinar los dataframes de valores horarios

df_seq_h <- seq(as.POSIXct("2018-12-06 00", format = "%Y-%m-%d %H"),
                as.POSIXct("2019-11-26 00", format = "%Y-%m-%d %H"),
                by = "hour")

df_seq_h <- data.frame(format(df_seq_h, format = "%Y-%m-%d %H"))

names(df_seq_h) <- "date_h"

df_horario <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                     list(prom_h_tn, prom_h_tx, prom_h_hr, df_seq_h))  
  
df_horario$date_h <- as.POSIXct(df_horario$date_h, format = "%Y-%m-%d %H")

df_horario$date_d <- format(df_horario$date_h, format = "%Y-%m-%d")

df_horario_final <- df_horario[df_horario$date_d %in% df_diario_final$date,]

# Ingresar las coordenadas de la estacion

input_runsimcast <- calculate_hhr(df_diario_final, -75.3601, -11.127505)


library(plyr)
library(openxlsx)

df_hourly_t_hr <- ldply(input_runsimcast , data.frame)

#########################################

# Exportar la información de interés

df_export1 <- data.frame(df_horario_final[25:(nrow(df_horario_final)-24),], 
                              df_hourly_t_hr)

df_export1$temp_avg <- (df_export1$Temp.min + df_export1$Temp.max)/2

df_export_final <- data.frame(df_export1[,c(1,8,4,7,6)])

names(df_export_final) <- c("date", "temp_station", "hr_station", 
                            "temp_model", "hr_model")


setwd("C:/Users/Asus/Desktop/CIP-SIMCAST/")
write.xlsx(df_export_final, "sanramon-obs-vs-model.xlsx", row.names = F)










