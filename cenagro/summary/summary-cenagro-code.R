##################################
# IV CENSO NACIONAL AGROPECUARIO 2012 - EJEMPLOS DE PREGUNTAS
# INPUT : CENAGRO database
# OUTPUT : Informacion socioeconomica de productores de papa
# AUTOR: Marvin Quispe Sedano

###############################################################
# Nota: Tildes omitidas en todos los textos
###############################################################

# Librerias 
pkgs = c("data.table", "cluster", "dplyr", "ggplot2", 
         "Rtsne", "raster", "ggsn", "partykit", "factoextra",
         "Hmisc", "corrgram", "openxlsx")

# install.packages(pkgs)
lapply(pkgs, library, character.only = TRUE)

###############################################################

# Configurar el directorio de trabajo
setwd("C:/Users/Asus/Documents/R/lateblight_tests/cenagro")
wd_data <- list.files("C:/Users/Asus/Documents/R/lateblight_tests/cenagro/combined/", 
                      pattern=".rds", full=TRUE)

data_list <- lapply(wd_data, readRDS)

###############################################################

# 1.- UNIDADES AGROPECUARIAS QUE SIEMBRAN NATIVA Y MEJORADA

# Cargamos los codigos de cultivo y eliminamos vacios

df_codigos <- setDT(data_list[[3]])
df_codigos <- df_codigos[!(is.na(df_codigos$P024_03) | df_codigos$P024_03==""), ]

# Unidades agropecuarias que siembran nativa y mejorada
# Codigos : 2612: blanca; 2610: amarga; 2611: amarilla; 2613: color; 2614: huayro; 2615: nativa

df_nym_total <- df_codigos[ , if(any(P024_03 == 2612) & (any(P024_03 == 2610) | any(P024_03 == 2611) | any(P024_03 == 2613) | any(P024_03 == 2614) | any(P024_03 == 2615))) .SD,
                      by = list(P001, P002, P003, P007X, P008, NPRIN)]

# Número de parcelas por unidad agropecuaria

df_parcelas_nym_total <- df_nym_total[ , max(NPARCX) , 
                                       by = list(P001, P002, P003, P007X, P008, NPRIN)]             

# Seleccionar solo las parcelas que contengan alguna variedad de papa

df_nym_papa <- df_nym_total[P024_03 %in% c(2610, 2611, 2612, 2613, 2614, 2615) ]

# Número de parcelas por unidad agropecuaria que solo tienen papa

df_parcelas_nym_papa <- df_nym_papa[ , length(unique(NPARCX)) , 
                                     by = list(P001, P002, P003, P007X, P008, NPRIN)]             


# Numero de unidades agropecuarias que siembran nativa y mejorada

nrow(df_parcelas_nym_total)
nrow(df_parcelas_nym_papa)

# Superficie sembrada del cultivo en hectareas

sum(df_nym_papa$P025)

# Superficie que se encuentra bajo riego y secano

df_nym_papa[ P026 == "Riego", sum(P025)]
df_nym_papa[ P026 == "Secano", sum(P025)]

# Superficie segun destino de la produccion

df_nym_papa[ P028 == "Autoconsumo", sum(P025)]
df_nym_papa[ P028 == "Autoinsumo", sum(P025)]
df_nym_papa[ P028 == "Alimento para sus animales", sum(P025)]

df_nym_papa[ P028 == "Venta", sum(P025)]


# Superficie segun destino de venta

df_nym_papa[ P029_01 == "Mercado nacional", sum(P025)]
df_nym_papa[ P029_02 == "Mercado exterior", sum(P025)]
df_nym_papa[ P029_03 == "Agroindustria", sum(P025)]



# Uniendo otras bases de datos de interes

df_nym_papa <- merge(df_nym_papa, 
                     data_list[[4]], 
                     by.x = c("P001","P002","P003","P007X","P008", "NPRIN", "NPARCX"),
                     by.y = c("P001","P002","P003","P007X","P008", "NPRIN", "NPARCY"),
                     all.x = TRUE)


# UA segun regimen de pertenencia de las parcelas

nrow(df_nym_papa[P037_01_03 == "Con título inscrito en registros públicos", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_nym_papa[P037_01_03 == "Con título no inscrito en registros públicos", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_nym_papa[P037_01_03 == "Sin título, pero en trámite de título", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_nym_papa[P037_01_03 == "Sin título, ni trámite", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# UA segun pertenencia a una comunidad campesina o nativa

nrow(df_nym_papa[P039_01 == "Comunidad campesina", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_nym_papa[P039_01 == "Comunidad nativa", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# Superficie sembrada del cultivo segun pertenencia a una comunidad 

df_nym_papa[ P039_01 == "Comunidad campesina", sum(P025)]

df_nym_papa[ P039_01 == "Comunidad nativa", sum(P025)]



# Uniendo otras bases de datos de interes

df_nym_papa <- merge(df_nym_papa, 
                     data_list[[7]], 
                     by.x = c("P001","P002","P003","P007X","P008", "NPRIN"),
                     by.y = c("P001","P002","P003","P007X","P008", "NPRIN"),
                     all.x = TRUE)

# UA que usan semilla y/o plantones certificados

nrow(df_nym_papa[P051 == "Si", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# UA que aplican guano, estiercol u abono organico

nrow(df_nym_papa[P052 == "En cantidad suficiente" | P052 == "En poca cantidad", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# UA que aplican fertilizantes quimicos

nrow(df_nym_papa[P053 == "En cantidad suficiente" | P053 == "En poca cantidad", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# UA que aplican insecticidas quimicos

nrow(df_nym_papa[P054_01 == "Si", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

# UA que aplican insecticidas no quimicos o biologico

nrow(df_nym_papa[P054_02 == "Si", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

# UA que aplican herbicida

nrow(df_nym_papa[P054_03 == "Si", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# UA que aplican fungicida

nrow(df_nym_papa[P054_04 == "Si", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

# UA que aplican control biologico

nrow(df_nym_papa[P056 == "Si", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# Uniendo otras bases de datos de interes

data_list[[10]] <- data_list[[10]][data_list[[10]]$P110 == "Productor(a)",]

df_nym_papa <- merge(df_nym_papa, 
                     data_list[[10]], 
                     by.x = c("P001","P002","P003","P007X","P008", "NPRIN"),
                     by.y = c("P001","P002","P003","P007X","P008", "NPRIN"),
                     all.x = TRUE)


# UA segun sexo del productor

nrow(df_nym_papa[P111 == "Hombre", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_nym_papa[P111 == "Mujer", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# Edad promedio del productor

df_nym_papa[ , mean(P112, na.rm = T)]


# UA segun nivel de educacion del productor

nrow(df_nym_papa[P114 == "Sin nivel", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_nym_papa[P114 == "Inicial", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_nym_papa[P114 == "Primaria incompleta", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_nym_papa[P114 == "Primaria completa", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_nym_papa[P114 == "Secundaria incompleta", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_nym_papa[P114 == "Secundaria completa", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_nym_papa[P114 == "Superior no univ. incompleta", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_nym_papa[P114 == "Superior no univ. completa", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_nym_papa[P114 == "Superior univ. incompleta", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_nym_papa[P114 == "Superior univ. completa", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# UA segun lengua de origen del productor

nrow(df_nym_papa[P115 == "Quechua", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_nym_papa[P115 == "Aymara", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_nym_papa[P115 == "Ashaninka", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_nym_papa[P115 == "Otra lengua nativa", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_nym_papa[P115 == "Castellano", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_nym_papa[P115 == "Idioma extranjero", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 



# Uniendo otras bases de datos de interes

df_nym_papa <- df_nym_papa[,-c("TIPO_REC.x", "TIPO_REC.y")]

df_nym_papa <- merge(df_nym_papa, 
                     data_list[[11]], 
                     by.x = c("P001","P002","P003","P007X","P008", "NPRIN"),
                     by.y = c("P001","P002","P003","P007X","P008", "NPRIN"),
                     all.x = TRUE)


# UA segun el tipo de saneamiento y alcantarillado

nrow(df_nym_papa[P118 == "Red pública de desagüe dentro de la vivienda", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_nym_papa[P118 == "Red pública de desagüe, fuera de la vivienda pero dentro de la edificación", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_nym_papa[P118 == "Pozo séptico", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_nym_papa[P118 == "Pozo ciego o negro / letrina", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_nym_papa[P118 == "Río, acequia o canal", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_nym_papa[P118 == "Otro", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_nym_papa[P118 == "No tiene", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# UA que cuentan con cocina mejorada

nrow(df_nym_papa[P119 == "Si", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

# UA que cuentan con computadora

nrow(df_nym_papa[P120_01 == "Si", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

# UA que cuentan con internet

nrow(df_nym_papa[P120_02 == "Si", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# UA segun lejania o cercania a la capital distrital

nrow(df_nym_papa[P121_01 == "Vive en la capital distrital", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


nrow(df_nym_papa[P121_01 == "Más de 24 horas", 
                 length(unique(NPARCX)) , 
                 by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# Guardar base de datos final agregando la base de datos de referencia espacial

df_nym_papa <- merge(df_nym_papa, 
                     data_list[[1]], 
                     by.x = c("P001","P002","P003","P007X","P008", "NPRIN"),
                     by.y = c("P001","P002","P003","P007X","P008", "NPRIN"),
                     all.x = TRUE)

write.csv(df_nym_papa, "data-nym-cenagro.csv", row.names = F)


###############################################################

# 2.- UNIDADES AGROPECUARIAS QUE SIEMBRAN SOLO MEJORADA

df_nym_total_2 <- df_codigos[ , if(!(any(P024_03 == 2612) & (any(P024_03 == 2610) | any(P024_03 == 2611) | any(P024_03 == 2613) | any(P024_03 == 2614) | any(P024_03 == 2615)))) .SD,
                            by = list(P001, P002, P003, P007X, P008, NPRIN)]


# Seleccionar solo las parcelas que contengan papa mejorada

df_m_papa <- df_nym_total_2[P024_03 %in% c(2612) ]

# Número de parcelas por unidad agropecuaria que solo tienen papa mejorada

df_parcelas_m_papa <- df_m_papa[ , max(NPARCX) , 
                                 by = list(P001, P002, P003, P007X, P008, NPRIN)]             

# Numero de unidades agropecuarias que siembran mejorada

nrow(df_parcelas_m_papa)

# Superficie sembrada del cultivo en hectareas

sum(df_m_papa$P025)

# Superficie que se encuentra bajo riego y secano

df_m_papa[ P026 == "Riego", sum(P025)]
df_m_papa[ P026 == "Secano", sum(P025)]

# Superficie segun destino de la produccion

df_m_papa[ P028 == "Autoconsumo", sum(P025)]
df_m_papa[ P028 == "Autoinsumo", sum(P025)]
df_m_papa[ P028 == "Alimento para sus animales", sum(P025)]

df_m_papa[ P028 == "Venta", sum(P025)]


# Superficie segun destino de venta

df_m_papa[ P029_01 == "Mercado nacional", sum(P025)]
df_m_papa[ P029_02 == "Mercado exterior", sum(P025)]
df_m_papa[ P029_03 == "Agroindustria", sum(P025)]



# Uniendo otras bases de datos de interes

df_m_papa <- merge(df_m_papa, 
                   data_list[[4]], 
                   by.x = c("P001","P002","P003","P007X","P008", "NPRIN", "NPARCX"),
                   by.y = c("P001","P002","P003","P007X","P008", "NPRIN", "NPARCY"),
                   all.x = TRUE)


# UA segun regimen de pertenencia de las parcelas

nrow(df_m_papa[P037_01_03 == "Con título inscrito en registros públicos", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_m_papa[P037_01_03 == "Con título no inscrito en registros públicos", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_m_papa[P037_01_03 == "Sin título, pero en trámite de título", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_m_papa[P037_01_03 == "Sin título, ni trámite", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# UA segun pertenencia a una comunidad campesina o nativa

nrow(df_m_papa[P039_01 == "Comunidad campesina", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_m_papa[P039_01 == "Comunidad nativa", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# Superficie sembrada del cultivo segun pertenencia a una comunidad 

df_m_papa[ P039_01 == "Comunidad campesina", sum(P025)]

df_m_papa[ P039_01 == "Comunidad nativa", sum(P025)]



# Uniendo otras bases de datos de interes

df_m_papa <- merge(df_m_papa, 
                   data_list[[7]], 
                   by.x = c("P001","P002","P003","P007X","P008", "NPRIN"),
                   by.y = c("P001","P002","P003","P007X","P008", "NPRIN"),
                   all.x = TRUE)

# UA que usan semilla y/o plantones certificados

nrow(df_m_papa[P051 == "Si", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# UA que aplican guano, estiercol u abono organico

nrow(df_m_papa[P052 == "En cantidad suficiente" | P052 == "En poca cantidad", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# UA que aplican fertilizantes quimicos

nrow(df_m_papa[P053 == "En cantidad suficiente" | P053 == "En poca cantidad", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# UA que aplican insecticidas quimicos

nrow(df_m_papa[P054_01 == "Si", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

# UA que aplican insecticidas no quimicos o biologico

nrow(df_m_papa[P054_02 == "Si", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

# UA que aplican herbicida

nrow(df_m_papa[P054_03 == "Si", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# UA que aplican fungicida

nrow(df_m_papa[P054_04 == "Si", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

# UA que aplican control biologico

nrow(df_m_papa[P056 == "Si", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# Uniendo otras bases de datos de interes

data_list[[10]] <- data_list[[10]][data_list[[10]]$P110 == "Productor(a)",]

df_m_papa <- merge(df_m_papa, 
                   data_list[[10]], 
                   by.x = c("P001","P002","P003","P007X","P008", "NPRIN"),
                   by.y = c("P001","P002","P003","P007X","P008", "NPRIN"),
                   all.x = TRUE)


# UA segun sexo del productor

nrow(df_m_papa[P111 == "Hombre", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_m_papa[P111 == "Mujer", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# Edad promedio del productor

df_m_papa[ , mean(P112, na.rm = T)]


# UA segun nivel de educacion del productor

nrow(df_m_papa[P114 == "Sin nivel", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_m_papa[P114 == "Inicial", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_m_papa[P114 == "Primaria incompleta", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_m_papa[P114 == "Primaria completa", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_m_papa[P114 == "Secundaria incompleta", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_m_papa[P114 == "Secundaria completa", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_m_papa[P114 == "Superior no univ. incompleta", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_m_papa[P114 == "Superior no univ. completa", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_m_papa[P114 == "Superior univ. incompleta", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_m_papa[P114 == "Superior univ. completa", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# UA segun lengua de origen del productor

nrow(df_m_papa[P115 == "Quechua", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_m_papa[P115 == "Aymara", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_m_papa[P115 == "Ashaninka", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_m_papa[P115 == "Otra lengua nativa", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_m_papa[P115 == "Castellano", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_m_papa[P115 == "Idioma extranjero", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 



# Uniendo otras bases de datos de interes

df_m_papa <- df_m_papa[,-c("TIPO_REC.x", "TIPO_REC.y")]

df_m_papa <- merge(df_m_papa, 
                   data_list[[11]], 
                   by.x = c("P001","P002","P003","P007X","P008", "NPRIN"),
                   by.y = c("P001","P002","P003","P007X","P008", "NPRIN"),
                   all.x = TRUE)


# UA segun el tipo de saneamiento y alcantarillado

nrow(df_m_papa[P118 == "Red pública de desagüe dentro de la vivienda", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_m_papa[P118 == "Red pública de desagüe, fuera de la vivienda pero dentro de la edificación", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_m_papa[P118 == "Pozo séptico", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_m_papa[P118 == "Pozo ciego o negro / letrina", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_m_papa[P118 == "Río, acequia o canal", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_m_papa[P118 == "Otro", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_m_papa[P118 == "No tiene", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# UA que cuentan con cocina mejorada

nrow(df_m_papa[P119 == "Si", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

# UA que cuentan con computadora

nrow(df_m_papa[P120_01 == "Si", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

# UA que cuentan con internet

nrow(df_m_papa[P120_02 == "Si", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# UA segun lejania o cercania a la capital distrital

nrow(df_m_papa[P121_01 == "Vive en la capital distrital", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


nrow(df_m_papa[P121_01 == "Más de 24 horas", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# Guardar base de datos final agregando la base de datos de referencia espacial

df_m_papa <- merge(df_m_papa, 
                   data_list[[1]], 
                   by.x = c("P001","P002","P003","P007X","P008", "NPRIN"),
                   by.y = c("P001","P002","P003","P007X","P008", "NPRIN"),
                   all.x = TRUE)


write.csv(df_m_papa, "data-m-cenagro.csv", row.names = F)

###############################################################


# 3.- UNIDADES AGROPECUARIAS QUE SIEMBRAN SOLO NATIVA


# Seleccionar solo las parcelas que contengan papa nativa

df_n_papa <- df_nym_total_2[P024_03 %in% c(2610, 2611, 2613, 2614, 2615) ]

# Número de parcelas por unidad agropecuaria que solo tienen papa nativa

df_parcelas_n_papa <- df_n_papa[ , max(NPARCX) , 
                                 by = list(P001, P002, P003, P007X, P008, NPRIN)]             

# Numero de unidades agropecuarias que siembran mejorada

nrow(df_parcelas_n_papa)


# Superficie sembrada del cultivo en hectareas

sum(df_n_papa$P025)

# Superficie que se encuentra bajo riego y secano

df_n_papa[ P026 == "Riego", sum(P025)]
df_n_papa[ P026 == "Secano", sum(P025)]

# Superficie segun destino de la produccion

df_n_papa[ P028 == "Autoconsumo", sum(P025)]
df_n_papa[ P028 == "Autoinsumo", sum(P025)]
df_n_papa[ P028 == "Alimento para sus animales", sum(P025)]

df_n_papa[ P028 == "Venta", sum(P025)]


# Superficie segun destino de venta

df_n_papa[ P029_01 == "Mercado nacional", sum(P025)]
df_n_papa[ P029_02 == "Mercado exterior", sum(P025)]
df_n_papa[ P029_03 == "Agroindustria", sum(P025)]



# Uniendo otras bases de datos de interes

df_n_papa <- merge(df_n_papa, 
                   data_list[[4]], 
                   by.x = c("P001","P002","P003","P007X","P008", "NPRIN", "NPARCX"),
                   by.y = c("P001","P002","P003","P007X","P008", "NPRIN", "NPARCY"),
                   all.x = TRUE)


# UA segun regimen de pertenencia de las parcelas

nrow(df_n_papa[P037_01_03 == "Con título inscrito en registros públicos", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_n_papa[P037_01_03 == "Con título no inscrito en registros públicos", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_n_papa[P037_01_03 == "Sin título, pero en trámite de título", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_n_papa[P037_01_03 == "Sin título, ni trámite", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# UA segun pertenencia a una comunidad campesina o nativa

nrow(df_n_papa[P039_01 == "Comunidad campesina", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_n_papa[P039_01 == "Comunidad nativa", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# Superficie sembrada del cultivo segun pertenencia a una comunidad 

df_n_papa[ P039_01 == "Comunidad campesina", sum(P025)]

df_n_papa[ P039_01 == "Comunidad nativa", sum(P025)]



# Uniendo otras bases de datos de interes

df_n_papa <- merge(df_n_papa, 
                   data_list[[7]], 
                   by.x = c("P001","P002","P003","P007X","P008", "NPRIN"),
                   by.y = c("P001","P002","P003","P007X","P008", "NPRIN"),
                   all.x = TRUE)

# UA que usan semilla y/o plantones certificados

nrow(df_n_papa[P051 == "Si", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# UA que aplican guano, estiercol u abono organico

nrow(df_n_papa[P052 == "En cantidad suficiente" | P052 == "En poca cantidad", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# UA que aplican fertilizantes quimicos

nrow(df_n_papa[P053 == "En cantidad suficiente" | P053 == "En poca cantidad", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# UA que aplican insecticidas quimicos

nrow(df_n_papa[P054_01 == "Si", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

# UA que aplican insecticidas no quimicos o biologico

nrow(df_n_papa[P054_02 == "Si", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

# UA que aplican herbicida

nrow(df_n_papa[P054_03 == "Si", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# UA que aplican fungicida

nrow(df_n_papa[P054_04 == "Si", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

# UA que aplican control biologico

nrow(df_n_papa[P056 == "Si", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# Uniendo otras bases de datos de interes

data_list[[10]] <- data_list[[10]][data_list[[10]]$P110 == "Productor(a)",]

df_n_papa <- merge(df_n_papa, 
                   data_list[[10]], 
                   by.x = c("P001","P002","P003","P007X","P008", "NPRIN"),
                   by.y = c("P001","P002","P003","P007X","P008", "NPRIN"),
                   all.x = TRUE)


# UA segun sexo del productor

nrow(df_n_papa[P111 == "Hombre", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_n_papa[P111 == "Mujer", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# Edad promedio del productor

df_n_papa[ , mean(P112, na.rm = T)]


# UA segun nivel de educacion del productor

nrow(df_n_papa[P114 == "Sin nivel", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_n_papa[P114 == "Inicial", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_n_papa[P114 == "Primaria incompleta", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_n_papa[P114 == "Primaria completa", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_n_papa[P114 == "Secundaria incompleta", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_n_papa[P114 == "Secundaria completa", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_n_papa[P114 == "Superior no univ. incompleta", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_n_papa[P114 == "Superior no univ. completa", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_n_papa[P114 == "Superior univ. incompleta", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_n_papa[P114 == "Superior univ. completa", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# UA segun lengua de origen del productor

nrow(df_n_papa[P115 == "Quechua", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_n_papa[P115 == "Aymara", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_n_papa[P115 == "Ashaninka", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_n_papa[P115 == "Otra lengua nativa", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_n_papa[P115 == "Castellano", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_n_papa[P115 == "Idioma extranjero", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 



# Uniendo otras bases de datos de interes

df_n_papa <- df_n_papa[,-c("TIPO_REC.x", "TIPO_REC.y")]

df_n_papa <- merge(df_n_papa, 
                   data_list[[11]], 
                   by.x = c("P001","P002","P003","P007X","P008", "NPRIN"),
                   by.y = c("P001","P002","P003","P007X","P008", "NPRIN"),
                   all.x = TRUE)


# UA segun el tipo de saneamiento y alcantarillado

nrow(df_n_papa[P118 == "Red pública de desagüe dentro de la vivienda", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_n_papa[P118 == "Red pública de desagüe, fuera de la vivienda pero dentro de la edificación", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_n_papa[P118 == "Pozo séptico", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_n_papa[P118 == "Pozo ciego o negro / letrina", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_n_papa[P118 == "Río, acequia o canal", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_n_papa[P118 == "Otro", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

nrow(df_n_papa[P118 == "No tiene", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# UA que cuentan con cocina mejorada

nrow(df_n_papa[P119 == "Si", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

# UA que cuentan con computadora

nrow(df_n_papa[P120_01 == "Si", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 

# UA que cuentan con internet

nrow(df_n_papa[P120_02 == "Si", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# UA segun lejania o cercania a la capital distrital

nrow(df_n_papa[P121_01 == "Vive en la capital distrital", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


nrow(df_n_papa[P121_01 == "Más de 24 horas", 
               length(unique(NPARCX)) , 
               by = list(P001, P002, P003, P007X, P008, NPRIN)]) 


# Guardar base de datos final agregando la base de datos de referencia espacial

df_n_papa <- merge(df_n_papa, 
                   data_list[[1]], 
                   by.x = c("P001","P002","P003","P007X","P008", "NPRIN"),
                   by.y = c("P001","P002","P003","P007X","P008", "NPRIN"),
                   all.x = TRUE)

write.csv(df_n_papa, "data-n-cenagro.csv", row.names = F)


