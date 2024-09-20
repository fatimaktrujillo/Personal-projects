#===========================================================================#
# Proyecto: Vision
# Encargados: SMR, LL, FTQ, LC
# Objetivo: Sistematizacion de informacion de servicios que demandan fibra
# Encoding: UTF-8
#===========================================================================#

#+---------------------+
#       Entorno    : |
#+---------------------+

#Limpiamos el entorno, consola y gráficos

rm(list = ls())
graphics.off()
cat("\014")



#+---------------------+
#       Librerias    : |
#+---------------------+

#Instalar librer?as
# install.packages('stringr')
# install.packages('RSelenium')
# install.packages('xml2')
# install.packages('rvest')
# install.packages('htt')
# install.packages('openxlsx')

#cargamos las librer?as
library("stringr")
library("RSelenium")
library("xml2")
library("rvest")
library("httr")
library("readxl")
library("openxlsx")
library("sqldf")
library("writexl")
library("tidyverse")
library("sf")
library("dplyr")
library("tidyverse")

#+-----------------------------------+
#    Descripción de procedimientos: |
#+----------------------------------+


### 1. Importar BD del Osiptel de cada una de las empresas

### 2. Limpieza de las BD
    #2.1: Loop para incorporar el nombre de la empresa en cada base
    #2.2: Loop para appendear la BD
    #2.3: Renombrar columnas
    #2.4: Limpieza de datos (mayúsculas, tíldes, Typos, variables numéricas)
    #2.5: Mantener solo observaciones del último mes disponible (en este caso mes 3)
    #2.6: Ordenar la BD

### 3. Análisis de participación de mercado en fibra (Estimación de Market shares)
    #3.1: Filtrar solo aquellas que son de FTTH y FTTX; y columnas con información relevante
    #3.2: Valores agregados a nivel de empresa por provincia (numerador)
    #3.3: Incluir valor a nivel de provincia (denominador)
    #3.4: Estimar participaciones de mercado de cada empresa por provincia
    #3.5: Estimar la participación de las partes (en valor y porcentaje)
    #3.6: Identificar en cuáles MI, existe traslape horizontal

### 4. An?lisis de participaciones por provincia
    #4,1; Collapse a nivel de provincia
    #4.2: Se presentan las participaciones de mercado
    #4.3: Evaluación de umbral horizontal
    #4.4: Evaluación de umbral vertical (a nivel nacional)


#------------------------#
##### 1. Importar BD #####
#------------------------#


# Establecemos la ruta

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


##Definimos la ruta con todos los archivos


filenames <- list.files("../../2 Bases/1 Osiptel/13-06-2023 Osiptel", full.names = TRUE)


#Aplicar funcion de leer cada excel, segun los nombres de los archivos
list_data <- lapply(filenames, read_excel)

#Nombrar a los archivos como df (para hacer Loop)
names(list_data) <- paste('df', seq_along(filenames), sep = '')

#Crear los objectos en el  global environment.
list2env(list_data, .GlobalEnv)

#---------------------------------#
##### 2. Limpieza de las BD #######
#---------------------------------#

# Loop 1: Loops para añadir nombre de la empresa
for(i in 1:44) {
  
  #Convertir n?mero a texto
  j<-str_pad(i,1,pad="0")
  print(j)
  
  #Crear BD auxiliar con información
  loops <- get(paste('df',j, sep=""))
  
  #Convertir a dataframe
  loops <- as.data.frame(loops)
  
  #Crear enpresa
  loops$Empresa <- loops[2,1]

  #Dropear filas
  loops <- loops[-c(1:19), ]
  
  #Reasignar base auxiliar a la base oficial
  assign( paste('df',j, sep=""), loops)
  
}

# Loop 2: Loop para consolidar base
BD_consolidada <- df1

for(i in 2:44) {
  
  #Convertir n?mero a texto
  j<-str_pad(i,1,pad="0")
  print(j)
  
  #Crear BD auxiliar con información
  loops <- get(paste('df',j, sep=""))
  
  #Crear loop con base auxiliar
  BD_consolidada <- rbind(BD_consolidada, loops)
  
}

# Renombrar variables
BD_consolidada <- BD_consolidada %>% 
  dplyr::rename("MES" = 1, 
         "DEPARTAMENTOS" = 2, 
         "PROVINCIA" = 3,
         "DISTRITO" = 4,
         "CENTRO POBLADO" = 5,
         "CÓDIGO_CENTRO_POBLADO" = 6,
         "SEGMENTO" = 7, 
         "TECNOLOGÍA" = 8,
         "VELOCIDAD_CONTRATADA_BAJADA" = 9,
         "VELOCIDAD_CONTRATADA_SUBIDA" = 10,
         "VELOCIDAD_GARANTIZADA_BAJADA" = 11,
         "VELOCIDAD_GARANTIZADA_SUBIDA" = 12,
         "CONEXIONES" = 13,
         "EMPRESA" = 14)

# Convertir en variables numéricas
BD_consolidada$MES <- as.numeric(BD_consolidada$MES)
BD_consolidada$CONEXIONES <- as.numeric(BD_consolidada$CONEXIONES)

# Corregir mayúsculas
BD_consolidada$DEPARTAMENTOS <- toupper(as.character(BD_consolidada$DEPARTAMENTOS))
BD_consolidada$PROVINCIA <- toupper(as.character(BD_consolidada$PROVINCIA))
BD_consolidada$DISTRITO <- toupper(as.character(BD_consolidada$DISTRITO))
BD_consolidada$`CENTRO POBLADO` <- toupper(as.character(BD_consolidada$`CENTRO POBLADO`))
BD_consolidada$TECNOLOGÍA <- toupper(as.character(BD_consolidada$TECNOLOGÍA))
BD_consolidada$EMPRESA <- toupper(as.character(BD_consolidada$EMPRESA))

# Corregir tildes

  # Obtener los nombres de las columnas de BD_consolidada
columnas <- colnames(BD_consolidada)

  # Recorrer cada columna
for (col in columnas) {
  
  # Eliminar tildes de las letras
  BD_consolidada[[col]] <- chartr("ÁÉÍÓÚÑ", "AEIOUN", BD_consolidada[[col]])
  
}

# Corregir texto de tecnologías

    ## Fibra
BD_consolidada$TECNOLOGÍA <- ifelse(BD_consolidada$TECNOLOGÍA=='FFTX','FTTX',BD_consolidada$TECNOLOGÍA)
BD_consolidada$TECNOLOGÍA <- ifelse(BD_consolidada$TECNOLOGÍA=='OTROS ALAMBRICOS (FIBRA)','FTTX',BD_consolidada$TECNOLOGÍA)
BD_consolidada$TECNOLOGÍA <- ifelse(BD_consolidada$TECNOLOGÍA=='OTROS INALAMBRICOS (FTTB)','FTTX',BD_consolidada$TECNOLOGÍA)
BD_consolidada$TECNOLOGÍA <- ifelse(BD_consolidada$TECNOLOGÍA=='OTROS ALAMBRICOS (FTTB)','FTTX',BD_consolidada$TECNOLOGÍA)
BD_consolidada$TECNOLOGÍA <- ifelse(BD_consolidada$TECNOLOGÍA=='FTTB','FTTX',BD_consolidada$TECNOLOGÍA)
BD_consolidada$TECNOLOGÍA <- ifelse(BD_consolidada$TECNOLOGÍA=='OTRAS TECNOLOGIAS ALAMBRICAS (FTTH)','FTTH',BD_consolidada$TECNOLOGÍA)
BD_consolidada$TECNOLOGÍA <- ifelse(BD_consolidada$TECNOLOGÍA=='PTP','FTTH',BD_consolidada$TECNOLOGÍA) #Clasificaci?n de acuerdo con Osiptel


    ## HFC
BD_consolidada$TECNOLOGÍA <- ifelse(BD_consolidada$TECNOLOGÍA=='CABLE MODEM','HFC',BD_consolidada$TECNOLOGÍA)
BD_consolidada$TECNOLOGÍA <- ifelse(BD_consolidada$TECNOLOGÍA=='CABLEMODEM','HFC',BD_consolidada$TECNOLOGÍA) 
BD_consolidada$TECNOLOGÍA <- ifelse(BD_consolidada$TECNOLOGÍA=='CABLEMODEN','HFC',BD_consolidada$TECNOLOGÍA) 
BD_consolidada$TECNOLOGÍA <- ifelse(BD_consolidada$TECNOLOGÍA=='EOC','HFC',BD_consolidada$TECNOLOGÍA) #Clasificaci?n de acuerdo con Osiptel

    ## Microondas
BD_consolidada$TECNOLOGÍA <- ifelse(BD_consolidada$TECNOLOGÍA=='OTROS INALAMBRICOS (WIFI)','MICROONDAS',BD_consolidada$TECNOLOGÍA)
BD_consolidada$TECNOLOGÍA <- ifelse(BD_consolidada$TECNOLOGÍA=='OTROS INALAMBRICOS (MICROONDAS)','MICROONDAS',BD_consolidada$TECNOLOGÍA)
BD_consolidada$TECNOLOGÍA <- ifelse(BD_consolidada$TECNOLOGÍA=='OTROS INALÁMBRICOS (MICROONDAS)','MICROONDAS',BD_consolidada$TECNOLOGÍA)

    ## TECNOLOGÍAs G
BD_consolidada$TECNOLOGÍA <- ifelse(BD_consolidada$TECNOLOGÍA=='OTROS INALAMBRICOS (3G)','3G',BD_consolidada$TECNOLOGÍA)
BD_consolidada$TECNOLOGÍA <- ifelse(BD_consolidada$TECNOLOGÍA=='OTROS INALAMBRICOS (3GPP)','3G',BD_consolidada$TECNOLOGÍA)
BD_consolidada$TECNOLOGÍA <- ifelse(BD_consolidada$TECNOLOGÍA=='OTROS INALAMBRICOS (2G)','2G',BD_consolidada$TECNOLOGÍA)

    ## Otras

BD_consolidada$TECNOLOGÍA <- ifelse(BD_consolidada$TECNOLOGÍA=='OTRAS TECNOLOGÍAS ALAMBRICAS','OTRAS ALAMBRICAS',BD_consolidada$TECNOLOGÍA)
BD_consolidada$TECNOLOGÍA <- ifelse(BD_consolidada$TECNOLOGÍA=='OTRAS TECNOLOGIAS ALAMBRICAS','OTRAS ALAMBRICAS',BD_consolidada$TECNOLOGÍA)
BD_consolidada$TECNOLOGÍA <- ifelse(BD_consolidada$TECNOLOGÍA=='MPLS','OTRAS ALAMBRICAS',BD_consolidada$TECNOLOGÍA)
BD_consolidada$TECNOLOGÍA <- ifelse(BD_consolidada$TECNOLOGÍA=='OTROS INALAMBRICOS','OTRAS INALAMBRICAS',BD_consolidada$TECNOLOGÍA)


    ## Revisar clasificación
table(BD_consolidada$TECNOLOGÍA)

  
# Corregir nombres de Departamentos o Provincias

    ##Departamentos
BD_consolidada$DEPARTAMENTOS <- ifelse(BD_consolidada$DEPARTAMENTOS=='LIMA' & BD_consolidada$PROVINCIA=='CALLAO', 
                                       'CALLAO',BD_consolidada$DEPARTAMENTOS )
BD_consolidada$DEPARTAMENTOS <- ifelse(BD_consolidada$DEPARTAMENTOS=='LAMBAQYEQUE', 
                                       'LAMBAYEQUE',BD_consolidada$DEPARTAMENTOS )

table(BD_consolidada$DEPARTAMENTOS)

    ##Provincias
BD_consolidada$PROVINCIA<- ifelse(BD_consolidada$DEPARTAMENTOS=='CALLAO', 
                                  'CALLAO',BD_consolidada$PROVINCIA)
BD_consolidada$PROVINCIA<- ifelse(BD_consolidada$PROVINCIA=='NASCA', 
                                  'NAZCA',BD_consolidada$PROVINCIA)
BD_consolidada$PROVINCIA<- ifelse(BD_consolidada$PROVINCIA=='AYNA' & BD_consolidada$DISTRITO=='CAYNA', 
                                  'AMBO',BD_consolidada$PROVINCIA)

table(BD_consolidada$PROVINCIA)

    ##Distritos
BD_consolidada$DISTRITO<- ifelse(BD_consolidada$DEPARTAMENTOS=='LIMA'&
                                 BD_consolidada$PROVINCIA=='LIMA' & 
                                 BD_consolidada$DISTRITO=='CERCADO DE LIMA',
                                 'LIMA', BD_consolidada$DISTRITO)
BD_consolidada$DISTRITO<- ifelse(BD_consolidada$DEPARTAMENTOS=='AYACUCHO'&
                                   BD_consolidada$PROVINCIA=='VICTOR FAJARDO' & 
                                   BD_consolidada$DISTRITO=='HUAYA',
                                 'HUALLA', BD_consolidada$DISTRITO)
BD_consolidada$DISTRITO<- ifelse(BD_consolidada$DEPARTAMENTOS=='JUNIN'&
                                   BD_consolidada$PROVINCIA=='SATIPO' & 
                                   BD_consolidada$DISTRITO=='MAZAMARI-PANGOA',
                                 'MAZAMARI', BD_consolidada$DISTRITO)
BD_consolidada$DISTRITO<- ifelse(BD_consolidada$DEPARTAMENTOS=='ICA'&
                                   BD_consolidada$PROVINCIA=='NAZCA' & 
                                   BD_consolidada$DISTRITO=='NASCA',
                                 'NAZCA', BD_consolidada$DISTRITO)


#Limpiamos la velocidad de bajada
  
  # Quitar todo lo que no sea número
BD_consolidada$VELOCIDAD_GARANTIZADA_BAJADA <- gsub("[^0-9]+", "", BD_consolidada$VELOCIDAD_GARANTIZADA_BAJADA)

  # Convertir a valor numértico
BD_consolidada$VELOCIDAD_GARANTIZADA_BAJADA <- as.numeric(BD_consolidada$VELOCIDAD_GARANTIZADA_BAJADA)

  # Eliminar la notación decimal
options(scipen=999) #Eliminar la notación decimal

  # Convertir a Mbps
for(i in 1:nrow(BD_consolidada)) {
  while(BD_consolidada$VELOCIDAD_GARANTIZADA_BAJADA[[i]] > 1000 & is.na(BD_consolidada$VELOCIDAD_GARANTIZADA_BAJADA[[i]])==FALSE)
  {
    BD_consolidada$VELOCIDAD_GARANTIZADA_BAJADA[[i]] <- (BD_consolidada$VELOCIDAD_GARANTIZADA_BAJADA[[i]])/1000
  }
}

  # Redondear resultados
BD_consolidada$VELOCIDAD_GARANTIZADA_BAJADA <- round(BD_consolidada$VELOCIDAD_GARANTIZADA_BAJADA, digits = 0)

  # Presentar la tabla
table(BD_consolidada$VELOCIDAD_GARANTIZADA_BAJADA)
summary(BD_consolidada$VELOCIDAD_GARANTIZADA_BAJADA)

# Ordenar base
BD_consolidada <- BD_consolidada [,c("EMPRESA", "MES", "DEPARTAMENTOS", "PROVINCIA",
                      "DISTRITO", "CENTRO POBLADO", "CÓDIGO_CENTRO_POBLADO",
                      "SEGMENTO", "TECNOLOGÍA", "VELOCIDAD_CONTRATADA_BAJADA",  
                      "VELOCIDAD_CONTRATADA_SUBIDA", "VELOCIDAD_GARANTIZADA_BAJADA",  
                      "VELOCIDAD_GARANTIZADA_SUBIDA", "CONEXIONES")]

# Eliminar elementos del Proyecto
rm(list = ls()[grepl("df", ls())])


#-----------------------------------------#
### 3. Caracterización de la demanda ######
#-----------------------------------------#

#Nos quedamos con las variables de interés
BD_consolidada <- subset(BD_consolidada, select = -c(VELOCIDAD_CONTRATADA_BAJADA, VELOCIDAD_CONTRATADA_SUBIDA, VELOCIDAD_GARANTIZADA_SUBIDA))

#Eliminamos las tecnologías que no son de nuestro interés(Se mantienen las conexiones G y las de acceso alámbrico)
BD_consolidada <- subset(BD_consolidada, !(TECNOLOGÍA %in% c("MICROONDAS", "OTROS INALAMBRICAS", "SATELITAL", "RADIOENLACE")))

#Velocidad requerida por centro poblado
BD_consolidada$CONEXIONES <- as.numeric(BD_consolidada$CONEXIONES)
BD_consolidada$VELOCIDAD_CONEXIONES = BD_consolidada$VELOCIDAD_GARANTIZADA_BAJADA * BD_consolidada$CONEXIONES

#A nivel de distrito
BD_consolidada <- BD_consolidada %>%
  dplyr::group_by(MES, DEPARTAMENTOS, PROVINCIA, DISTRITO, EMPRESA) %>%
  dplyr::summarise(CONEXIONES = sum(CONEXIONES, na.rm=TRUE),
            VELOCIDAD_CONEXIONES = sum(VELOCIDAD_CONEXIONES, na.rm=TRUE))

#Calcular el tamaño de mercado
BD_consolidada <- BD_consolidada %>% 
  dplyr::group_by(MES, DEPARTAMENTOS, PROVINCIA, DISTRITO) %>% 
  dplyr::mutate(TOTAL_CON = sum(CONEXIONES, na.rm=TRUE),
         TOTAL_CAP = sum(VELOCIDAD_CONEXIONES, na.rm=TRUE))

#Calcular participación de mercado (Importante para hcer el cruce de la oferta)
BD_consolidada$share_CON <- BD_consolidada$CONEXIONES / BD_consolidada$TOTAL_CON
BD_consolidada$share_CON <- round(BD_consolidada$share_CON, 2)

BD_consolidada$share_CAP <- BD_consolidada$VELOCIDAD_CONEXIONES / BD_consolidada$TOTAL_CAP
BD_consolidada$share_CAP <- round(BD_consolidada$share_CAP, 2)

#Dejamos los meses y las participaciones creadas como números
BD_consolidada$MES <- as.numeric(BD_consolidada$MES)
BD_consolidada$share_CON <- as.numeric(BD_consolidada$share_CON)
BD_consolidada$share_CAP <- as.numeric(BD_consolidada$share_CAP)

#Clasificamos a los proveedores minoristas según su integración aguas arriba y su demanda de fibra a terceros

  #Lista de empresas integradas
  INTEGRADA <- c("AMERICA MOVIL PERU S.A.C.", "AMERICATEL PERU S.A.",
                  "CENTURYLINK PERU S.A.C.", "DIRECTV PERU S.R.L.", "DKR VISION S.R.L.",
                  "ENTEL PERU S.A.","FIBERLUX TECH S.A.C.", "GTD PERU S.A.", "INTERNEXA S.A.", 
                  "IWAY TELECOM S.A.C.", "LAZUS PERU S.A.C.", "LELI TV E.I.R.L.", "MOCHE INVERSIONES S.A.",
                  "MULTIMEDIA ALFA S.A.C.", "NETLINE PERU S.A.C.", "OLO DEL PERU S.A.C.",
                  "OPTICAL NETWORKS S.A.C.", "OPTICAL TECHNOLOGIES S.A.C",
                  "TELEFONICA DEL PERU S.A.A.", "TELXIUS TELECOM S.A.", "TI SPARKLE PERU S.A.",
                  "TVS WIRELESS S.A.C.", "VIETTEL PERU S.A.C.", "WIGO S.A.", "WI-NET TELECOM S.A.C.",
                  "WINNER SYSTEMS S.A.C.", "WORLDS TV S.A.C.")
  
    #Creamos una dummy que toma el valor de 1 si la empresa está integrada
  BD_consolidada$INTEGRADA <- ifelse(BD_consolidada$EMPRESA %in% INTEGRADA, 1, 0)
  
  #Lista de empresas integradas que demandan fibra de terceros
  FIBRA_TERCEROS <- c("AMERICA MOVIL PERU S.A.C.", "AMERICATEL PERU S.A.", "DIRECTV PERU S.R.L.",
                      "INTERNEXA S.A.", "OLO DEL PERU S.A.C.", "TELEFONICA DEL PERU S.A.A.",
                      "TELXIUS TELECOM S.A.", "TI SPARKLE PERU S.A.", "TVS WIRELESS S.A.C.",
                      "VIETTEL PERU S.A.C.")
  
  #Creamos una dummy que toma el valor de 1 si la empresa demanda fibra a terceros
  BD_consolidada$FIBRA_TERCEROS <- ifelse(BD_consolidada$EMPRESA %in% FIBRA_TERCEROS, 1, 0)

  #Creamos un contador para los competidores
  BD_consolidada$COMPETIDORES <- 1
  
#Unimos con el excel de proyectos
  
  #Leemos la base de proyectos
  Proyectos <- read_excel("../../2 Bases/2 Georeferencias/Distritos de las LT de Acciona.xlsx", sheet = "Proyectos")
  
  #Realizamos el merge
  BD_consolidada <- BD_consolidada %>%
    left_join(Proyectos, by = c("DEPARTAMENTOS", "PROVINCIA", "DISTRITO"))

  # Exportamos la BD
  write_xlsx(BD_consolidada, "../../2 Bases/1 Osiptel/Bases consolidadas/Consolidada_Internet_Fijo.xlsx")
  
  
    
#Filtramos solo para los distritos que asociados a una LT
  
  #Nos quedamos solo con los distritos de interpes
  BD_lt <- filter(BD_consolidada, !is.na(LINEA_TRANSMISION))
  
  #Agregamos un identificador del servicio en cuestión
  BD_lt$SERVICIO <- "INTERNET FIJO"

  # Exportamos la BD
  write_xlsx(BD_lt, "../../2 Bases/1 Osiptel/Bases consolidadas/Competidores_Internet_Fijo.xlsx")
  
  #Borramos la BD_lt
  rm(BD_lt)
  
#-------------------------------------------------------#
### 4. Departamentos y distritos según conexiones #######
#-------------------------------------------------------#


  #En esta sección crearemos una nueva base a partir de la BD_consolidada
  
  
  #Creamos una base nueva
  
  BD_departamental <- BD_consolidada %>%
    group_by(MES, DEPARTAMENTOS) %>%
    summarize(TOTAL_CON = sum(TOTAL_CON, na.rm = TRUE),
              TOTAL_CAP = sum(TOTAL_CAP, na.rm = TRUE),
              LOG_CON = log(TOTAL_CON),
              LOG_CAP = log(TOTAL_CAP)) %>%
    ungroup()

#Nos quedamos solo con el último mes
  
  BD_departamental <- BD_departamental %>%
    filter(MES == 3)
    
#Llamamos al shp para construir el mapa
  
  #Leemos la base de departamentos
  
  depart <- st_read("../../2 Bases/2 Georeferencias/2 Mapas genericos/BAS_LIM_DEPARTAMENTO.shp")
  depart <- st_as_sf(depart, coords = c("LON", "LAT"), crs = "EPSG:4326")
  
#Merge
  
  df_dep <- depart |>
    left_join(BD_departamental, by = c("NOMBDEP"="DEPARTAMENTOS"))
  
  
  # Exportamos las BD
  write_xlsx(df_dep, "../../2 Bases/1 Osiptel/Bases consolidadas/BD_DEPARTAMENTOS.xlsx")
  
#Graficamos
  
 conexiones_dep <- ggplot() + # Añadimos las capas
    geom_sf(data = depart, color = "black", 
            fill = NA, size = 0.1) +
    geom_sf(data = df_dep, aes(fill = LOG_CON), size = 0.1) +  
    scale_fill_gradient(name = "", 
                        low = "white", 
                        high = "red") +
    theme_void() +
    guides(fill = "none") 
  
  # Guardamos el gráfico en la carpeta especificada
  ggsave(filename = "../2 RStudio/Gráficos/conexiones.png", plot = conexiones_dep)
  
#Graficos distritales

  #Agrupamos la base 
  
  BD_distrital <- BD_consolidada %>%
    group_by(MES, LINEA_TRANSMISION, DEPARTAMENTOS, PROVINCIA, DISTRITO) %>%
    summarize(TOTAL_CON = mean(TOTAL_CON, na.rm = TRUE),
              TOTAL_CAP = mean(TOTAL_CAP, na.rm = TRUE),
              LOG_CON = log(TOTAL_CON),
              LOG_CAP = log(TOTAL_CAP)) %>%
    ungroup()

  #Nos quedamos solo con el último mes
  
  BD_distrital <- BD_distrital %>%
    filter(MES == 3)
  
  
  BD_distrital_2  <- BD_distrital %>% 
    filter(!is.na(LINEA_TRANSMISION)) 
  
  sum(BD_distrital_2$TOTAL_CON)
  
  #Calculamos el promedio excluyendo a lima
  
  BD_distrital <- BD_distrital %>%
    mutate(CON_1 =  mean(TOTAL_CON[PROVINCIA != "LIMA"], na.rm = TRUE))
  
  #Reemplazamos
  
  BD_distrital <- BD_distrital %>%
    mutate(CON_1 = ifelse(!is.na(LINEA_TRANSMISION), TOTAL_CON, CON_1))
  
  #Llamamos al shp para construir el mapa
  
  #Leemos la base de departamentos
  
  dist <- st_read("../../2 Bases/2 Georeferencias/2 Mapas genericos/BAS_LIM_DISTRITOS.shp")
  dist <- st_as_sf(dist, coords = c("LON", "LAT"), crs = "EPSG:4326")
  
  #Merge

  df_dist <- left_join(BD_distrital, dist,
                            by = c("DEPARTAMENTOS" = "NOMBDEP",
                                   "PROVINCIA" = "NOMBPROV",
                                   "DISTRITO" = "NOMBDIST"))
  
  
  #Graficamos
  
  conexiones_dist <-  ggplot() +
    geom_sf(data = dist, aes(geometry = geometry), color = "black", fill = "white", size = 0.1) +
    geom_sf(data = df_dist, aes(geometry = geometry, fill = CON_1), size = 0.1) +
    scale_fill_gradient(name = "", low = "white", high = "red") +
    theme_void() +
    guides(fill = "none")
  
  
  #conexiones_dist
  
  # Guardamos el gráfico en la carpeta especificada
  ggsave(filename = "../2 RStudio/Gráficos/conexiones_distritales.png", plot = conexiones_dist)

  #Nos quedamos solo con las observaciones que tienen una LT asociada
  
  df_dist <- filter(df_dist, !is.na(LINEA_TRANSMISION))
  
  conexiones_dist_1 <-  ggplot() +
    geom_sf(data = df_dist, aes(geometry = geometry), color = "black", fill = "white", size = 0.1) +
    geom_sf(data = df_dist, aes(geometry = geometry, fill = CON_1), size = 0.1) +
    scale_fill_gradient(name = "", low = "white", high = "red") +
    theme_void() +
    guides(fill = "none")
  
  conexiones_dist_1
  
  # Guardamos el gráfico en la carpeta especificada
  ggsave(filename = "../2 RStudio/Gráficos/conexiones_distritales_1.png", plot = conexiones_dist_1)
  
  
  # Eliminar elementos del Proyecto
  rm(list = ls()[grepl("df", ls())])
  rm(dist, depart, BD_departamental, BD_distrital)
  rm(conexiones_dep, conexiones_dist, conexiones_dist_1)
  
#------------------------------------------------------------#
### 5. Distritos según conexiones y tipo de minoristas #######
#------------------------------------------------------------#
  
  #A nivel de distrito

  
  BD_final <- BD_consolidada %>%
    dplyr::group_by(MES, DEPARTAMENTOS, PROVINCIA, DISTRITO, LINEA_TRANSMISION) %>%
    dplyr::summarize(TOTAL_CON = mean(TOTAL_CON, na.rm = TRUE),
              TOTAL_CAP = mean(TOTAL_CAP, na.rm = TRUE),
              LOG_CON = log(TOTAL_CON),
              LOG_CAP = log(TOTAL_CAP),
              N_COMPETIDORES = sum(COMPETIDORES, na.rm = TRUE),
              N_INTEGRADAS = sum(INTEGRADA, na.rm = TRUE),
              N_INT_TERCEROS = sum(FIBRA_TERCEROS, na.rm = TRUE),
              CON_NO_INT = 1 - sum(ifelse(INTEGRADA == 1, share_CON, 0), na.rm = TRUE),
              CAP_NO_INT = 1 - sum(ifelse(INTEGRADA == 1, share_CAP, 0), na.rm = TRUE),
              CON_NO_TER = 1 - sum(ifelse(FIBRA_TERCEROS == 1, share_CON, 0), na.rm = TRUE),
              CAP_NO_TER = 1 - sum(ifelse(FIBRA_TERCEROS == 1, share_CAP, 0), na.rm = TRUE)) %>%
    ungroup()
  
  #Transformamos los negativos en 0
  BD_final$CON_NO_INT <- ifelse(BD_final$CON_NO_INT<0,0,BD_final$CON_NO_INT)
  BD_final$CAP_NO_INT <- ifelse(BD_final$CAP_NO_INT<0,0,BD_final$CAP_NO_INT)
  
  BD_final$CON_NO_TER <- ifelse(BD_final$CON_NO_TER<0,0,BD_final$CON_NO_TER)
  BD_final$CAP_NO_TER <- ifelse(BD_final$CAP_NO_TER<0,0,BD_final$CAP_NO_TER)
  
  #Nos quedamos solo con las observaciones que tienen una LT asociada
  BD_final <- filter(BD_final, !is.na(LINEA_TRANSMISION))
  
  #Agregamos un identificador del servicio en cuestión
  BD_final$SERVICIO <- "INTERNET FIJO"
  
  
  # Exportamos las BD
  write_xlsx(BD_final, "../../2 Bases/1 Osiptel/Bases consolidadas/BD_Final_Internet_Fijo.xlsx")
  
  

  # write_xlsx(BD_distrital, "../../2 Bases/1 Osiptel/Bases consolidadas/BD_distrital.xlsx")
  
