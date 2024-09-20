####################################################################
# Proyecto: Acciona BR
# Encargados: SMR, LL, FTQ
# Objetivo: Sistematización de bases de datos
####################################################################
#+---------------------+
#       Entorno    : |
#+---------------------+

#Limpiamos el entorno, consola y gráficos

rm(list = ls())
graphics.off()
cat("\014")
options(scipen = 0)

#+---------------------+
#       Librerias    : |
#+---------------------+

#Instalar librerías
# install.packages('stringr')
# install.packages('RSelenium')
# install.packages('xml2')
# install.packages('rvest')
# install.packages('htt')
# install.packages('openxlsx')

#cargamos las librerías
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
library("readr")
library("geosphere")
library("sf")
library("readr")
library("gdata")

#+-----------------------------------+
#             Seteo inicial         |
#+----------------------------------+



#Definir la ruta con todos los archivos (LL)

# Establecemos la ruta

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


trabajadas <- "../../2 Bases/2. Trabajadas/2. ANEEL"
  
#+-----------------------------------+
#    1: Importar PDE 2032 |
#+----------------------------------+

## 1.1: Base de proyectos futuros

  # Abrir la base de datos  
  PDE2032 <- read_excel("../../2 Bases/1. Originales/1. ANEEL/2. PDE 2032/PMO PDE2032.xlsx", sheet = "Futuras")
  
#+-----------------------------------+
#    2: Importar base de proyectos  |
#+----------------------------------+

## 2.1: Bases de todos los proyectos

  # Importar el archivo base
  BD <- read.csv(("../../2 Bases/1. Originales/1. ANEEL/1. SIGA/siga-empreendimentos-geracao.csv"), sep=";")

  # Mantener solo las observaciones de SP
  BD <- BD %>%
    subset(SigUFPrincipal == "SP" | SigUFPrincipal == "BA" |SigUFPrincipal == "TO" | SigUFPrincipal == "PI" | SigUFPrincipal == "PR")

  # Modificar el nombre de los proyectos
  BD$CodCEG <-substr(BD$CodCEG,1,nchar(BD$CodCEG)-2)
  
  # Convertir en valores de datos
  
  BD$NumCoordNEmpreendimento <- gsub(",", ".", BD$NumCoordNEmpreendimento)
  BD$NumCoordEEmpreendimento <- gsub(",", ".", BD$NumCoordEEmpreendimento)
  
  BD$NumCoordNEmpreendimento <- as.numeric(BD$NumCoordNEmpreendimento)
  BD$NumCoordEEmpreendimento <- as.numeric(BD$NumCoordEEmpreendimento)

  # Eliminar observaciones fuera del rango de SP, BA. TO y PI (zona de influencia)
  
  BD$LONGITUD <- BD$NumCoordEEmpreendimento
  BD$LATITUD <- BD$NumCoordNEmpreendimento
  
  BD <- BD %>%
    filter(SigUFPrincipal == "SP" | SigUFPrincipal == "BA" |SigUFPrincipal == "TO" | SigUFPrincipal == "PI" | SigUFPrincipal == "PR" & LATITUD > -29) 
  
  BD <- BD %>%
    filter(LATITUD != LONGITUD )
  
  # Modificar casos de observaciones 
  
  # Convertir a geometry
  BD <- BD %>%
          sf::st_as_sf(coords = c("LONGITUD", "LATITUD"), crs=4326)

  # Exportar como SHP
  sf::st_write(BD, "../../2 Bases/2. Trabajadas/2. ANEEL/geo.shp", append=FALSE)

## 2.2: Bases segmentadas de los proyectos
  
  # Crear SHP de proyectos en construcción 
  
    #Renombrar la BD
    Usinas_1 <- BD %>%
                  filter(DscFaseUsina == 'Construção')
    
    #Mantener variables relevantes
    Usinas_1 <- Usinas_1 %>%
                  select(-c(DatGeracaoConjuntoDados, SigUFPrincipal, NumCoordNEmpreendimento, 
                            NumCoordEEmpreendimento, DscSubBacia, DscMuninicpios))
    
    #Crear referencia
    Usinas_1 <- Usinas_1 %>%
      sf::st_as_sf(coords = c("LONGITUD", "LATITUD"), crs=4326)
    
    #Crear SHP
    options("SHAPE_RESTORE_SHX" = "YES")
    
    sf::st_write(Usinas_1, "../../2 Bases/2. Trabajadas/2. ANEEL/En Construccion.shp", append=FALSE)
  
  # Crear SHP de proyectos todavía no iniciados
  
    #Renombrar la BD
    Usinas_2 <- BD %>%
      filter(DscFaseUsina == 'Construção não iniciada')
    
    #Mantener variables relevantes
    Usinas_2 <- Usinas_2 %>%
      select(-c(DatGeracaoConjuntoDados, SigUFPrincipal, NumCoordNEmpreendimento, 
                NumCoordEEmpreendimento, DscSubBacia, DscMuninicpios))
    
    #Crear referencia
    Usinas_2 <- Usinas_2 %>%
      sf::st_as_sf(coords = c("LONGITUD", "LATITUD"), crs=4326)
    
    #Crear SHP
    
    options("SHAPE_RESTORE_SHX" = "YES")
    sf::st_write(Usinas_2, "../../2 Bases/2. Trabajadas/2. ANEEL/No iniciada.shp", append=FALSE)
    
#+-----------------------------------+
#    3: Importar las bases de proyectos en proceso |
#+----------------------------------+

## 3: Base de plantas de generación eléctrica en estudio

# Abrir base de datos de datos
    
  # DRO
  Usinas_3_1 <- read_excel(paste0("../../2 Bases/2. Trabajadas/2. ANEEL/DROs - SP-BA-TO-PI.xlsx"), col_names = TRUE)

  # PCH e UHE
  Usinas_3_2 <- read_excel(paste0("../../2 Bases/2. Trabajadas/2. ANEEL/PCHs e UHEs em estudo - SP-BA-TO-PI.xlsx"), col_names = TRUE)

# Homogeneizar variables
  
  # Eliminar variables
  
    #Usinas 1
    Usinas_3_1 <- Usinas_3_1 %>%
                    dplyr::select(-c('VigênciadoAto'))
    
    Usinas_3_1$UF <- substr(Usinas_3_1$Município, nchar(Usinas_3_1$Município) - 1, nchar(Usinas_3_1$Município))
  
    #Usinas 2
    Usinas_3_2 <- Usinas_3_2 %>%
      dplyr::select(-c(Processo))
  
  
    Usinas_3_2$CEG <- ""
  
    # Renombrar variables 
  
    #Usinas 1
    Usinas_3_1 <-  Usinas_3_1 %>%
                    rename("PotênciaMW" = 6)
    
    Usinas_3_1$PotênciaMW <- Usinas_3_1$PotênciaMW/1000 
    
   
    #Usinas 2
    Usinas_3_2 <-  Usinas_3_2 %>%
      rename("Latitude" = 13,
             "Longitude" = 14)
  
  # Convertir valores de variables
    
    Usinas_3_2$DatadePublicação <- as.Date(Usinas_3_2$DatadePublicação, format = "%d-%m-%Y")

# Appendear ambas bases

  # Append
    Usinas_3 <- dplyr::bind_rows(Usinas_3_1, Usinas_3_2)
    
  # Convertir a geometry
    
    Usinas_3 <- Usinas_3[complete.cases(Usinas_3$Latitude), ]
    Usinas_3 <- Usinas_3[complete.cases(Usinas_3$Longitude), ]
    
    # Convertir Latitude y Longitude a numérico en el dataframe Usinas_3
    Usinas_3$Latitude <- as.numeric(Usinas_3$Latitude)
    Usinas_3$Longitude <- as.numeric(Usinas_3$Longitude)
    
    
    
    Usinas_3 <- Usinas_3 %>%
      sf::st_as_sf(coords = c("Longitude", "Latitude"), crs=4326)
    
    Usinas_3 <- Usinas_3 %>%
      filter(Latitude != Longitude )
    
  # Exportar como SHP
   options("SHAPE_RESTORE_SHX" = "YES")
    
    sf::st_write(Usinas_3, "../../2 Bases/2. Trabajadas/2. ANEEL/En estudio.shp", append=FALSE)
    
    
#############Subestaciones proyectadas
    
    # Lectura de los archivos
    
    #Leemos el CSV con dcofidicación UTF-8
    SE <- read.csv("../../2 Bases/1. Originales/1. ANEEL/Subestações - Expansão Planejada.csv", sep =";", encoding = "UTF-8")
    
    # Eliminamos columnas no necesarias
    
    SE <- SE %>%
      dplyr::select(-c(x, y))
    
#+-----------------------------------+
#    4: SISTEMATIZACIÓN DE PROYECTO  |
#+----------------------------------+ 
    
  # Leer códigos de proyectos
    
    Códigos_de_proyectos_priorizados <- read_delim("../../2 Bases/2. Trabajadas/1. Filtros/Códigos de proyectos priorizados.csv",  
                                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)
    
    Códigos_de_proyectos_priorizados <-  Códigos_de_proyectos_priorizados %>% 
                                            rename(NmEmprn=`Nombre del Proyecto` )
    
    Códigos_de_proyectos_priorizados$NmEmprn <- trim(Códigos_de_proyectos_priorizados$NmEmprn) 
      
  # Leemos los shapefile
    
    en_estudio <- st_read("../../2 Bases/2. Trabajadas/2. ANEEL/En estudio.shp")
    
    en_estudio <- en_estudio %>% 
                    rename(NmEmprn=Emprndm)
      
    no_iniciada <- st_read("../../2 Bases/2. Trabajadas/2. ANEEL/No iniciada.shp")
    
    en_construccion <- st_read("../../2 Bases/2. Trabajadas/2. ANEEL/En Construccion.shp")
    
  # Unimos los shp
    
    proyectos_lotes <- dplyr::bind_rows(no_iniciada, en_construccion)
    proyectos_lotes <- dplyr::bind_rows(proyectos_lotes, en_estudio)
    
    proyectos_lotes$NmEmprn <- trim(proyectos_lotes$NmEmprn) 
    
  # Crear una nueva fila con los valores deseados
    Alta <- data.frame(NmEmprn = "3 Alta", CodCEG = "033220-8", SgTpGrc = "PCH", Latitude = -11.35748108, Longitude = -46.92390872)  
    Itaóca <- data.frame(NmEmprn = "Itaóca", CodCEG = "037176-9", SgTpGrc = "PCH", Latitude = -24.67056753, Longitude = -48.86024787)
    
    nuevos <- bind_rows(Alta, Itaóca)
    
    nuevos_py <-  nuevos  %>%
      sf::st_as_sf(coords = c("Longitude", "Latitude"), crs=4326)
    
  # Añadir la nueva fila a la base de datos proyectos_lotes
    proyectos_lotes <- bind_rows(proyectos_lotes, nuevos_py)
    
  # Cruzar con información y filtrar
    
    merge_proyectos = merge(x=Códigos_de_proyectos_priorizados,y=proyectos_lotes,by="NmEmprn")
    rev = anti_join(Códigos_de_proyectos_priorizados,proyectos_lotes,by="NmEmprn")
    
  #Guardamos los shp
  
    options("SHAPE_RESTORE_SHX" = "YES")  
    st_write(merge_proyectos,"../../2 Bases/2. Trabajadas/2. ANEEL/Proyectos totales.shp", append = FALSE )
    
    
    
    
    
       
    
    
    
    
     
    