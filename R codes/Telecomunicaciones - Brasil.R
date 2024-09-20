#===========================================================================#
# Encargados: FTQ
# Objetivo: Sistematización y análisis de bases de datos
#Encoding: UTF8
#===========================================================================#


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
library("sp")
library("sf")
library("dplyr")
library("tidyverse")
library("raster")

#+-----------------------------------+
#    Descripción de procedimientos: |
#+----------------------------------+




#--------------------------------------#
########## 1.1 Municipios 2021 #########
#-------------------------------------#

# Establecemos la ruta

  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  getwd()
  

# Lectura de los archivos

  #Leemos el CSV con dcofidicación UTF-8
  backhaul <- read.csv("../../2 Bases/1. Originales/4. ANATEL/1. Mayorista/1. Backhaul/backhaul_municipios_2021.csv", sep =";", encoding = "UTF-8")
 
# Renombrar variables de inerés
  
  backhaul <- backhaul %>% 
                rename(#CD_MUN=X.U.FEFF.Código.IBGE,
                       otras=Outras.empresas..Fibra.) 

#Filtramos solo las observaciones con FO en 2021
  
  backhaul <- backhaul  %>%
    subset(X2021 == "Fibra Óptica")

  
# Filtramos las observaciones de los municipios de interés
  
  backhaul <- backhaul  %>%
                subset(UF == "SP" | UF == "BA" | UF == "PI" | UF == "TO" | UF == "PR" )

  
#Uniformizamos las obs de brisanet
 # backhaul$otras <- gsub("^Brisanet.*", "Fibra óptica", backhaul$otras)
  
  
  
  # Contar las ocurrencias de cada observación en la columna otras
  otros_op <- table(backhaul$otras)
  
  # Ordenar las ocurrencias de mayor a menor y tomar las 10 primeras
  top_otros <- head(sort(otros_op, decreasing = TRUE), 20)
  
  # Mostrar las 10 observaciones más repetidas y su número de repeticiones
  print(top_otros)
  
  #Cilnet Comunicação e Informática Ltda
  #LPNET Telecomunicações Ltda
  #Copel
  #Copel
  #Brasilnet
  #INFORBARRA COMERCIO DE MATERIAIS DE INFORMATICA LTDA
  # W. H. DOS ANJOS MENEZES
  #MOB SERVICOS DE TELECOMUNICACOES LTDA
  #HTM SERVICOS DE TELECOMUNICACOES EIRELI - EPP
  #ZAP TELECOMUNICACOES LTDA  - ME
  # Eletronet
  # HE-NET / FIBER CONECTIVIDADE LTDA ME
  # 
  
  
  
  #Uniformizamos las obs de las empresas más importantes en "otros"

  backhaul$otras <- gsub(".*Cilnet.*", "Cilnet", backhaul$otras, ignore.case = TRUE)
  backhaul$otras <- gsub(".*LPNET.*", "LPNET", backhaul$otra, ignore.case = TRUE)
  backhaul$otras <- gsub(".*Copel.*", "Copel", backhaul$otras, ignore.case = TRUE)
  backhaul$otras <- gsub(".*Inforbarra.*", "Inforbarra", backhaul$otras, ignore.case = TRUE)
  backhaul$otras <- gsub(".*Dos anjos.*", "Dos_anjos", backhaul$otras, ignore.case = TRUE)
  backhaul$otras <- gsub(".*MOB SERVICOS.*", "Mob", backhaul$otras, ignore.case = TRUE)
  backhaul$otras <- gsub(".*HTM SERVICOS.*", "HTM", backhaul$otras, ignore.case = TRUE)
  backhaul$otras <- gsub(".*ZAP.*", "ZAP", backhaul$otras, ignore.case = TRUE)
  backhaul$otras <- gsub(".*Eletronet.*", "Eletronet", backhaul$otras, ignore.case = TRUE)
  backhaul$otras <- gsub(".*He-net.*", "He_Net", backhaul$otras, ignore.case = TRUE)
  
  #Eliminamos las observaciones de otras distintas a las de los operadores principales
  
  backhaul <- backhaul %>%
    mutate(otras = if_else(
      grepl("^(Copel|Cilnet|LPNET|Brasilnet|Inforbarra|Dos_anjos|Mob|HTM|ZAP|Eletronet|He_Net)$", otras, ignore.case = TRUE),
      otras, ""))
 
  
# Creamos columnas para las prinicipales empresas identificadas
  
  # Lista de nombres de las nuevas columnas
  principales <- unique(backhaul$otras)
  
  # Eliminamos cadenas vacías de la lista de nombres de columnas
  principales <- principales[principales != ""]
  
  #Creamos las columnas con un loop
  
  for (empresa in principales) {
    backhaul <- backhaul %>%
      mutate({{ empresa }} := if_else(otras == empresa, otras, ""))
  }
  
  #Cambiamos las observaciones a "Fibra óptica" para uniformizarlas con el resto
  for (nombre_col in principales) {
    backhaul[[nombre_col]] <- ifelse(backhaul[[nombre_col]] == nombre_col, "Fibra Óptica", backhaul[[nombre_col]])
  }
  
  
  # Filtro de variables de interés
  
  backhaul <- backhaul %>%
    dplyr::select(c(CD_MUN, Município, UF, Claro, Oi, TIM, Vivo, SKY, 
                    Algar, Sercomtel, Telebrás, Abrint.Abramult, NEOTV, RedeTelesul,
                    Solintel, ZAP, Eletronet, Dos_anjos, Mob, HTM, Inforbarra, LPNET, Cilnet, He_Net))
  
  # Realizar un reshape
  
  backhaul <- backhaul  %>% 
    pivot_longer(
      cols = !c(CD_MUN, Município, UF), 
      names_to = "operador", 
      values_to = "tecnología"
    )
  
  
# Conteo  
  
  #Usamos temporalmente otra base para no alterar la original
  backhaul_todos <- backhaul %>%
    subset(tecnología == "Fibra Óptica" |tecnología == "Leased Line (LL)")
  
  #Contamos cuántos operadores de fibra optica hay por municipio
  backhaul_todos <- backhaul_todos %>%
    group_by(CD_MUN) %>%
    mutate(num_operadores = sum(tecnología == "Fibra Óptica" |tecnología == "Leased Line (LL)"))
  
  #Mantenemos solo una observación por municipio
  backhaul_todos <- backhaul_todos %>%
    group_by(CD_MUN) %>%
    slice(1)
  
  #Mantenemos las columnas de interés
  backhaul_todos <- backhaul_todos %>%
    dplyr::select(c(CD_MUN, Município, tecnología, num_operadores))
  
 #Exportamos
   write.csv(backhaul_todos, "../../2 Bases/2. Trabajadas/5. ANATEL/backhaul_todos.csv", row.names = FALSE)
  

#Volvemos a la base original
  
# Considerar únicamente aquellas que son de fibra óptica 
  backhaul <- backhaul %>%
                subset(tecnología == "Fibra Óptica" |
                         tecnología == "Leased Line (LL)" )

  
# Crear el loop para exportar csv de cada operador
  
  columnas <- c("Claro","Oi", "TIM", "Vivo", "SKY", "Algar", "Sercomtel", "Telebrás", "Abrint.Abramult", "NEOTV", "RedeTelesul", "Solintel",
                "ZAP", "Eletronet", "Dos_anjos", "Mob", "HTM", "Inforbarra", "LPNET", "Cilnet","He_Net")
  
   for (i in columnas){
    
    operador <- backhaul %>%
                          subset(operador == i)
    
    #i <- merge(municipios, operadores, by = 'CD_MUN')
    operador$cobertura <- 1 
    
    nombre_archivo <- paste0("../../2 Bases/2. Trabajadas/5. ANATEL/1. Por operador/", i, ".csv")
    nombre_archivo
    
    write.csv(operador, file = nombre_archivo, row.names = FALSE)
  }
  

  



