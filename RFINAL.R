#Librerias
library(tidyverse)
library(scales)
library(readxl)
library(ggcorrplot)
library(visreg)
library(nortest)
library(lmtest)

#Base de datos

datos_hogar=read.csv("datos_hogar.csv", sep = ";") %>% 
  select(DIRECTORIO,I_HOGAR) %>% rename("Ingreso del hogar"=2)

educacion=read.csv("educacion.csv", sep= ";")

caracteristicas_hogar=read.csv("Características_composición.CSV",sep= ";") %>% 
  select(DIRECTORIO,P6020,P6040,P6051) %>% rename(Sexo=2,Edad=3,Parentesco=4) %>% 
  filter(Parentesco==1)#jefes del hogar

#Base de datos final

Base_datos=datos_hogar %>%merge(educacion,by="DIRECTORIO") %>% 
  merge(caracteristicas_hogar,by="DIRECTORIO")



#Modelos


#Pruebas modelo


#Graficas