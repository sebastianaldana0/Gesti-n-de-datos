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

educacion=read.csv("educacion.csv", sep= ";") %>% 
  select(DIRECTORIO,P8587) %>% rename("Ultimo grado alcanzado"=2)

caracteristicas_hogar=read.csv("Características_composición.CSV",sep= ";") %>% 
  select(DIRECTORIO,P6020,P6040,P6051,P5502) %>% rename(Sexo=2,Edad=3,Parentesco=4,Casado=5) %>% 
  filter(Parentesco==1)#jefes del hogar

tenencia=read.csv("tenencia y financiación de la vivienda.CSV",sep=";") %>%
  mutate(Arriendo_estimacion=rowSums(select(., P5130, P5140), na.rm = TRUE)) %>% 
  select(DIRECTORIO,Arriendo_estimacion) %>% rename(estimacion=2,Arriendo=3)

trabajo=read.csv("Fuerza de trabajo.CSV",sep=";") %>% 
  select(DIRECTORIO,P8624,P415) %>% rename(Ingresos_mes=2,Horas_trabajadas_semana=3)
  


#Base de datos final

Base_datos=datos_hogar %>% inner_join(educacion,by="DIRECTORIO") %>% 
  inner_join(caracteristicas_hogar,by="DIRECTORIO")



#Modelos


#Pruebas modelo


#Graficas