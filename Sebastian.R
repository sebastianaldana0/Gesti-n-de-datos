#Pruebas de Sebastian

library(tidyverse)
library(scales)
library(readxl)
library(ggcorrplot)
library(visreg)
library(nortest)
library(lmtest)
library(ggthemes)
library(ggpmisc)


Base_de_datos_EAC=read.csv("EAC.CSV",sep = ";") %>% 
  select(IDOJ1,BRUTA,ENERGIA,PUBLICI,
         ADECUA,SUELDOS,VENTA,APRENDIZ,TOTMUJ,
         TOTHOM,GASTOS) %>% 
  rename(Tipo_organizacion=1,Produccion=2,Publicidad=3,
         Mantenimiento=4) %>% 
  filter(VENTA!=0,!is.na(VENTA),!is.na(Publicidad)) %>% 
  mutate(
    VENTA = as.numeric(VENTA),
    VENTA=log(VENTA),
    Publicidad = as.numeric(Publicidad),
    Mantenimiento=as.numeric(Mantenimiento),
    APRENDIZ=as.numeric(APRENDIZ),
    Produccion=as.numeric(Produccion),
    TOTMUJ=as.numeric(TOTMUJ),
    TOTHOM=as.numeric(TOTHOM)
  )


#Modelo

Modelo_final_EAC=lm(VENTA~Publicidad+Mantenimiento+APRENDIZ+
                      Produccion+TOTMUJ+TOTHOM,Base_de_datos_EAC,na.action = na.omit)
summary(Modelo_final_EAC)
x=Modelo_final_EAC

plot(x,which=1)
plot(x,which=2)
plot(x,which=3)
plot(x,which=5)
