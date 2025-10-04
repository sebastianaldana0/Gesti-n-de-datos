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
  select(DIRECTORIO,I_HOGAR,PERCAPITA,CANT_PERSONAS_HOGAR) %>% 
  rename("Ingreso del hogar"=2,"Cantidad de personas en el hogar"=4) %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)


educacion=read.csv("educacion.csv", sep= ";") %>% 
  select(DIRECTORIO,P8587) %>% 
  group_by(DIRECTORIO) %>%
  filter(P8587 == max(P8587)) %>%
  rename("Ultimo grado alcanzado"=2)  %>%
  ungroup() %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)

caracteristicas_hogar=read.csv("Características_composición.CSV",sep= ";") %>% 
  select(DIRECTORIO,P6020,P6040,P6051,P5502) %>% rename(Sexo=2,Edad=3,Parentesco=4,Casado=5) %>% 
  filter(Parentesco==1)#jefes del hogar

tenencia=read.csv("tenencia y financiación de la vivienda.CSV",sep=";") %>%
  filter(P5130!=99) %>% 
  mutate(Arriendo_estimacion=rowSums(select(., P5130, P5140), na.rm = TRUE)) %>% 
  select(DIRECTORIO,Arriendo_estimacion) %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)
 

trabajo=read.csv("Fuerza de trabajo.CSV",sep=";") %>% 
  select(DIRECTORIO,P8624,P415,P8634) %>% rename(Ingresos_mes=2,Horas_trabajadas_semana=3,
                                                 "Lugar de trabajo"=4) %>% 
  group_by(DIRECTORIO) %>%
  filter(Ingresos_mes == max(Ingresos_mes)) %>%
  ungroup()

vivienda=read.csv("Datos de la vivienda.csv",sep=";") %>% 
  select(DIRECTORIO,P8520S1A1,CLASE) %>% rename(Estrato=2,Ubicacion=3) %>% 
  filter(Estrato!=0,Estrato!=8,Estrato!=9) %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)

salud=read.csv("Salud.CSV", sep=";")  %>%
  select(DIRECTORIO,P6090) %>% 
  group_by(DIRECTORIO) %>%
  filter(P6090 == max(P6090)) %>%
  rename(Afiliado=2)%>%
  ungroup() %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)

Muestra=read.csv("muestral.CSV",sep=";") %>% 
  select(DIRECTORIO,MPIO) %>% rename(Municipio=2) %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)

#Base de datos final

Base_datos=inner_join(Muestra,caracteristicas_hogar,by="DIRECTORIO") %>% 
  inner_join(tenencia,by="DIRECTORIO") %>% inner_join(datos_hogar,by="DIRECTORIO") %>% 
  inner_join(vivienda,by="DIRECTORIO") %>% inner_join(educacion,by="DIRECTORIO") %>% 
  inner_join(salud,by="DIRECTORIO") %>% select(DIRECTORIO,Municipio,Estrato,`Ingreso del hogar`,
                                               Arriendo_estimacion,`Cantidad de personas en el hogar`,
                                               Sexo,Edad,`Ultimo grado alcanzado`,
                                               Afiliado,Casado,PERCAPITA) %>%
  filter(Municipio==76001)
  

#Modelos

Modelo_sexo=lm(`Ingreso del hogar`~Sexo,Base_datos)
summary(Modelo_sexo)

Modelo_estrato=lm(`Ingreso del hogar`~Estrato,Base_datos)
summary(Modelo_estrato)

Modelo_edad=lm(`Ingreso del hogar`~Edad,Base_datos)
summary(Modelo_edad)

Modelo_arriendo=lm(`Ingreso del hogar`~Arriendo_estimacion,Base_datos)
summary(Modelo_arriendo)

Modelo_grado=lm(`Ingreso del hogar`~`Ultimo grado alcanzado`,Base_datos)
summary(Modelo_grado)

Modelo_afiliado=lm(`Ingreso del hogar`~Afiliado,Base_datos)
summary(Modelo_afiliado)

Modelo_casado=lm(`Ingreso del hogar`~Casado,Base_datos)
summary(Modelo_casado)


#Modelo final

Modelo_final=lm(`Ingreso del hogar`~Estrato+Arriendo_estimacion+`Ultimo grado alcanzado`+Sexo+
                  `Cantidad de personas en el hogar`,Base_datos)
summary(Modelo_final)
plot(Modelo_final,which=1)
plot(Modelo_final,which=2)
plot(Modelo_final,which=3)
plot(Modelo_final,which=4)
#Pruebas modelo


#Graficas