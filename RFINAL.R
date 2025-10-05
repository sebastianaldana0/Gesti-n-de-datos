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
  distinct(DIRECTORIO, .keep_all = TRUE) %>% filter(`Ingreso del hogar`!=0)


educacion=read.csv("educacion.csv", sep= ";") %>% 
  select(DIRECTORIO,P8587) %>% 
  group_by(DIRECTORIO) %>%
  filter(P8587 == max(P8587)) %>%
  rename("Ultimo grado alcanzado"=2)  %>%
  ungroup() %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)

caracteristicas_hogar=read.csv("Características_composición.CSV",sep= ";") %>% 
  select(DIRECTORIO,P6020,P6040,P6051,P5502,P1895) %>% rename(Sexo=2,Edad=3,Parentesco=4,Casado=5,
                                                              sastifacion=6)

tenencia=read.csv("tenencia y financiación de la vivienda.CSV",sep=";") %>%
  filter(P5130!=99) %>% 
  mutate(Arriendo_estimacion=rowSums(select(., P5130, P5140), na.rm = TRUE)) %>% 
  select(DIRECTORIO,Arriendo_estimacion) %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)
 

trabajo=read.csv("Fuerza de trabajo.CSV",sep=";") %>% 
  select(DIRECTORIO,P8624,P415,P8634,P6426,P416,P6440,P6460,P6426,P8636) %>% 
  rename(Ingresos_mes=2,Horas_trabajadas_semana=3,"Lugar de trabajo"=4,Tiempo_trabajado=5,
         Semana_horas=6,contrato=7,Tipo_contrato=8,
         meses=8) %>% 
  filter(!is.na(Ingresos_mes))
         

vivienda=read.csv("Datos de la vivienda.csv",sep=";") %>% 
  select(DIRECTORIO,CLASE,P1_DEPARTAMENTO) %>% rename(Ubicacion=2,Departamento=3) %>% 
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
  inner_join(salud,by="DIRECTORIO")  %>% inner_join(trabajo,by="DIRECTORIO") %>% 
  select(-Afiliado,-Arriendo_estimacion,-Casado,-PERCAPITA,-Ubicacion,
         -Ingresos_mes,-contrato,-DIRECTORIO) %>% 
  mutate(`Ingreso del hogar`=log(`Ingreso del hogar`),Edad=Edad*Edad) %>% 
  filter(Departamento%in%c(76,19,52,27))
  

#Modelos

Modelo_sexo=lm(`Ingreso del hogar`~Sexo,Base_datos)
summary(Modelo_sexo)

Modelo_estrato=lm(`Ingreso del hogar`~Estrato,Base_datos)
summary(Modelo_estrato)

Modelo_tiempo_trabajado=lm(`Ingreso del hogar`~Tiempo_trabajado,Base_datos)
summary(Modelo_tiempo_trabajado)

Modelo_arriendo=lm(`Ingreso del hogar`~Arriendo_estimacion,Base_datos)
summary(Modelo_arriendo)

Modelo_grado=lm(`Ingreso del hogar`~`Ultimo grado alcanzado`,Base_datos)
summary(Modelo_grado)

Modelo_afiliado=lm(`Ingreso del hogar`~Afiliado,Base_datos)
summary(Modelo_afiliado)

Modelo_sastifacion=lm(`Ingreso del hogar`~ sastifacion,Base_datos)
summary(Modelo_sastifacion)


#Modelo final

Modelo_final=lm(`Ingreso del hogar`~Sexo+Tiempo_trabajado+`Ultimo grado alcanzado`+
                  `Cantidad de personas en el hogar`+Edad,Base_datos)

summary(Modelo_final)
plot(Modelo_final,which=1)
plot(Modelo_final,which=2)
plot(Modelo_final,which=3)
plot(Modelo_final,which=5)
#Pruebas modelo
shapiro.test(Modelo_final$residuals)

#Graficas