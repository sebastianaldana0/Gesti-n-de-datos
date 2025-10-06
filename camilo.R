
#Librerias
library(tidyverse)
library(scales)
library(readxl)
library(ggcorrplot)
library(visreg)
library(nortest)
library(lmtest)
library(ggthemes)
library(ggpmisc)

#Base de datos

datos_hogar=read.csv("datos_hogar.csv", sep = ";") %>% 
  select(DIRECTORIO,I_HOGAR,PERCAPITA,CANT_PERSONAS_HOGAR) %>% 
  rename("Ingreso del hogar"=2,"Cantidad de personas en el hogar"=4) %>% 
  distinct(DIRECTORIO, .keep_all = TRUE) %>% filter(`Ingreso del hogar`!=0)


educacion=read.csv("educacion.csv", sep= ";") %>% 
  select(DIRECTORIO,P8587) %>% 
  group_by(DIRECTORIO) %>%
  filter(P8587 == max(P8587))  %>%
  ungroup() %>% 
  distinct(DIRECTORIO, .keep_all = TRUE) %>% 
  mutate(
    Estudios = case_when(
      P8587 == 1 ~ "Ninguno",
      P8587 == 2 ~ "Preescolar",
      P8587 == 3 ~ "Primaria (1º-5º)",
      P8587 == 4 ~ "Secundaria (6º-9º)",
      P8587 == 5 ~ "Media (10º-13º)",
      P8587 == 6 ~ "Técnico sin título",
      P8587 == 7 ~ "Técnico con título",
      P8587 == 8 ~ "Tecnológico sin título",
      P8587 == 9 ~ "Tecnológico con título",
      P8587 == 10 ~ "Universitario sin título",
      P8587 == 11 ~ "Universitario con título",
      P8587 == 12 ~ "Postgrado sin título",
      P8587 == 13 ~ "Postgrado con título",
      TRUE ~ "No especificado"
    ),
    CATEGORIA_EDUCATIVA = case_when(
      P8587 %in% 1:2 ~ "Primaria",
      P8587 %in% 3:5 ~ "Secundaria",
      P8587 %in% 6:9 ~ "Técnica/Tecnológica",
      P8587 %in% 10:11 ~ "Universitaria",
      P8587 %in% 12:13 ~ "Postgrado",
      TRUE ~ "Otro"
    ),    CATEGORIA_EDUCATIVA = factor(CATEGORIA_EDUCATIVA,
                                       levels = c("Primaria", "Secundaria", "Técnica/Tecnológica", 
                                                  "Universitaria", "Postgrado")))

caracteristicas_hogar=read.csv("Características_composición.CSV",sep= ";") %>% 
  filter(P6051==1) %>% 
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
         meses=8) 


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

Base_datos=caracteristicas_hogar  %>% inner_join(datos_hogar,by="DIRECTORIO") %>% 
  inner_join(vivienda,by="DIRECTORIO") %>% inner_join(educacion,by="DIRECTORIO") %>% inner_join(trabajo,by="DIRECTORIO")%>% 
  mutate(`Ingreso del hogar`=log(`Ingreso del hogar`),Edad2=Edad*Edad) %>% 
  filter(Departamento%in%c(76,19,52,27),Edad!=0,Edad>=18) %>% 
  select(Departamento,`Ingreso del hogar`,`Cantidad de personas en el hogar`,Sexo,
         Edad,CATEGORIA_EDUCATIVA,Tiempo_trabajado,Horas_trabajadas_semana,
         sastifacion) %>% mutate(
           Departamento= case_when(
             Departamento == 76 ~ "Valle del Cauca",
             Departamento == 19 ~ "Cauca",
             Departamento == 52 ~ "Nariño",
             Departamento == 27 ~ "Choco"))
           

#Modelo final

Modelo_final=lm(`Ingreso del hogar`~Sexo+Tiempo_trabajado+CATEGORIA_EDUCATIVA+
                  `Cantidad de personas en el hogar`+Edad+sastifacion+Horas_trabajadas_semana+
                  Departamento
                ,Base_datos)

summary(Modelo_final)

plot(Modelo_final,which=1)
plot(Modelo_final,which=2)
plot(Modelo_final,which=3)
plot(Modelo_final,which=5)
