#diego #diego   
#analisis de variables
#primero pues la y= ingreso de la persona (P8624)
#suposicion de primeras x
#X=sexo(P6020), niveleducativo(P8587), edad(P6040), horas_semana(P415)
#estrato()
library(readr)
library(tidyverse)
library(dplyr)
ECV <- read_delim("Educacion.CSV", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(ECV)

#filtrare para ver la importancia de las variables
ECV=ECV %>% select(P8587)
view(ECV)
##SE VE QUE SOLO HAY UNA VARIABLE IMPORTANTE PARA EXPLICAR LA CALIDAD DE 
#VIDA EN LA BASE DE DATOS DE EDUCACION P8587 nivel alncanzado edu
#AGREGA BASE DE DATOS DBF-ENCV-Caracteristicas_composicion_hogar-2024
library(readr)
Características_y_composición_del_hogar <- read_delim("Características y composición del hogar.CSV", 
                                                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(Características_y_composición_del_hogar)

##SEXO AL NACER DE CADA PERSONA EN EL HOGAR Sexo al nacer: (P6020)
##EDAD ¿Cuántos años cumplidos tiene ...? (P6040)
##INTERESANTE no la escogo CASADO O NO 6. Actualmente…: (P5502)
##
library(readr)
Fuerza_de_trabajo <- read_delim("Fuerza de trabajo.CSV", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(Fuerza_de_trabajo)
##Antes de descuentos , ¿cuánto ganó el mes pasado en este empleo? (incluya propinas y comisiones 
#y excluya viáticos y pagos en especie) (P8624)
##¿cuántas horas a la semana trabaja normalmente ____ en ese trabajo ? (P415)


####TERMINADO###UNIFICACION DE VARIABLES####
ECV <- read_delim("Educacion.CSV", 
                  delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(ECV)

ECV=ECV %>% select(P8587)

##P6090 esta afiliado a eps
##¿Cuánto paga o cuánto le descuentan mensualmente a_____ para estar cubierto/a por una entidad de seguridad social en salud? (P8551)

salud=read.csv("Salud.CSV", sep=";")  %>%
  select(DIRECTORIO,P6090, P8551)
#####################################################################################################################

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
  select(DIRECTORIO,I_HOGAR) %>% rename("Ingreso del hogar"=2) %>% 
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
  select(DIRECTORIO,P8520S1A1) %>% rename(Estrato=2) %>% 
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
  inner_join(salud,by="DIRECTORIO")

library(ggplot2)
library(scales)

ggplot(Base_datos, aes(x = as.factor(Estrato), y = `Ingreso del hogar`)) +
  geom_boxplot(fill = "#2C7BB6", alpha = 0.6, outlier.color = "red") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Distribución del ingreso del hogar por estrato socioeconómico",
    x = "Estrato socioeconómico",
    y = "Ingreso mensual del hogar (COP)"
  ) +
  theme_minimal(base_size = 14)

