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
########################################################################################################
#rfinal
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

# --- Limpieza y Transformación de Variables ---

Base_datos_final <- Base_datos %>%
  # 1. Aplicar logaritmo natural al ingreso (añadir +1 para evitar log(0) si hay ingresos cero)
  mutate(log_Ingreso_hogar = log(`Ingreso del hogar` + 1)) %>%
  # 2. Convertir variables categóricas importantes a tipo factor
  mutate(
    Estrato = factor(Estrato),
    Sexo = factor(Sexo),
    `Ultimo grado alcanzado` = factor(`Ultimo grado alcanzado`)
  )

# --- Modelo Final Corregido y Transformado ---

Modelo_final_corregido = lm(log_Ingreso_hogar ~ Estrato + sastifacion + `Ultimo grado alcanzado` + Sexo +
                              `Cantidad de personas en el hogar`, Base_datos_final)

summary(Modelo_final_corregido)


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


###diego

#Librerias
library(tidyverse)
library(scales)
library(readxl)
library(ggcorrplot)
library(visreg)
library(nortest)
library(lmtest)

# --- 1. PREPARACIÓN DE LAS BASES DE DATOS ---

datos_hogar=read.csv("datos_hogar.csv", sep = ";") %>% 
  select(DIRECTORIO,I_HOGAR,PERCAPITA,CANT_PERSONAS_HOGAR) %>% 
  rename("Ingreso del hogar"=2,"Cantidad de personas en el hogar"=4) %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)

educacion=read.csv("educacion.csv", sep= ";") %>% 
  select(DIRECTORIO,P8587) %>% 
  group_by(DIRECTORIO) %>%
  filter(P8587 == max(P8587)) %>%
  rename("Ultimo grado alcanzado"=2) %>%
  ungroup() %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)

caracteristicas_hogar=read.csv("Características_composición.CSV",sep= ";") %>% 
  select(DIRECTORIO,P6020,P6040,P6051,P5502,P1895) %>% rename(Sexo=2,Edad=3,Parentesco=4,Casado=5,
                                                              Satisfaccion=6) %>% # ¡NOMBRE CORREGIDO!
  filter(Parentesco==1)#jefes del hogar

tenencia=read.csv("tenencia y financiación de la vivienda.CSV",sep=";") %>%
  filter(P5130!=99) %>% 
  mutate(Arriendo_estimacion=rowSums(select(., P5130, P5140), na.rm = TRUE)) %>% 
  select(DIRECTORIO,Arriendo_estimacion) %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)

trabajo=read.csv("Fuerza de trabajo.CSV",sep=";") %>% 
  select(DIRECTORIO,P8624,P415,P8634,P6426,P416) %>% rename(Ingresos_mes=2,Horas_trabajadas_semana=3,
                                                            "Lugar de trabajo"=4,Tiempo_trabajado=5,
                                                            Semana_horas=6) %>% 
  filter(!is.na(Ingresos_mes)) %>% 
  group_by(DIRECTORIO) %>%
  filter(Ingresos_mes == max(Ingresos_mes)) %>%
  ungroup()

vivienda=read.csv("Datos de la vivienda.csv",sep=";") %>% 
  select(DIRECTORIO,P8520S1A1,CLASE) %>% rename(Estrato=2,Ubicacion=3) %>% 
  filter(Estrato!=0,Estrato!=8,Estrato!=9) %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)

salud=read.csv("Salud.CSV", sep=";") %>%
  select(DIRECTORIO,P6090) %>% 
  group_by(DIRECTORIO) %>%
  filter(P6090 == max(P6090)) %>%
  rename(Afiliado=2)%>%
  ungroup() %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)

Muestra=read.csv("muestral.CSV",sep=";") %>% 
  select(DIRECTORIO,MPIO) %>% rename(Municipio=2) %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)

# --- 2. UNIFICACIÓN DE DATOS ---

Base_datos=inner_join(Muestra,caracteristicas_hogar,by="DIRECTORIO") %>% 
  inner_join(tenencia,by="DIRECTORIO") %>% inner_join(datos_hogar,by="DIRECTORIO") %>% 
  inner_join(vivienda,by="DIRECTORIO") %>% inner_join(educacion,by="DIRECTORIO") %>% 
  inner_join(salud,by="DIRECTORIO") %>% 
  
  # La variable Satisfaccion se incluye aquí correctamente
  select(DIRECTORIO,Municipio,Estrato,`Ingreso del hogar`,
         Arriendo_estimacion,`Cantidad de personas en el hogar`,
         Sexo,Edad,`Ultimo grado alcanzado`,
         Satisfaccion,PERCAPITA) %>% # ¡USO CORREGIDO!
  
  inner_join(trabajo,by="DIRECTORIO")


# --- 3. LIMPIEZA Y TRANSFORMACIÓN LOGARÍTMICA ---

Base_datos_analisis <- Base_datos %>%
  # Creamos la variable dependiente transformada
  mutate(log_Ingreso_hogar = log(`Ingreso del hogar` + 1)) %>%
  # Convertimos categóricas a factor para el modelo
  mutate(
    Estrato = factor(Estrato),
    Sexo = factor(Sexo),
    `Ultimo grado alcanzado` = factor(`Ultimo grado alcanzado`),
    Satisfaccion = factor(Satisfaccion) # ¡USO CORREGIDO!
  )

# --- 4. MODELOS DE REGRESIÓN (Usando el Logaritmo) ---

# Modelo final con la variable dependiente log-transformada
Modelo_final=lm(log_Ingreso_hogar ~ Estrato + Satisfaccion + `Ultimo grado alcanzado` + Sexo +
                  `Cantidad de personas en el hogar`,Base_datos_analisis) # ¡USO CORREGIDO!

summary(Modelo_final)

# Modelos simples
Modelo_sastifacion=lm(log_Ingreso_hogar ~ Satisfaccion,Base_datos_analisis) # ¡USO CORREGIDO!
summary(Modelo_sastifacion)

# Diagnóstico de Residuos
plot(Modelo_final,which=1)
plot(Modelo_final,which=2)
plot(Modelo_final,which=3)
plot(Modelo_final,which=5)


##
#Librerias
library(tidyverse)
library(scales)
library(readxl)
library(ggcorrplot)
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
  select(DIRECTORIO,P6020,P6040,P6051,P5502,P1895,P6080) %>% rename(Sexo=2,Edad=3,Parentesco=4,Casado=5,
                                                              sastifacion=6, etnia=7)

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
  select(DIRECTORIO,CLASE) %>% rename(Ubicacion=2) %>% 
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
  inner_join(tenencia,by="DIRECTORIO") %>% inner_join(datos_hogar,etnia,by="DIRECTORIO") %>% 
  inner_join(vivienda,by="DIRECTORIO") %>% inner_join(educacion,by="DIRECTORIO") %>% 
  inner_join(salud,by="DIRECTORIO")  %>%
  filter(Municipio==76001) %>% inner_join(trabajo,by="DIRECTORIO") %>% 
  select(-Afiliado,-Arriendo_estimacion,-Casado,-Sexo,-PERCAPITA,-Ubicacion,
         -Ingresos_mes,-contrato,-DIRECTORIO,-etnia) %>% 
  mutate(`Ingreso del hogar`=log(`Ingreso del hogar`),Edad=Edad*Edad)


#Modelos

Modelo_sexo=lm(`Ingreso del hogar`~Sexo,Base_datos)
summary(Modelo_sexo)

Modelo_estrato=lm(`Ingreso del hogar`~Estrato,Base_datos)
summary(Modelo_estrato)

Modelo_etnia=lm(`Ingreso del hogar`~etnia,Base_datos)
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

Modelo_final=lm(`Ingreso del hogar`~sastifacion+Tiempo_trabajado+`Ultimo grado alcanzado`+
                  `Cantidad de personas en el hogar`+Edad,Base_datos+etnia)

summary(Modelo_final)
plot(Modelo_final,which=1)
plot(Modelo_final,which=2)
plot(Modelo_final,which=3)
plot(Modelo_final,which=5)


library(tidyverse)
library(scales)
library(readxl)
library(ggcorrplot)
library(lmtest)

# --- Base de datos ---

datos_hogar=read.csv("datos_hogar.csv", sep = ";") %>% 
  select(DIRECTORIO,I_HOGAR,PERCAPITA,CANT_PERSONAS_HOGAR) %>% 
  rename("Ingreso del hogar"=2,"Cantidad de personas en el hogar"=4) %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)

educacion=read.csv("educacion.csv", sep= ";") %>% 
  select(DIRECTORIO,P8587) %>% 
  group_by(DIRECTORIO) %>%
  filter(P8587 == max(P8587)) %>%
  rename("Ultimo grado alcanzado"=2) %>%
  ungroup() %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)

# Se corrige el nombre sastifacion por Satisfaccion
caracteristicas_hogar=read.csv("Características_composición.CSV",sep= ";") %>% 
  select(DIRECTORIO,P6020,P6040,P6051,P5502,P1895,P6080) %>% 
  rename(Sexo=2,Edad=3,Parentesco=4,Casado=5,Satisfaccion=6, Etnia=7) %>%
  filter(Parentesco==1) # Filtramos solo jefes de hogar para variables de persona

tenencia=read.csv("tenencia y financiación de la vivienda.CSV",sep=";") %>%
  filter(P5130!=99) %>% 
  mutate(Arriendo_estimacion=rowSums(select(., P5130, P5140), na.rm = TRUE)) %>% 
  select(DIRECTORIO,Arriendo_estimacion) %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)


# Se corrige el error en el renombre de las columnas (meses y contrato)
trabajo=read.csv("Fuerza de trabajo.CSV",sep=";") %>% 
  select(DIRECTORIO,P8624,P415,P8634,P6426,P416,P6440,P6460,P8636) %>% 
  rename(Ingresos_mes=2,Horas_trabajadas_semana=3,"Lugar de trabajo"=4,Tiempo_trabajado=5,
         Semana_horas=6,Contrato=7,Tipo_contrato=8, Meses=9) %>% # Corregido para evitar duplicidad
  filter(!is.na(Ingresos_mes)) %>%
  group_by(DIRECTORIO) %>%
  filter(Ingresos_mes == max(Ingresos_mes)) %>%
  ungroup()


vivienda=read.csv("Datos de la vivienda.csv",sep=";") %>% 
  select(DIRECTORIO,P8520S1A1,CLASE) %>% rename(Estrato=2,Ubicacion=3) %>% # Añadimos Estrato
  filter(Estrato!=0,Estrato!=8,Estrato!=9) %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)

salud=read.csv("Salud.CSV", sep=";") %>%
  select(DIRECTORIO,P6090) %>% 
  group_by(DIRECTORIO) %>%
  filter(P6090 == max(P6090)) %>%
  rename(Afiliado=2)%>%
  ungroup() %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)

Muestra=read.csv("muestral.CSV",sep=";") %>% 
  select(DIRECTORIO,MPIO) %>% rename(Municipio=2) %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)

# --- Base de datos final ---

Base_datos=inner_join(Muestra,caracteristicas_hogar,by="DIRECTORIO") %>% 
  inner_join(tenencia,by="DIRECTORIO") %>% 
  inner_join(datos_hogar,by="DIRECTORIO") %>% # Corregido el error de join
  inner_join(vivienda,by="DIRECTORIO") %>% inner_join(educacion,by="DIRECTORIO") %>% 
  inner_join(salud,by="DIRECTORIO") %>%
  filter(Municipio==76001) %>% inner_join(trabajo,by="DIRECTORIO") %>% 
  
  # Eliminamos solo las variables que NO usaremos en los modelos
  select(-Ingresos_mes, -Parentesco, -DIRECTORIO, -Lugar.de.trabajo) %>% 
  
  # Transformación y creación de la variable Edad al cuadrado
  mutate(`Ingreso del hogar`=log(`Ingreso del hogar` + 1), # Se añade +1 para evitar log(0)
         `Edad al cuadrado`=Edad*Edad) %>%
  
  # Convertir categóricas a factor
  mutate(Estrato = factor(Estrato),
         Satisfaccion = factor(Satisfaccion),
         Etnia = factor(Etnia),
         Sexo = factor(Sexo),
         Afiliado = factor(Afiliado),
         Ubicacion = factor(Ubicacion),
         Contrato = factor(Contrato))


# --- Modelos (Ahora 'Sexo', 'Afiliado', 'Etnia', 'Arriendo_estimacion' existen) ---

Modelo_sexo=lm(`Ingreso del hogar`~Sexo,Base_datos)
summary(Modelo_sexo)

Modelo_estrato=lm(`Ingreso del hogar`~Estrato,Base_datos)
summary(Modelo_estrato)

Modelo_etnia=lm(`Ingreso del hogar`~Etnia,Base_datos)
summary(Modelo_etnia) # Corregido el nombre de la variable y del modelo

Modelo_tiempo_trabajado=lm(`Ingreso del hogar`~Tiempo_trabajado,Base_datos)
summary(Modelo_tiempo_trabajado)

Modelo_arriendo=lm(`Ingreso del hogar`~Arriendo_estimacion,Base_datos)
summary(Modelo_arriendo)

Modelo_grado=lm(`Ingreso del hogar`~`Ultimo grado alcanzado`,Base_datos)
summary(Modelo_grado)

Modelo_afiliado=lm(`Ingreso del hogar`~Afiliado,Base_datos)
summary(Modelo_afiliado)

Modelo_sastifacion=lm(`Ingreso del hogar`~ Satisfaccion,Base_datos) # Corregido el nombre de la variable
summary(Modelo_sastifacion)



library(tidyverse)
library(scales)
library(readxl)
library(ggcorrplot)
library(lmtest)
# library(car) # Necesaria para VIF (diagnóstico)
# library(ggeffects) # Necesaria para visualización (diagnóstico)

# --- Base de datos ---

datos_hogar=read.csv("datos_hogar.csv", sep = ";") %>% 
  select(DIRECTORIO,I_HOGAR,PERCAPITA,CANT_PERSONAS_HOGAR) %>% 
  rename("Ingreso del hogar"=2,"Cantidad de personas en el hogar"=4) %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)

educacion=read.csv("educacion.csv", sep= ";") %>% 
  select(DIRECTORIO,P8587) %>% 
  group_by(DIRECTORIO) %>%
  filter(P8587 == max(P8587)) %>%
  rename("Ultimo grado alcanzado"=2) %>%
  ungroup() %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)

caracteristicas_hogar=read.csv("Características_composición.CSV",sep= ";") %>% 
  select(DIRECTORIO,P6020,P6040,P6051,P5502,P1895,P6080) %>% 
  rename(Sexo=2,Edad=3,Parentesco=4,Casado=5,Satisfaccion=6, Etnia=7) %>% # Corregido 'sastifacion'
  filter(Parentesco==1) 

tenencia=read.csv("tenencia y financiación de la vivienda.CSV",sep=";") %>%
  filter(P5130!=99) %>% 
  mutate(Arriendo_estimacion=rowSums(select(., P5130, P5140), na.rm = TRUE)) %>% 
  select(DIRECTORIO,Arriendo_estimacion) %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)

# CORRECCIÓN DE MANY-TO-MANY Y RENOMBRE
trabajo=read.csv("Fuerza de trabajo.CSV",sep=";") %>% 
  select(DIRECTORIO,P8624,P415,P8634,P6426,P416,P6440,P6460,P8636) %>% 
  rename(Ingresos_mes=2,Horas_trabajadas_semana=3,"Lugar de trabajo"=4,Tiempo_trabajado=5,
         Semana_horas=6,Contrato=7,Tipo_contrato=8, Meses=9) %>% 
  filter(!is.na(Ingresos_mes)) %>%
  group_by(DIRECTORIO) %>%
  filter(Ingresos_mes == max(Ingresos_mes)) %>%
  slice(1) %>% # SOLUCIÓN PARA EMPATES Y MANY-TO-MANY
  ungroup()

vivienda=read.csv("Datos de la vivienda.csv",sep=";") %>% 
  select(DIRECTORIO,P8520S1A1,CLASE) %>% rename(Estrato=2,Ubicacion=3) %>% 
  filter(Estrato!=0,Estrato!=8,Estrato!=9) %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)

salud=read.csv("Salud.CSV", sep=";") %>%
  select(DIRECTORIO,P6090) %>%
  group_by(DIRECTORIO) %>%
  filter(P6090 == max(P6090)) %>%
  rename(Afiliado=2)%>%
  ungroup() %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)

Muestra=read.csv("muestral.CSV",sep=";") %>% 
  select(DIRECTORIO,MPIO) %>% rename(Municipio=2) %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)

# --- Base de datos final ---

Base_datos=inner_join(Muestra,caracteristicas_hogar,by="DIRECTORIO") %>% 
  inner_join(tenencia,by="DIRECTORIO") %>% 
  inner_join(datos_hogar,by="DIRECTORIO") %>% 
  inner_join(vivienda,by="DIRECTORIO") %>% inner_join(educacion,by="DIRECTORIO") %>% 
  inner_join(salud,by="DIRECTORIO") %>%
  filter(Municipio==76001) %>% inner_join(trabajo,by="DIRECTORIO") %>% 
  
  # CORRECCIÓN: El nombre se refiere con el espacio y comillas
  select(-Ingresos_mes, -Parentesco, -DIRECTORIO, -"Lugar de trabajo") %>% 
  
  # Transformación y creación de la variable Edad al cuadrado
  mutate(`Ingreso del hogar`=log(`Ingreso del hogar` + 1), 
         `Edad al cuadrado`=Edad*Edad) %>%
  
  # Convertir categóricas a factor
  mutate(Estrato = factor(Estrato),
         Satisfaccion = factor(Satisfaccion),
         Etnia = factor(Etnia),
         Sexo = factor(Sexo),
         Afiliado = factor(Afiliado),
         Ubicacion = factor(Ubicacion),
         Contrato = factor(Contrato))

# --- Modelos ---
# Ahora todas las variables existen
Modelo_sexo=lm(`Ingreso del hogar`~Sexo,Base_datos)
summary(Modelo_sexo)

Modelo_estrato=lm(`Ingreso del hogar`~Estrato,Base_datos)
summary(Modelo_estrato)

Modelo_etnia=lm(`Ingreso del hogar`~Etnia,Base_datos)
summary(Modelo_etnia) 

Modelo_tiempo_trabajado=lm(`Ingreso del hogar`~Tiempo_trabajado,Base_datos)
summary(Modelo_tiempo_trabajado)

Modelo_arriendo=lm(`Ingreso del hogar`~Arriendo_estimacion,Base_datos)
summary(Modelo_arriendo)

Modelo_grado=lm(`Ingreso del hogar`~`Ultimo grado alcanzado`,Base_datos)
summary(Modelo_grado)

Modelo_afiliado=lm(`Ingreso del hogar`~Afiliado,Base_datos)
summary(Modelo_afiliado)

Modelo_sastifacion=lm(`Ingreso del hogar`~ Satisfaccion,Base_datos) 
summary(Modelo_sastifacion)


# --- Modelo final ---

Modelo_final=lm(`Ingreso del hogar`~Satisfaccion + Tiempo_trabajado + `Ultimo grado alcanzado` +
                  `Cantidad de personas en el hogar` + Edad + `Edad al cuadrado` + Etnia, 
                Base_datos) 

summary(Modelo_final)
plot(Modelo_final,which=1)
plot(Modelo_final,which=2)
plot(Modelo_final,which=3)
plot(Modelo_final,which=5)


# --- Modelo final ---

Modelo_final=lm(`Ingreso del hogar`~Satisfaccion + Tiempo_trabajado + `Ultimo grado alcanzado` +
                  `Cantidad de personas en el hogar` + Edad + `Edad al cuadrado` + Etnia, # Corregido
                Base_datos) # Corregido el argumento data

summary(Modelo_final)
plot(Modelo_final,which=1)
plot(Modelo_final,which=2)
plot(Modelo_final,which=3)
plot(Modelo_final,which=5)

