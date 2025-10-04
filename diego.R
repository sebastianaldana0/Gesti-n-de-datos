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

