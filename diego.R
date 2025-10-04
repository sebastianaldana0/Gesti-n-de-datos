#diego #diego   
#analisis de variables
#primero pues la y= ingreso de la persona ()
#suposicion de primeras x
#X=sexo(), niveleducativo(P8587), edad(), horas_semana()
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
