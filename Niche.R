install.packages("readr")

#Página en uso: "https://microdatos.dane.gov.co/index.php/catalog/865/data-dictionary/F4?file_name=EAC%20NACIONAL%202023"
library(readr)
library(tidyverse)
eac <- read.csv("C:/Users/Alejandro/Documents/ProbabilidadYEstadistica/Pruebas/Estructura EAC - 2004/Estructura EAC - 2004.csv", 
                sep = ";", header = TRUE, stringsAsFactors = FALSE)
eac=eac %>% select(VENTA, TOTPERSO, GASTOS, SUELDOS, INVINI, AGREGA, BRUTA ) %>%
  mutate(TOTPERSO=as.numeric(TOTPERSO), GASTOS=as.numeric(GASTOS),
         SUELDOS=as.numeric(SUELDOS), INVINI=as.numeric(INVINI),
         AGREGA=as.numeric(AGREGA), BRUTA=as.numeric(BRUTA)) %>% filter(TOTPERSO>250)

ggplot(eac)+geom_point(aes(x=GASTOS, y=VENTA))

ggplot(eac)+geom_point(aes(x=TOTPERSO, y=VENTA))

modelo=lm(VENTA~GASTOS+TOTPERSO+SUELDOS, eac)
summary(modelo)

View(eac)
plot(modelo)

eak <- read.csv("C:/Users/Alejandro/Documents/ProbabilidadYEstadistica/Pruebas/BasesDatos-EAC-Nacional-2023/EAC_CIFRAS_2023_ANONIMIZADA_FINAL.csv", 
                sep = ";", header = TRUE, stringsAsFactors = FALSE)
eak=eak %>% select(VENTA, CONSUI, AGREGA, TOTREM, CTO, GASTOS, TOTMUJ, TOTHOM, IDOJ1) %>% 
  mutate(VENTA=as.numeric(VENTA), CONSUI=as.numeric(CONSUI),
         AGREGA=as.numeric(AGREGA), TOTPERSO=as.numeric(TOTPERSO),
         TOTREM=as.numeric(TOTREM), CTO=as.numeric(CTO), GASTOS=as.numeric(GASTOS)) %>% filter(IDOJ1=)



modelo2=lm(VENTA~CONSUI+AGREGA+TOTREM+CTO+GASTOS, eak)
summary(modelo2)
ggplot(eak)+geom_point(aes(x=IDOJ1, y=VENTA))
plot(modelo2)