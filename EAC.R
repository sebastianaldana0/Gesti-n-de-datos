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
    TOTHOM=as.numeric(TOTHOM),
    GASTOS=as.numeric(GASTOS)
  )


#Modelo

Modelo_final_EAC=lm(VENTA~Publicidad+Mantenimiento+APRENDIZ+
                      Produccion+TOTMUJ+TOTHOM+GASTOS,Base_de_datos_EAC,na.action = na.omit)
summary(Modelo_final_EAC)
x=Modelo_final_EAC

plot(x,which=1)
plot(x,which=2)
plot(x,which=3)
plot(x,which=5)

#Graficos

ggplot(Base_de_datos_EAC, aes(x = Produccion, y = VENTA)) +
  geom_point(alpha = 0.6, size = 3, color = "#1F77B4", shape = 21, fill = "#1F77B4") +
  geom_smooth(method = "lm", formula = y ~ log(x), 
              color = "#FF7F0E", fill = "#FF7F0E", alpha = 0.2, size = 1.3) +
  labs(
    title = "Relación Logarítmica: Ventas vs Producción",
    subtitle = "Tendencia: Ventas = a + b·log(Producción)",
    x = "Producción",
    y = "Ventas",
    caption = "Línea naranja muestra modelo logarítmico"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title = element_text(face = "bold")
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma)
