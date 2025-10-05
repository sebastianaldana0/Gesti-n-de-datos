#Librerias
library(tidyverse)
library(scales)
library(readxl)
library(ggcorrplot)
library(visreg)
library(nortest)
library(lmtest)
library(ggthemes)

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
  mutate(`Ingreso del hogar`=log(`Ingreso del hogar`),Edad2=Edad*Edad) %>% 
  filter(Departamento%in%c(76,19,52,27),Edad!=0,Edad>=18)
  

#Modelos



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

ggplot(Base_datos, aes(x = Edad, y = `Ingreso del hogar`)) +
  geom_point(alpha = 0.3, color = "#2E86AB", size = 1.5) +
  geom_smooth(method = "loess", span = 0.7, color = "#A23B72", 
              linewidth = 1.5, fill = "#F18F01", alpha = 0.2) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    title = "Relación entre Edad e Ingresos del Hogar",
    subtitle = "Patrón típico del ciclo de vida económico",
    x = "Edad",
    y = "Ingreso Anual del Hogar (USD)",
    caption = "Fuente: Datos simulados basados en patrones económicos típicos"
  ) +
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  scale_color_manual(values = c("#2E86AB", "#A23B72")) +
  geom_vline(xintercept = 50, linetype = "dashed", color = "gray50", alpha = 0.7) +
  annotate("text", x = 52, y = max(Base_datos$`Ingreso del hogar`) * 0.9, 
           label = "Pico de ingresos\ntípico ~50 años", 
           color = "gray40", size = 3, hjust = 0)

datos_agrupados <- Base_datos %>%
  group_by(grupo_edad = cut(Edad, breaks = seq(20, 65, by = 5))) %>%
  summarise(
    edad_media = mean(Edad),
    ingreso_mediano = median(`Ultimo grado alcanzado`),
    ingreso_promedio = mean(`Ultimo grado alcanzado`),
    q10 = quantile(`Ultimo grado alcanzado`, 0.10),
    q25 = quantile(`Ultimo grado alcanzado`, 0.25),
    q75 = quantile(`Ultimo grado alcanzado`, 0.75),
    q90 = quantile(`Ultimo grado alcanzado`, 0.90),
    .groups = 'drop'
  )

colores <- c(
  mediana = "#2C3E50",     
  iqr = "#3498DB",          
  deciles = "#BDC3C7"     
)

ggplot(datos_agrupados, aes(x = edad_media)) +
  geom_ribbon(aes(ymin = q10, ymax = q90), 
              fill = colores["deciles"], alpha = 0.15) +
  geom_ribbon(aes(ymin = q25, ymax = q75), 
              fill = colores["iqr"], alpha = 0.3) +
  geom_line(aes(y = ingreso_mediano), 
            color = colores["mediana"], 
            linewidth = 2.5,
            alpha = 0.9) +
  geom_point(aes(y = ingreso_mediano), 
             color = colores["mediana"], 
             size = 4.5,
             fill = "white",
             stroke = 1.2,
             shape = 21) +
  theme_economist() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, 
                              margin = margin(b = 15)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray50",
                                 margin = margin(b = 20)),
    plot.caption = element_text(size = 9, color = "gray60", hjust = 1,
                                margin = margin(t = 15)),
    axis.title = element_text(face = "bold", size = 12),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.text = element_text(size = 10, color = "gray40"),
    panel.grid.major = element_line(color = "gray92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    plot.margin = margin(25, 25, 25, 25)
  ) +
  labs(
    title = "Evolución de los Ingresos del Hogar por Edad",
    subtitle = "Mediana y distribución percentilar del ingreso anual por grupo de edad",
    x = "Edad",
    y = "Ingreso Anual del Hogar (USD)",
    caption = "Fuente: Análisis basado en patrones económicos del ciclo de vida\nEl área sombreada representa los percentiles 25-75 (intercuartílico)"
  )+
  scale_x_continuous(
    breaks = seq(25, 60, by = 5),
    limits = c(22, 62),
    expand = expansion(mult = c(0, 0))
  )


