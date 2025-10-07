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
  select(DIRECTORIO,CLASE,P1_DEPARTAMENTO,P8520S1A1) %>% rename(Ubicacion=2,Departamento=3,Estrato=4) %>% 
  distinct(DIRECTORIO, .keep_all = TRUE)

%>% 
  filter(Estrato%in%c(1,2,3))

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
  filter(Departamento%in%c(76,19,52,27),Edad!=0,Edad>=18) %>% 
  select(Departamento,`Ingreso del hogar`,`Cantidad de personas en el hogar`,Sexo,
         Edad,CATEGORIA_EDUCATIVA,Tiempo_trabajado,Horas_trabajadas_semana,
         sastifacion) %>% mutate(
           Departamento= case_when(
             Departamento == 76 ~ "Valle del Cauca",
             Departamento == 19 ~ "Cauca",
             Departamento == 52 ~ "Nariño",
             Departamento == 27 ~ "Choco")) %>% 
  rename(Estudios=6,"Tiempo total trabajado"=7,"Horas trabajadas la semana pasada"=8)
  

#Modelos



#Modelo final

Modelo_final=lm(log(`Ingreso del hogar`)~Sexo+`Tiempo total trabajado`+Estudios+
                  `Cantidad de personas en el hogar`+Edad+sastifacion+`Horas trabajadas la semana pasada`+
                  Departamento
                ,Base_datos)

summary(Modelo_final)

plot(Modelo_final,which=1)
plot(Modelo_final,which=2)
plot(Modelo_final,which=3)
plot(Modelo_final,which=5)

Modelo_pruebas=lm(log(`Ingreso del hogar`)~Estudios+`Cantidad de personas en el hogar`+
                    `Horas trabajadas la semana pasada`+Edad+Sexo
                ,Base_datos)

summary(Modelo_pruebas)

plot(Modelo_pruebas,which=1)
plot(Modelo_pruebas,which=2)
plot(Modelo_pruebas,which=3)
plot(Modelo_pruebas,which=5)

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

datos_procesados <- Base_datos %>%
  mutate(
    grupo_edad = cut(Edad,
                     breaks = c(18, 26, 36, 46, 56, 66, Inf),
                     labels = c("18-25", "26-35", "36-45", "46-55", "56-65", "66+"),
                     right = FALSE,
                     include.lowest = TRUE)
  ) %>%
  filter(!is.na(grupo_edad) & !is.na(`Ingreso del hogar`))

# Calcular estadísticas por grupo de edad
estadisticas_edad <- datos_procesados %>%
  group_by(grupo_edad) %>%
  summarise(
    n = n(),
    ingreso_promedio = mean(`Ingreso del hogar`, na.rm = TRUE),
    ingreso_mediano = median(`Ingreso del hogar`, na.rm = TRUE),
    desviacion_estandar = sd(`Ingreso del hogar`, na.rm = TRUE),
    error_estandar = desviacion_estandar / sqrt(n),
    .groups = 'drop'
  ) %>%
  mutate(
    limite_inferior = ingreso_promedio - 1.96 * error_estandar,
    limite_superior = ingreso_promedio + 1.96 * error_estandar,
    crecimiento_promedio = (ingreso_promedio / lag(ingreso_promedio) - 1) * 100,
    crecimiento_mediano = (ingreso_mediano / lag(ingreso_mediano) - 1) * 100
  )


# GRÁFICA PRINCIPAL - Evolución del ingreso por grupos de edad
ggplot(estadisticas_edad, aes(x = grupo_edad, y = ingreso_promedio, group = 1)) +
  geom_line(color = "#2C3E50", linewidth = 1.5, alpha = 0.8) +
  geom_point(aes(color = ingreso_promedio), size = 6, alpha = 0.9) +
  geom_errorbar(aes(ymin = limite_inferior, ymax = limite_superior),
                width = 0.2, color = "#34495E", alpha = 0.7, linewidth = 0.8) +
  geom_text(aes(label = dollar(ingreso_promedio, prefix = "$", big.mark = ",",
                               accuracy = 1)),
            vjust = -1.5, size = 3.2, fontface = "bold", color = "#2C3E50") +
  geom_text(aes(label = ifelse(!is.na(crecimiento_promedio),
                               paste0("+", round(crecimiento_promedio, 1), "%"),
                               "Inicio")),
            vjust = -3.2, size = 2.8, color = "#27AE60", fontface = "bold") +
  geom_text(aes(y = limite_inferior * 0.95,
                label = paste0("n=", n)),
            vjust = 1.5, size = 2.5, color = "#7F8C8D") +
  scale_color_gradient(low = "#3498DB", high = "#E74C3C", name = "Ingreso") +
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ","),
                     expand = expansion(mult = c(0.1, 0.15))) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5,
                              margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40",
                                 margin = margin(b = 20)),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 1,
                                margin = margin(t = 10)),
    axis.title = element_text(face = "bold", size = 12),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text = element_text(size = 10, color = "gray30"),
    axis.text.x = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    title = "EVOLUCIÓN DEL INGRESO DEL HOGAR POR GRUPOS DE EDAD",
    subtitle = "Línea muestra la tendencia de crecimiento | Barras representan intervalo de confianza del 95%",
    x = "Grupos de Edad",
    y = "Ingreso Anual Promedio del Hogar (USD)",
    caption = "El porcentaje verde indica crecimiento respecto al grupo etario anterior | n = tamaño de muestra"
  )

#Cantidad de personas

ggplot(Base_datos, aes(x = `Cantidad de personas en el hogar`, y = `Ingreso del hogar`)) +
  geom_point(alpha = 0.6, 
             color = "#3498DB", 
             size = 2.5,
             position = position_jitter(width = 0.1, height = 0)) +
  geom_smooth(method = "lm", 
              color = "#E74C3C", 
              linewidth = 1.5,
              fill = "#F1948A",
              alpha = 0.2) +
  stat_poly_line(color = "#E74C3C", linewidth = 1.5) +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), 
                                 after_stat(rr.label), 
                                 sep = "~~~")),
               label.x = 0.05, label.y = 0.95,
               size = 4.5,
               color = "#2C3E50") +
  stat_summary(fun = mean, geom = "point", 
               shape = 18, size = 4, color = "#27AE60") +
  stat_summary(fun = mean, geom = "line", 
               linewidth = 1, color = "#27AE60", linetype = "dashed",
               alpha = 0.7) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5,
                              margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40",
                                 margin = margin(b = 15)),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    title = "RELACIÓN ENTRE TAMAÑO DEL HOGAR E INGRESOS",
    subtitle = "Análisis para modelo de regresión lineal",
    x = "Cantidad de Personas en el Hogar",
    y = "Ingreso Anual del Hogar (USD)",
    caption = "● Puntos individuales | ■ Media por grupo | ── Tendencia lineal"
  )

library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(devtools)

colombia <- ne_states(country = "colombia", returnclass = "sf")

1# Filtrar región Pacífica
pacifica <- colombia %>% filter(name %in% c("Chocó", "Valle del Cauca", "Cauca", "Nariño"))

# Crear el mapa
ggplot() + 
  geom_sf(data = colombia, fill = "lightblue", color = "white") + 
  geom_sf(data = pacifica, fill = "#2b6cb0", color = "white") + 
  theme_void() + 
  labs(title = "Región Pacífica de Colombia")
