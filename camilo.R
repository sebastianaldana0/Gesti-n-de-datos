
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
library(kableExtra)
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
           
View

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

library(Hmisc)


#  Seleccionar solo variables numéricas
num_vars <- Base_datos %>% 
  select_if(is.numeric)

#  Calcular correlaciones de Pearson
corr_result <- Hmisc::rcorr(as.matrix(num_vars))

#  Extraer correlaciones y p-valores con la variable dependiente
cor_y <- corr_result$r[, "Ingreso del hogar"]
pval_y <- corr_result$P[, "Ingreso del hogar"]
summary(pval_y)
view(pval_y)



tabla_corr <- tibble(
  Variable = names(cor_y),
  `Coef. de Pearson (r)` = round(cor_y, 3),
  `Valor p` = pval_y) %>%
  filter(Variable != "Ingreso del hogar", Variable != "Sexo") %>%
  mutate(    `Valor p` = case_when(
      is.na(`Valor p`) ~ NA_character_,
      `Valor p` < 2e-16 ~ "< 2e-16",
      TRUE ~ formatC(`Valor p`, format = "e", digits = 3)),
    `Decisión (α = 0.05)` = ifelse(
      as.numeric(pval_y[match(Variable, names(pval_y))]) < 0.05,
      "Rechaza H₀",
      "No Rechaza H₀"))

# Mostrar tabla con estilo uniforme
tabla_corr %>%
  kbl(caption = "Coeficiente de Correlación de Pearson con la Variable Dependiente",
    align = c("l", "r", "r", "c"),
    col.names = c("Variable", "Coef. de Pearson (r)", "Valor p", "Decisión")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 14,
    position = "center") %>%
  row_spec(0, background = "#2b6cb0", color = "white", bold = TRUE) %>%
  column_spec(1, bold = TRUE, width = "3cm") %>%
  column_spec(2:4, width = "2.5cm") %>%
  footnote(
    general = "Decisión basada en un nivel de significancia α = 0.05. Se rechaza H₀ cuando p < 0.05, indicando correlación estadísticamente significativa.",
    general_title = "Nota:",
    footnote_as_chunk = TRUE)

cor(Base_datos$`Ingreso del hogar`, Base_datos$Edad)

Modelo_final=lm(log(`Ingreso del hogar`)~Sexo+Tiempo_trabajado+CATEGORIA_EDUCATIVA+
                  `Cantidad de personas en el hogar`+Edad+sastifacion+Horas_trabajadas_semana+
                  Departamento
                ,Base_datos)

summary(Modelo_final)

library(broom)

# Crear resumen del modelo
resumen_mod <- broom::tidy(Modelo_final) %>%
  mutate(across(where(is.numeric), ~ round(., 5))) %>%
  rename(`Variable` = term,
    `Coeficiente` = estimate,
    `Error Estándar` = std.error,
    `Estadístico t` = statistic,
    `Valor p` = p.value) %>%
  mutate(Significancia = case_when(
      `Valor p` < 0.001 ~ "***",
      `Valor p` < 0.01  ~ "**",
      `Valor p` < 0.05  ~ "*",
      TRUE ~ "")) %>% 
  mutate(`Valor p` = case_when(
      is.na(`Valor p`) ~ "1",
      `Valor p` < 2e-16 ~ "2e-16",
      TRUE ~ formatC(`Valor p`, format = "e", digits = 3)),
    Sig. = case_when(
      as.numeric(`Valor p`) < 0.001 ~ "***",
      as.numeric(`Valor p`) < 0.01  ~ "**",
      as.numeric(`Valor p`) < 0.05  ~ "*",
      as.numeric(`Valor p`) < 0.1  ~ ".",
      TRUE ~ ""))

# Extraer medidas globales del modelo

resumen_global <- glance(Modelo_final)
summary(resumen_global)

# Crear tabla principal

resumen_global <- glance(Modelo_final)
R2 <- round(resumen_global$adj.r.squared, 4)
Fstat <- round(resumen_global$statistic, 2)
pvalor_modelo <- formatC(resumen_global$p.value, format = "e", digits = 2)
N <- resumen_global$df.residual + length(Modelo_final$coefficients)

# ---- 3️⃣ Crear filas resumen y añadirlas debajo ----

filas_resumen <- tibble(
  Variable = c("R² ajustado", "F", "Valor p (modelo)", "N"),
  Coeficiente = as.character(c(R2, Fstat, pvalor_modelo, N)),
  `Error Estándar` = as.character(c("", "", "", "")),
  `Estadístico t` = as.character(c("", "", "", "")),
  `Valor p` = as.character(c("", "", "", "")),
  Sig. = as.character(c("", "", "", "")))

resumen_mod_chr= resumen_mod %>%
  mutate_all(as.character)

tabla_final <- bind_rows(resumen_mod_chr, filas_resumen)

tabla_final <- tabla_final %>% 
  select(Variable, Coeficiente, `Error Estándar`, `Estadístico t`, `Valor p`, Sig.)

kbl(tabla_final,
    caption = "Resultados del modelo de regresión lineal múltiple",
    align = c("l", "r", "r", "r", "r", "c"),
    col.names = names(tabla_final)) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 14,
    position = "center") %>%
  row_spec(0, background = "#2b6cb0", color = "white", bold = TRUE) %>%
  column_spec(1, bold = TRUE, width = "4cm") %>%
  column_spec(2:6, width = "2.5cm") %>%
  row_spec((nrow(resumen_mod_chr) + 1):(nrow(resumen_mod_chr) + nrow(filas_resumen)),
           bold = TRUE, italic = FALSE, background = "#e6f0ff") %>%
  footnote(
    general = "Elaboración propia con base en ECV DANE 2024.",
    general_title = "Nota:",
    footnote_as_chunk = TRUE)

tema_powerbi <- theme_minimal() +
  theme(text = element_text(family = "Segoe UI", color = "#2d3748"),
  plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "#323130"),
  plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#605e5c", margin = margin(b = 15)),
  plot.caption = element_text(size = 10, color = "#605e5c", hjust = 0),
  panel.grid.major = element_line(color = "#f3f2f1"),
  panel.grid.minor = element_blank(),
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA),
  axis.title = element_text(face = "bold", color = "#323130"),
  axis.text = element_text(color = "#605e5c"),
  legend.position = "none")



#### Validacion de los supuestos 
## Analisis grafico 
# media cero de los residuales 

library(plotly)
res_df <- augment(Modelo_final)

G_residuos_hist <- ggplot(res_df, aes(x = .resid)) +
  geom_histogram(bins = 30, fill = "#0078D4", color = "white", alpha = 0.7) +
  geom_vline(xintercept = 0, color = "#E81123", linetype = "dashed", linewidth = 1) +
  labs(
    title = "📊 Figura 1. Distribución de los residuos del modelo",
    subtitle = "Validación del supuesto de media cero de los errores",
    x = "Residuos", y = "Frecuencia",
    caption = "Fuente: Elaboración propia con base en ECV DANE 2024 | Análisis: Equipo de Investigación"
  ) +
  tema_powerbi
G_residuos_hist

Grafico1=ggplot(res_df, aes(.fitted, .resid)) +
  geom_point(color = "#0078D4", alpha = 0.5, size = 1.5) +
  geom_smooth(method = "loess", se = FALSE, color = "#FFB900", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#E81123") +
  labs(
    title = "Residuos vs Valores Ajustados",
    subtitle = "Evaluación de la linealidad y homocedasticidad",
    x = "Valores ajustados", y = "Residuos",
    caption = "Fuente: Elaboración propia con base en ECV DANE 2024 | Análisis: Equipo de Investigación"
  ) +
  tema_powerbi

G1_interactivo <- ggplotly(Grafico1, tooltip = c("x", "y")) %>%
  layout(font = list(family = "Segoe UI", size = 12, color = "#323130"),
    title = list(x = 0.05, y = 0.95),
    plot_bgcolor = "#ffffff",
    paper_bgcolor = "#ffffff",
    xaxis = list(title = "<b>Valores ajustados</b>", gridcolor = "#f3f2f1"),
    yaxis = list(title = "<b>Residuos</b>", gridcolor = "#f3f2f1"),
    margin = list(l = 70, r = 40, t = 90, b = 60),
    annotations = list(
      list(x = 0.02, y = -0.15,
        text = "Fuente: Encuesta ECV DANE 2024 | Análisis: Equipo de Investigación",
        showarrow = FALSE,
        xref = "paper", yref = "paper",
        xanchor = "left", yanchor = "bottom",
        font = list(size = 10, color = "#605e5c")))) %>%
  config(displaylogo = FALSE, displayModeBar = TRUE)

G1_interactivo

plot(Modelo_final, 2)

#Los residuales se distribuyen de forma normal 

  # Crear manualmente los valores para el gráfico Q-Q

qq_data <- qqnorm(res_df$.std.resid, plot.it = FALSE)
qq_df <- data.frame(
  teóricos = qq_data$x,
  muestrales = qq_data$y)

Grafico2= ggplot(qq_df, aes(x = teóricos, y = muestrales)) +
  geom_point(color = "#0078D4", alpha = 0.6, size = 1) +
  geom_abline(slope = 1, intercept = 0, color = "#E81123", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Gráfico Q-Q de los residuos estandarizados",
    subtitle = "Evaluación del supuesto de normalidad",
    x = "Cuantiles teóricos", y = "Cuantiles muestrales",
    caption = "Fuente: Elaboración propia con base en ECV DANE 2024 | Análisis: Equipo de Investigación") +
  tema_powerbi

G2_interactivo <- ggplotly(Grafico2, tooltip = c("x", "y")) %>%
  layout(
    font = list(family = "Segoe UI", size = 12, color = "#323130"),
    title = list(x = 0.05, y = 0.95),
    plot_bgcolor = "#ffffff",
    paper_bgcolor = "#ffffff",
    xaxis = list(title = "<b>Cuantiles teóricos</b>", gridcolor = "#f3f2f1"),
    yaxis = list(title = "<b>Cuantiles muestrales</b>", gridcolor = "#f3f2f1"),
    margin = list(l = 70, r = 40, t = 90, b = 60),
    annotations = list(
      list(
        x = 0.02, y = -0.15,
        text = "Fuente: Encuesta ECV DANE 2024 | Análisis: Equipo de Investigación",
        showarrow = FALSE,
        xref = "paper", yref = "paper",
        xanchor = "left", yanchor = "bottom",
        font = list(size = 10, color = "#605e5c")))) %>%
  config(displaylogo = FALSE, displayModeBar = TRUE)

G2_interactivo

#Varianza constante 

G3 <- ggplot(res_df, aes(x = .fitted, y = sqrt(abs(.std.resid)))) +
  geom_point(color = "#0078D4", alpha = 0.6, size = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "#FFB900", linewidth = 1) +
  labs(
    title = "Gráfico Scale-Location",
    subtitle = "Evaluación de la homocedasticidad de los residuos",
    x = "Valores ajustados",
    y = expression(sqrt("|Residuos estandarizados|")),
    caption = "Fuente: Elaboración propia con base en ECV DANE 2024 | Análisis: Equipo de Investigación") +
  tema_powerbi

G3_interactivo <- ggplotly(G3, tooltip = c("x", "y")) %>%
  layout(
    font = list(family = "Segoe UI", size = 12, color = "#323130"),
    title = list(x = 0.05, y = 0.95),
    plot_bgcolor = "#ffffff",
    paper_bgcolor = "#ffffff",
    xaxis = list(title = "<b>Valores ajustados</b>", gridcolor = "#f3f2f1"),
    yaxis = list(title = "<b>√|Residuos estandarizados|</b>", gridcolor = "#f3f2f1"),
    margin = list(l = 70, r = 40, t = 90, b = 60),
    annotations = list(
      list(
        x = 0.02, y = -0.15,
        text = "Fuente: Encuesta ECV DANE 2024 | Análisis: Equipo de Investigación",
        showarrow = FALSE,
        xref = "paper", yref = "paper",
        xanchor = "left", yanchor = "bottom",
        font = list(size = 10, color = "#605e5c")))) %>%
  config(displaylogo = FALSE, displayModeBar = TRUE)

G3_interactivo

plot(Modelo_final,3)

# los errores son aleatorios entre si 

 # Calcular Cook's distance y agregarlo a res_df
res_df$CooksD <- cooks.distance(Modelo_final)

Grafico4=ggplot(res_df, aes(x = .hat, y = .std.resid, text = paste(
  "Leverage:", round(.hat, 3), "<br>",
  "Residuos estandarizados:", round(.std.resid, 3), "<br>",
  "Cook’s Distance:", round(CooksD, 4)))) +
  geom_point(aes(size = CooksD, 
                 color = ifelse(CooksD > 4 / nrow(res_df), "#E81123", "#0078D4")),
             alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "#FFB900", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#E81123") +
  labs(
    title = "Figura 4. Residuos estandarizados vs Leverage",
    subtitle = "Identificación de observaciones influyentes (Cook’s distance)",
    x = "Leverage", y = "Residuos estandarizados",
    caption = "Fuente: Elaboración propia con base en ECV DANE 2024 | Análisis: Equipo de Investigación") +
  tema_powerbi +
  guides(size = "none", color = "none")

G4_interactivo <- ggplotly(Grafico4, tooltip = "text") %>%
  layout(
    font = list(family = "Segoe UI", size = 12, color = "#323130"),
    title = list(x = 0.05, y = 0.95),
    plot_bgcolor = "#ffffff",
    paper_bgcolor = "#ffffff",
    xaxis = list(title = "<b>Leverage</b>", gridcolor = "#f3f2f1"),
    yaxis = list(title = "<b>Residuos estandarizados</b>", gridcolor = "#f3f2f1"),
    margin = list(l = 70, r = 40, t = 90, b = 60),
    annotations = list(
      list(
        x = 0.02, y = -0.15,
        text = "Fuente: Encuesta ECV DANE 2024 | Análisis: Equipo de Investigación",
        showarrow = FALSE,
        xref = "paper", yref = "paper",
        xanchor = "left", yanchor = "bottom",
        font = list(size = 10, color = "#605e5c")))) %>%
  config(displaylogo = FALSE, displayModeBar = TRUE)

G4_interactivo
plot(Modelo_final,4) 
