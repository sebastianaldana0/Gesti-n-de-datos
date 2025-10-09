
library(dplyr)
library(knitr)

# Crear el conjunto de datos de ejemplo
datos_educacion <- data.frame(
  nivel_educativo = c("Primaria o inferior", 
                      "Secundaria", 
                      "Técnico o tecnológico", 
                      "Superior universitaria o posgrado"),
  porcentaje = c(45, 30, 15, 10)  # Reemplaza con tus porcentajes reales
)

# Crear la tabla con formato kable
tabla_formateada <- datos_educacion %>%
  kable(
    format = "pipe",
    col.names = c("Nivel Educativo", "Porcentaje (%)"),
    align = c("l", "c"),
    caption = "Distribución del Nivel Educativo del Jefe de Hogar"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = FALSE,
    position = "center"
  )

# Mostrar la tabla
print(tabla_formateada)


Modelo_final=lm(log(`Ingreso del hogar`)~Sexo+Estudios+
                  `C. personas en el hogar`+Edad+`Horas trabajadas la semana pasada`+
                  Departamento
                ,Base_datos)

summary(Modelo_final)

Modelo_final=lm(log(`Ingreso del hogar`)~Sexo+Estudios+
                  `C. personas en el hogar`+Edad+`Horas trabajadas la semana pasada`+
                  Departamento
                ,Base_datos)
library(broom)

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


resumen_global <- glance(Modelo_final)


resumen_global <- glance(Modelo_final)
R2 <- round(resumen_global$adj.r.squared, 4)
Fstat <- round(resumen_global$statistic, 2)
pvalor_modelo <- formatC(resumen_global$p.value, format = "e", digits = 2)
N <- resumen_global$df.residual + length(Modelo_final$coefficients)

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
    caption = "Figura 8. Resultados del modelo de regresión lineal múltiple",
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
```

## Prediccion

```{r prediccion}
names(Base_datos)<-make.names(names(Base_datos))


Base_datos$Departamento <- factor(Base_datos$Departamento)

Modelo_predictorio <- lm(
  Ingreso.del.hogar ~ Sexo + Horas.trabajadas.la.semana.pasada + Estudios +
    Cantidad.de.personas.en.el.hogar + Edad + Departamento,
  data = Base_datos
)

nuevos_datos <- data.frame(
  Sexo = c("Hombre", "Mujer", "Hombre", "Mujer"),
  Horas.trabajadas.la.semana.pasada= c(12, 24, 36, 48),
  Estudios = factor(
    c("Secundaria", "Universitaria", "Técnica/Tecnológica", "Postgrado"),
    levels = levels(Base_datos$CATEGORIA_EDUCATIVA)
  ),
  Cantidad.de.personas.en.el.hogar = c(3, 4, 2, 5),
  Edad = c(25, 35, 45, 55),
  Departamento = factor(
    c("Valle del Cauca", "Cauca", "Nariño", "Choco"),
    levels = levels(Base_datos$Departamento)
  )
)

nuevos_datos$Prediccion_log <- predict(Modelo_predictorio, newdata = nuevos_datos)

nuevos_datos$Prediccion_ingreso <- exp(nuevos_datos$Prediccion_log)

nuevos_datos$Sexo_label <- factor(nuevos_datos$Sexo, 
                                  levels = c(1, 2), 
                                  labels = c("Hombre", "Mujer"))

nuevos_datos %>%
  select(Departamento, Sexo_label, Estudios, Edad, Horas.trabajadas.la.semana.pasada,
         Cantidad.de.personas.en.el.hogar, Prediccion_ingreso) %>%
  rename(
    Sexo = Sexo_label,
    `Cantidad de personas` = Cantidad.de.personas.en.el.hogar,
    `Tiempo trabajado` = Horas.trabajadas.la.semana.pasada,
    `Ingreso Predicho` = Prediccion_ingreso
  ) %>%
  kbl(
    caption = "Predicción del Ingreso del Hogar según Características Simuladas",
    align = "c",
    digits = 0
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 13,
    position = "center"
  ) %>%
  row_spec(0, background = "#2b6cb0", color = "white", bold = TRUE) %>%
  footnote(
    general = "Las predicciones corresponden al ingreso estimado (en pesos) a partir del modelo ajustado en logaritmo natural.",
    general_title = "Nota:",
    footnote_as_chunk = TRUE
  )

