
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