# Cargar librerías
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

url <- "https://github.com/ricardoramos12/30daychartchallenge/raw/refs/heads/main/Day20-Correlation/datos_correlacion.csv"
datos <- read_csv(url)

# Filtrar los datos para obtener las series necesarias
datos_filtrados <- datos %>%
  filter(Indicador %in% c("Crecimiento_PIB", "Inflación"))
datos_wide <- datos_filtrados %>%
  pivot_wider(names_from = Indicador, values_from = Porcentaje)

# Calcular la correlación entre Crecimiento_PIB e Inflación
correlacion <- cor(datos_wide$Crecimiento_PIB, datos_wide$Inflación, use = "complete.obs")

# Gráfico de líneas con subtítulo de correlación
ggplot(datos_filtrados, aes(x = Año, y = Porcentaje, color = Indicador)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Evolución del Crecimiento del PIB e Inflación en el Tiempo",
    subtitle = paste("Correlación:", round(correlacion, 2)),
    x = "Año", 
    y = "Porcentaje (%)",
    color = "Indicador"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.title.position = "plot"
  )

