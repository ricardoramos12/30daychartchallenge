# Cargar librerías necesarias
library(ggplot2)
library(dplyr)

# Cargar los datos desde el archivo CSV en GitHub
url <- "https://raw.githubusercontent.com/ricardoramos12/30daychartchallenge/refs/heads/main/Day26-ai/ai.csv"
data <- read.csv(url, stringsAsFactors = FALSE)

# Convertir la columna de Fecha a tipo Date
data$Fecha <- as.Date(data$Fecha)

# Graficar
ggplot(data, aes(x = Fecha)) +
  geom_line(data = subset(data, Fecha < as.Date("2023-01-01")), 
            aes(y = Precio_Real, color = "Precio Real"), size = 1.2) +
  # Línea de predicción IA
  geom_line(aes(y = Prediccion, color = "Predicción IA"), size = 1.2, linetype = "dashed") +
  # Relleno de los intervalos de confianza
  geom_ribbon(aes(ymin = Lower_Bound, ymax = Upper_Bound), alpha = 0.3, fill = "orange") +
  # Puntos de la predicción
  geom_point(aes(y = Prediccion), color = "orange", size = 2) +
  labs(title = "Predicción del Precio de Bitcoin con Intervalos de Confianza (2020-2026)",
       x = "Fecha",
       y = "Precio de Bitcoin (USD)",
       color = "Leyenda") +
  scale_color_manual(values = c("Precio Real" = "blue", "Predicción IA" = "red")) +
  scale_x_date(limits = as.Date(c("2020-01-01", "2026-12-01"))) +  # Limitar el eje X hasta diciembre de 2026
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
