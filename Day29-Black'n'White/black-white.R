# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(lubridate)

# Leer el dataset desde el enlace
url <- "https://github.com/ricardoramos12/30daychartchallenge/raw/refs/heads/main/Day27-good_bad/dataset.csv"
data <- read.csv(url, stringsAsFactors = FALSE)
data$Fecha <- as.Date(data$Fecha)

# Resumir los datos para contar la cantidad de "Bueno" y "Malo" por mes
data_resumida <- data %>%
  group_by(Mes = floor_date(Fecha, "month"), Incertidumbre) %>%
  summarise(Cuenta = n(), .groups = 'drop')

# Crear la gráfica
ggplot(data_resumida, aes(x = Mes, y = Cuenta, fill = Incertidumbre)) +
  geom_bar(stat = "identity", position = "fill") +  
  labs(title = "Tendencia de Incertidumbre en el Precio de Bitcoin (Bueno vs. Malo) 2022-2024",
       x = "Mes",
       y = "Proporción",
       fill = "Incertidumbre") +
  scale_y_continuous(labels = scales::percent_format()) +  
  scale_fill_manual(values = c("Bueno" = "#7D7D7D", "Malo" = "#D9D9D9")) +  # Colores en escala de grises
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",  # Posicionar la leyenda en la parte superior
        plot.title = element_text(size = 14, face = "bold"))  # Estilo del título
