# Cargar librerías necesarias
library(dplyr)
library(lubridate)
library(ggplot2)

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
  labs(title = "Tendencia de Incertidumbre en el Precio de Bitcoin (Bueno vs. Malo) (2022-2024)",
       x = "Mes",
       y = "Proporción",
       fill = "Incertidumbre") +
  scale_y_continuous(labels = scales::percent_format()) +  
  scale_fill_manual(values = c("Bueno" = "skyblue", "Malo" = "orange")) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))