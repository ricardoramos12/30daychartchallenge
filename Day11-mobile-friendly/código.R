# Instalar y cargar las bibliotecas necesarias
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

url2 <- "https://raw.githubusercontent.com/ricardoramos12/30daychartchallenge/refs/heads/main/Day11-mobile-friendly/data.csv"
data <- read.csv(url2,sep = ",")

# Resumir la cantidad total de sesiones por hora y tipo de aplicación
data_summary <- data %>%
  group_by(hora, tipo_app) %>%
  summarise(total_sesiones = sum(sesiones), .groups = "drop")

# Graficar la distribución de uso de aplicaciones móviles a lo largo del día
ggplot(data_summary, aes(x = hora, y = total_sesiones, color = tipo_app)) +
  geom_line(size = 1.2) +
  labs(
    title = "Distribución de Uso de Aplicaciones Móviles a lo Largo del Día",
    x = "Hora del Día",
    y = "Número Total de Sesiones",
    color = "Tipo de Aplicación"
  ) +
  theme_minimal()

