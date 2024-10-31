
library(readr)
library(ggplot2)

url <- "https://github.com/ricardoramos12/30daychartchallenge/raw/refs/heads/main/Day18-AsianDevelopmentBank/fertilidad.csv"
data <- read_csv(url)
data


# Convertir la columna 'Rango' en factor para asegurar el orden correcto
data$Rango <- factor(data$Rango, levels = unique(data$Rango))
data$Tasa <- round(data$Tasa, 2)

# Crear el gráfico de líneas con etiquetas en cada punto
ggplot(data, aes(x = Rango, y = Tasa, color = Paises, group = Paises)) +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +  
  geom_text(aes(label = Tasa), vjust = -0.5, size = 4) +  # Agregar etiquetas con valores
  labs(title = "Tasa de Fertilidad en Tailandia y Vietnam (1980-2020)",
       x = "Rango de Años",
       y = "Tasa de Fertilidad") +
  scale_color_manual(values = c("Tailandia" = "blue", "Vietnam" = "red")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  