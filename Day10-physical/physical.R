# Cargar librerías necesarias
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
library(readr)

# Cargar datos desde la URL
url <- "https://github.com/ricardoramos12/30daychartchallenge/raw/refs/heads/main/Day10-physical/data.csv"
data <- read_csv(url, show_col_types = FALSE)

# Gráfico de densidad de estatura por deporte
ggplot(data, aes(x = Height, fill = Sport)) +
  geom_density(alpha = 0.6) +
  labs(title = "Distribución de Estatura de Atletas en Diferentes Deportes",
       x = "Estatura (cm)", y = "Densidad") +
  scale_fill_manual(values = c("skyblue", "orange", "green")) +
  theme_minimal()

