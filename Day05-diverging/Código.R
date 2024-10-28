if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyr)) install.packages("tidyr")
if (!require(readr)) install.packages("readr")

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# Cargar datos desde la URL
url <- "https://raw.githubusercontent.com/ricardoramos12/30daychartchallenge/refs/heads/main/Day05-diverging/data.csv"
data <- read_csv(url, show_col_types = FALSE)

# Calcular la diferencia de ventas entre las sucursales para cada mes
data_diff <- data %>%
  pivot_wider(names_from = branch, values_from = sales) %>%
  mutate(divergence = `Sucursal A` - `Sucursal B`)

# Graficar la divergencia en ventas entre las sucursales
ggplot(data_diff, aes(x = month, y = divergence)) +
  geom_bar(stat = "identity", aes(fill = divergence > 0), width = 0.6) +
  geom_text(aes(label = round(divergence, 1)), 
            vjust = ifelse(data_diff$divergence >= 0, -0.3, 1.3), 
            color = "black") +  # Ajuste de la posición vertical
  scale_fill_manual(values = c("red", "blue"), labels = c("Sucursal B > Sucursal A", "Sucursal A > Sucursal B")) +
  labs(title = "Divergencia en Ventas entre Sucursal A y Sucursal B", x = "Mes", y = "Divergencia en Ventas") +
  theme_minimal() +
  theme(legend.title = element_blank())
