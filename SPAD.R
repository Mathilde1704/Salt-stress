library(ggplot2)
library(readr)
library(dplyr)
library(scales)

SPAD = read.csv("F:/POST_DOC_SALT'EAU/données/villeroy_2024/SPAD/SPAD.csv", sep=";")


SPAD$date <- as.Date(SPAD$date, format = "%d/%m/%Y")

# Calculer la moyenne de Brix par date et traitement
SPAD_grouped <- SPAD %>%
  group_by(date, traitement) %>%
  summarise(mean_SPAD = mean(SPAD_moy, na.rm = TRUE))

# Créer le graphique avec ggplot
ggplot(SPAD_grouped, aes(x = date, y = mean_SPAD, color = traitement, group = traitement)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  labs(title = "Moyenne des SPAD par jour et par traitement",
       x = "Date",
       y = "Moyenne des SPAD",
       color = "Traitement") +
  theme_minimal() +
  scale_y_continuous(limits = c(30, 38), breaks = seq(30, 38, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  #scale_x_date(date_breaks = "4 days", date_labels = "%d/%m")
