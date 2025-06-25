library(ggplot2)
library(readr)
library(dplyr)
library(scales)

brix = read.csv("F:/POST_DOC_SALT'EAU/données/villeroy_2024/Brix/brix.csv", sep=";")


brix$date <- as.Date(brix$date, format = "%d/%m/%Y")

# Calculer la moyenne de Brix par date et traitement
brix_grouped <- brix %>%
  group_by(date, traitement) %>%
  summarise(mean_brix = mean(Brix, na.rm = TRUE))

# Créer le graphique avec ggplot
ggplot(brix_grouped, aes(x = date, y = mean_brix, color = traitement, group = traitement)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  labs(title = "Moyenne des Brix par jour et par traitement",
       x = "Date",
       y = "Moyenne des Brix",
       color = "Traitement") +
  theme_minimal() +
  scale_y_continuous(limits = c(2, 22), breaks = seq(2, 22, by = 2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "4 days", date_labels = "%d/%m")
