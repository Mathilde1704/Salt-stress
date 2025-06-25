library(ggplot2)
library(readr)
library(dplyr)
library(scales)

pluvio = read.csv("F:/POST_DOC_SALT'EAU/données/villeroy_2024/meteo/pluvio.csv", sep=";")
meteo = read.csv("F:/POST_DOC_SALT'EAU/données/villeroy_2024/meteo/meteo.csv", sep=";")
temp = read.csv("F:/POST_DOC_SALT'EAU/données/villeroy_2024/meteo/temperature_villeroy.csv", sep=";")


pluvio$heure <- as.POSIXct(pluvio$heure, format="%H:%M")
pluvio$date<- as.POSIXct(pluvio$date, format="%d/%m/%Y")

daily_precipitation_sum <- aggregate(pluvio_mm ~ date, pluvio, sum)


meteo$heure <- as.POSIXct(meteo$heure, format="%H:%M")
meteo$date<- as.POSIXct(meteo$date, format="%d/%m/%Y")

temp$heure <- as.POSIXct(temp$heure, format="%H:%M")
temp$date<- as.POSIXct(temp$date, format="%d/%m/%Y")
temp <- temp %>%
  mutate(hour = as.numeric(format(heure, "%H")))

meteo <- meteo %>%
  mutate(hour = as.numeric(format(heure, "%H")))

meteo_bis <- meteo %>%
  filter(hour >= 5 & hour < 23)

pluvio <- pluvio %>%
  mutate(heure_extrait = format(as.POSIXct(heure), "%H:%M:%S"))
pluvio <- pluvio %>%
  mutate(datetime = as.POSIXct(paste(date, heure_extrait), format = "%Y-%m-%d %H:%M:%S"))

meteo <- meteo %>%
  mutate(heure_extrait = format(as.POSIXct(heure), "%H:%M:%S"))
meteo <- meteo %>%
  mutate(datetime = as.POSIXct(paste(date, heure_extrait), format = "%Y-%m-%d %H:%M:%S"))

temp <- temp %>%
  mutate(heure_extrait = format(as.POSIXct(heure), "%H:%M:%S"))
temp <- temp %>%
  mutate(datetime = as.POSIXct(paste(date, heure_extrait), format = "%Y-%m-%d %H:%M:%S"))


labels_data <- pluvio %>% filter(pluvio_mm > 0.7)

# Générer le graphique avec ggplot2


a =ggplot(pluvio, aes(x = date, y = pluvio_mm)) +
  geom_line(color = "blue") +
  labs(title = "Pluviométrie au fil du temps",
       x = "Datetime",
       y = "Pluvio (mm)") +
  theme_minimal()+
  #scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) + 
  scale_x_datetime(date_breaks = "4 days", date_labels = "%d/%m") +
  theme(
    axis.text.x = element_text(size = 8 ),  
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14)
  )
a
ggplot(pluvio, aes(x = date, y = pluvio_mm)) +
  geom_col(color = "blue", fill = "blue") +  # Utiliser geom_col pour des bâtonnets
  labs(title = "Pluviométrie au fil du temps",
       x = "Datetime",
       y = "Pluvio (mm)") +
  theme_minimal() +
  scale_x_datetime(date_breaks = "4 days", date_labels = "%d/%m") +
  theme(
    axis.text.x = element_text(size = 8),  
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14)
  )

labels_data <- meteo %>% filter(T_AIR > 35)


max_temps_per_day <- temp %>%
  group_by(date) %>%
  summarize(max_temp = max(T_AIR), datetime = first(datetime))

min_temps_per_day <- temp %>%
  group_by(date) %>%
  summarize(min_temp = min(T_AIR), datetime = first(datetime))

labels_data <- max_temps_per_day %>%
  filter(max_temp > 35)

b=ggplot() +
  geom_line(data = max_temps_per_day, aes(x = datetime, y = max_temp), color = "red") +
  geom_point(data = max_temps_per_day, aes(x = datetime, y = max_temp), color = "red") +
  
  geom_line(data = min_temps_per_day, aes(x = datetime, y = min_temp), color = "blue") +
  geom_point(data = min_temps_per_day, aes(x = datetime, y = min_temp), color = "blue") +
  
  # geom_text(data = labels_data, aes(x = datetime, y = max_temp, label = date), 
  #           vjust = -1, hjust = 1, angle = 45, size = 3) +
  
  #scale_y_continuous(limits = c(20, 40), breaks = seq(20, 40, by = 2)) + 
  labs(title = "Températures maximales et minimales par jour",
       x = "Datetime",
       y = "Température (°C)") +
  theme_minimal() +
  scale_x_datetime(date_breaks = "2 days", date_labels = "%d/%m")+
  theme(
    axis.text.x = element_text(size = 8, angle= 90),  
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14)
  )

b
combined_plot <- (b | a) 
print(combined_plot)

labels_data <- meteo %>% 
  filter(T_AIR > 33) %>%
  group_by(date) %>% 
  slice(1)  # Garder uniquement la première occurrence de chaque date

# Générer le graphique avec ggplot2
ggplot(meteo, aes(x = datetime, y = T_AIR)) +
  geom_line(color = "black") +
  geom_text(data = labels_data, aes(label = format(datetime, "%d/%m")), 
            vjust = -2, hjust = 1, angle = 45, size = 3) +  # Augmentez la valeur de vjust pour remonter les labels
  labs(title = "Température air",
       x = "Datetime",
       y = "Température (°C)") +
  theme_minimal() +
  scale_x_datetime(date_breaks = "5 days", date_labels = "%d/%m")


# Générer le graphique avec ggplot2

meteo_HR <- aggregate(HR ~ date, data = meteo_bis, FUN = mean)

ggplot(meteo, aes(x = datetime, y = HR)) +
  geom_line(color = "black") +
  labs(title = "Température air",
       x = "Datetime",
       y = "HR (%)") +
  theme_minimal() +
  scale_x_datetime(date_breaks = "4 days", date_labels = "%d/%m")


ggplot(meteo_HR, aes(x = date, y = HR)) +
  geom_point(color = "black") +
  geom_line()+
  labs(title = "Température air",
       x = "Datetime",
       y = "HR (%)") +
  theme_minimal() +
  scale_x_datetime(date_breaks = "4 days", date_labels = "%d/%m")




