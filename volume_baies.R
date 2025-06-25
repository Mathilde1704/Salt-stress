library(ggplot2)
library(readr)
library(dplyr)
library(scales)
library(agricolae)
library(multcompView)
library(emmeans)
library(multcomp)

vol = read.csv("F:/POST_DOC_SALT'EAU/données/villeroy_2024/volume_baies/vol_baies_r_corrigé.csv", sep=";")

vol$Date <- as.POSIXct(vol$Date, format="%d/%m/%Y")

avg_volume <- aggregate(Volume ~ Date + traitement+ individu, vol, mean)
avg_volume <- aggregate(Volume ~ traitement, vol, mean)


volume_moy <- aggregate(Volume ~ Date + traitement, vol, mean)


vol_stats <- vol %>%
  group_by(traitement, Date) %>%
  summarise(
    sd_volume = sd(Volume, na.rm = TRUE)
  )

vol_stats$date_traitement <- paste(vol_stats$Date, vol_stats$traitement, sep = "_")
volume_moy$date_traitement <- paste(volume_moy$Date, volume_moy$traitement, sep = "_")

volume_moy[,c("sd")] = 
  vol_stats[match(volume_moy$date_traitement, vol_stats$date_traitement), c("sd_volume")]

ggplot(volume_moy, aes(x = Date, y = Volume, color = traitement, group = traitement)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(
      ymin = Volume - sd, 
      ymax = Volume + sd,
      position = ifelse(traitement == "PNS", 0.2, 0)  
    ),
    width = 0.2,
    size = 1, 
    position = position_dodge(0.1)  
  ) +
  labs(title = "Volume moyen par Date et Traitement",
       x = "Date",
       y = "Volume moyen (mL)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("PNS" = "#75b8d1", "PS" = "#d18975")) +
  scale_y_continuous(limits = c(0.5, 2.3), breaks = seq(0.5, 2.3, 0.2)) 
  

vol_max = read.csv("F:/POST_DOC_SALT'EAU/données/villeroy_2024/volume_baies/vol_max.csv", sep=";")


ggplot(vol_max, aes(x = traitement, y = Volume, color = traitement, group = traitement)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("blue", "red")) 


test<-lm(Volume ~ traitement  ,data=vol_max)
anova(test)
summary(test)


# Filtrer les données pour le 23/08
date_specific <- as.POSIXct("2024-07-01")
date_specific <- as.POSIXct("2024-07-11")
date_specific <- as.POSIXct("2024-07-25")
date_specific <- as.POSIXct("2024-08-01")
date_specific <- as.POSIXct("2024-08-08")
date_specific <- as.POSIXct("2024-08-23")

volume_23_august <- volume_moy %>% filter(Date == date_specific)

vol_ps <- volume_23_august %>% filter(traitement == "PS") %>% pull(Volume)
vol_pns <- volume_23_august %>% filter(traitement == "PNS") %>% pull(Volume)

percent_difference <- (vol_ps - vol_pns) / vol_pns * 100
percent_difference


test<-lm(Volume.Moyen_mL ~ traitement*Date  ,data=vol)
anova(test)
summary(test)


anova_model <- aov(Volume ~ traitement * date_simple, data = vol)
posthoc <- emmeans(anova_model, pairwise ~ traitement | date_simple)

lettres <- cld(posthoc$emmeans, adjust = "sidak")
summary(lettres)

plot(posthoc)
plot(lettres)

vol$Date_traitement <- factor(paste(vol$Date, vol$traitement), 
                               levels = unique(paste(vol$Date, vol$traitement)))




ggplot(vol, aes(x = as.factor(Date), y = Volume, fill = traitement)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("PS" = "#d18975", "PNS" = "#75b8d1")) + # PS en rouge et PNS en bleu
  labs(title = "Variabilité du volume des baies en fonction des traitements et des dates",
       x = "Date",
       y = "Volume des baies (mL)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "top")


## nombre de baies par grappe sur chaque traitement

max_numero_baie_per_individu <- aggregate(Numero_baie ~ individu + traitement, data = vol, max)

max_numero_baie_per_individu <- aggregate(Numero_baie ~  traitement, data = vol, max)

ggplot(max_numero_baie_per_individu, aes(x = traitement, y = Numero_baie, color = traitement)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("blue", "red"))+
scale_y_continuous(limits = c(60, 250), breaks = seq(60, 250, 20))


max_numero_baie_per_individu %>%
  group_by(traitement) %>%
  summarise(min = min(Numero_baie, na.rm = TRUE))




moyennes_traitement <- max_numero_baie_per_individu %>%
  group_by(traitement) %>%
  summarize(moyenne_Numero_baie = mean(Numero_baie, na.rm = TRUE))

max_numero_baie_per_individu %>%
  group_by(traitement) %>%
  summarise(mediane = median(Numero_baie, na.rm = TRUE))

test<-lm(Numero_baie ~ traitement  ,data=max_numero_baie_per_individu)
anova(test)
summary(test)

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

