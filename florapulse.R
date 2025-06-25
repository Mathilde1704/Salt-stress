library(ggplot2)
library(vctrs)
library(tidyr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(scales)
library(readr)
library(hms)
library(gridExtra) 
library(patchwork)
library(emmeans)

flora = read.csv("F:/POST_DOC_SALT'EAU/données/villeroy_2024/flux_eau/florapulses/25_07_24/flora_r.csv", sep=";")

flora =read.csv("F:/POST_DOC_SALT'EAU/données/villeroy_2024/flux_eau/florapulses/25_07_24/flora_r_sansD722_0608.csv", sep=";")

flora$heure <- as.POSIXct(flora$heure, format="%H:%M")
flora$jour <- as.POSIXct(flora$jour, format = "%d/%m/%Y")
flora <- flora %>%
  mutate(heure_extrait = format(as.POSIXct(heure), "%H:%M:%S"))

flora$datetime <- as.POSIXct(paste(flora$jour, flora$heure), format = "%d/%m/%Y %H:%M:%S")
flora$jour <- as.Date(flora$jour, format = "%d/%m/%Y")
filtered_data <- flora %>% filter(Type %in% c("D601", "D722_D746"))
filtered_data_b <- flora %>% filter(Type %in% c("D722", "D746"))
filtered_data_b <- flora %>% filter(Type %in% c("D601"))



flora_date <- subset(flora, jour == "2024-08-24")
flora_date <- subset(flora, jour == "2024-08-10")
flora_date <- subset(flora, jour == "2024-09-19")


flora_traitement <- subset(flora, Type %in% c("D601", "D746"))

max_min_data <- flora_traitement %>%
  group_by(jour, Type) %>%
  summarise(max_potentiel_Mpa = max(potentiel_Mpa, na.rm = TRUE),
            min_potentiel_Mpa = min(potentiel_Mpa, na.rm = TRUE))

ggplot(max_min_data, aes(x = jour)) +
  geom_line(aes(y = max_potentiel_Mpa, color = Type), linetype = "solid",  size = 1) +  # Max line
  geom_line(aes(y = min_potentiel_Mpa, color = Type), linetype = "solid", size = 1) + # Min line (dashed)
  labs(title = "Max and Min potentiel_Mpa per day for sensors D601 and D722_D746",
       x = "Day",
       y = "Potentiel Mpa") +
  scale_x_date(date_breaks = "2 days", date_labels = "%d-%m")+   # Set tick marks every 5 days
  theme_minimal()+
  theme(axis.text.x = element_text(face = "bold", 
                                   size = 7, angle = 90, hjust=1))+
  geom_point(data = cha_pns, aes(x= date, y= moyenne_cap ))
#scale_y_continuous(limits = c(-3.3, -0.1), breaks = seq(-3.3, -0.1, 0.4)) 

ggplot(flora_date, aes(x = heure_extrait, y = potentiel_Mpa, color = Type)) +
  geom_point() +
  theme_minimal()+
  scale_x_discrete(breaks = flora_date$heure_extrait[seq(1, nrow(flora_date), by = 6)]) + 
  theme(axis.text.x = element_text(face = "bold", 
                                   size = 12, angle = 90, hjust=1),
        axis.text.y = element_text(face = "bold", 
                                   size = 12, angle = 0))


max_data <- filtered_data_b %>%
  group_by(jour, Type) %>%
  summarise(max_potentiel_Mpa = max(potentiel_Mpa, na.rm = TRUE))


ggplot(max_data, aes(x = jour, y = max_potentiel_Mpa, color = Type)) +
  geom_line() +
  labs(title = "Maximum potentiel_Mpa per day for sensors D601 and D722_D746",
       x = "Day",
       y = "Maximum Potential (MPa)") +
  scale_x_date(date_breaks = "4 days", date_labels = "%d-%m") +
  scale_y_continuous(limits = c(-0.9, -0.1), breaks = seq(-0.9, -0.1, 0.2)) +
  theme_minimal()

min_data <- filtered_data_b %>%
  group_by(jour, Type) %>%
  summarise(min_potentiel_Mpa = min(potentiel_Mpa, na.rm = TRUE))

max_min_data <- filtered_data_b %>%
  group_by(jour, Type) %>%
  summarise(max_potentiel_Mpa = max(potentiel_Mpa, na.rm = TRUE),
            min_potentiel_Mpa = min(potentiel_Mpa, na.rm = TRUE))





cha_pns = read.csv("F:/POST_DOC_SALT'EAU/données/villeroy_2024/flux_eau/cha_pns.csv", sep=";")
cha_pns = read.csv("F:/POST_DOC_SALT'EAU/données/villeroy_2024/flux_eau/cha_ps.csv", sep=";") 

cha_pns$date <- as.Date(cha_pns$date, format = "%d/%m/%Y")

# Create the ggplot showing both maximum and minimum potentiel_Mpa per day for the selected sensors
ggplot(max_min_data, aes(x = jour)) +
  geom_line(aes(y = max_potentiel_Mpa, color = Type), linetype = "solid",  size = 1) +  # Max line
  geom_line(aes(y = min_potentiel_Mpa, color = Type), linetype = "solid", size = 1) + # Min line (dashed)
  labs(title = "Max and Min potentiel_Mpa per day for sensors D601 and D722_D746",
       x = "Day",
       y = "Potentiel Mpa") +
  scale_x_date(date_breaks = "2 days", date_labels = "%d-%m")+   # Set tick marks every 5 days
  theme_minimal()+
  theme(axis.text.x = element_text(face = "bold", 
                                   size = 7, angle = 90, hjust=1))+
  geom_point(data = cha_pns, aes(x= date, y= moyenne_cap ))
  #scale_y_continuous(limits = c(-3.3, -0.1), breaks = seq(-3.3, -0.1, 0.4)) 
  
mean_data <- max_min_data %>%
  group_by(jour) %>%
  summarize(
    mean_min_potentiel_Mpa = mean(min_potentiel_Mpa, na.rm = TRUE),
    mean_max_potentiel_Mpa = mean(max_potentiel_Mpa, na.rm = TRUE))

###Variation potentiel tige et potentiel base Chambre a pression###

data <- read.csv("F:/POST_DOC_SALT'EAU/données/villeroy_2024/flux_eau/potentiel_hydrique/potentiel_base/potentiel_base_2024.csv", sep = ";")
data$date <- as.Date(data$date, format="%d/%m/%Y")
data$potentiel_base_Mpa <- -data$potentiel_base_Mpa

data_stats <- data %>%
  group_by(date, traitement) %>%
  summarise(
    mean_potential = mean(potentiel_base_Mpa, na.rm = TRUE),
    sd_potential = sd(potentiel_base_Mpa, na.rm = TRUE)
  )

a=ggplot(data_stats, aes(x = date, y = mean_potential, color = traitement)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_potential - sd_potential, ymax = mean_potential + sd_potential), width = 0.2) +
  scale_color_manual(values = c("salt" = "red", "unsalted" = "blue")) +
  labs(y = "Water potential (MPa)", title = "base") +
  theme_minimal() +
  scale_x_date(date_labels = "%d/%m/%Y", date_breaks = "4 day") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_y_continuous(limits = c(-1.5, -0.1), breaks = seq(-1.5, -0.1, by = 0.2))   # Ajustement des limites et des intervalles

a
test<-lm(potentiel_base_Mpa ~ traitement*date  ,data=data)
anova(test)
summary(test)

tukey_by_date <- emmeans(anova_model, pairwise ~ traitement | date)
summary(tukey_by_date$contrasts)


data_t <- read.csv("F:/POST_DOC_SALT'EAU/données/villeroy_2024/flux_eau/potentiel_hydrique/potentiel_tige/potentiel_tige_2024.csv", sep = ";")

data_t$date <- as.Date(data_t$date, format="%d/%m/%Y")
data_t$Mpa <- -data_t$Mpa

data_stats_t <- data_t %>%
  group_by(date, traitement) %>%
  summarise(
    mean_potential = mean(Mpa, na.rm = TRUE),
    sd_potential = sd(Mpa, na.rm = TRUE)
  )

b=ggplot(data_stats_t, aes(x = date, y = mean_potential, color = traitement)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_potential - sd_potential, ymax = mean_potential + sd_potential), width = 0.2) +
  scale_color_manual(values = c("salt" = "red", "unsalted" = "blue")) +
  labs(y = "Water potential (MPa)", title = "tige") +
  theme_minimal() +
  scale_x_date(date_labels = "%d/%m/%Y", date_breaks = "4 day") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  #scale_y_continuous(limits = c(-1.5, -0.1), breaks = seq(-1.5, -0.1, by = 0.2))+
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank())   # Ajustement des limites et des intervalles
b
t_test_results <- data_t %>%
  group_by(date) %>%
  summarise(p_value = t.test(Mpa ~ traitement, data = .)$p.value)

combined_plot <- (a | b)


data_moyenne_t <- data_t %>%
  group_by(date, traitement) %>%
  summarise(potentiel_moyen_t = mean(Mpa, na.rm = TRUE))


data_stats <- data %>%
  group_by(date, traitement) %>%
  summarise(
    mean_potential = mean(potentiel_base_Mpa, na.rm = TRUE),
    sd_potential = sd(potentiel_base_Mpa, na.rm = TRUE)
  )

a=ggplot(data_stats, aes(x = date, y = mean_potential, color = traitement)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_potential - sd_potential, ymax = mean_potential + sd_potential), width = 0.2) +
  scale_color_manual(values = c("salt" = "red", "unsalted" = "blue")) +
  labs(y = "Water potential (MPa)", title = "base") +
  theme_minimal() +
  scale_x_date(date_labels = "%d/%m/%Y", date_breaks = "10 day") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_y_continuous(limits = c(-1.3, -0.1), breaks = seq(-1.3, -0.1, by = 0.2))   # Ajustement des limites et des intervalles
  a
t_test_results <- data %>%
  group_by(date) %>%
  summarise(p_value = t.test(potentiel_base_Mpa ~ traitement, data = .)$p.value)b=ggplot(data_moyenne_t, aes(x = date, y = potentiel_moyen_t, color = traitement)) +
  geom_point(size = 3) +
  geom_line()+
  labs(title = "Stem foliar water potential",x = "Date") +
  scale_color_manual(values = c("salt" = "red", "unsalted" = "blue")) +
  scale_y_continuous(limits = c(-1.3, -0.1), breaks = seq(-1.3, -0.1, by = 0.2)) +  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_labels = "%d/%m/%Y", date_breaks = "10 day")+
   theme(
     axis.text.y = element_blank(),
     axis.title.y = element_blank(),
     axis.ticks.y = element_blank())

b

combined_plot <- (a | b)



flora_cap_base <- read.csv("F:/POST_DOC_SALT'EAU/données/villeroy_2024/flux_eau/flora_cap_base.csv", sep = ";")
flora_cap <- read.csv("F:/POST_DOC_SALT'EAU/données/villeroy_2024/flux_eau/cha_flora.csv", sep = ";")
flora_cap <- read.csv("F:/POST_DOC_SALT'EAU/données/villeroy_2024/flux_eau/cha_flora_feuille.csv", sep = ";")
flora_cap <- read.csv("F:/POST_DOC_SALT'EAU/données/villeroy_2024/flux_eau/Classeur2.csv", sep = ";")


ggplot(flora_cap, aes(x = moyenne_cap, y = flora)) +
  geom_point(aes(shape = interaction(traitement, type), 
                 color = interaction(traitement, type)), size = 3) +  # Points colorés et formes selon traitement et type
  
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1) +  # Ligne y=x
  
  geom_smooth(method = "lm", se = FALSE, aes(color = interaction(traitement, type)), linetype = "solid") +  # Régressions linéaires par groupe
  
  scale_shape_manual(values = c("pns.base" = 17, "pns.tige" = 16,  # Formes différentes pour chaque groupe
                             "ps.base" = 15, "ps.tige" = 18)) +
  
  scale_color_manual(values = c("pns.base" = "darkblue", "pns.tige" = "lightblue",  # Couleurs spécifiques par groupe
                                "ps.base" = "orange", "ps.tige" = "red")) +
  
  labs(title = "Mpa vs Moyenne Cap selon Traitement et Type avec Régressions",
       x = "Moyenne Cap", 
       y = "Mpa",
       color = "Traitement et Type",
       shape = "Traitement et Type") +  # Légendes pour couleur et forme
  
  theme_minimal()+
  
  scale_y_continuous(limits = c(-1.8, 0), breaks = seq(-1.8, 0, by = 0.2))+
  scale_x_continuous(limits = c(-1.8, 0), breaks = seq(-1.8, 0, by = 0.2))



flora_cap_test <- read.csv("F:/POST_DOC_SALT'EAU/données/villeroy_2024/flux_eau/cha_flora_feuille.csv", sep = ";")

flora_cap_test$potentiel_base_Mpa = -flora_cap_test$potentiel_base_Mpa

summary_data <- flora_cap_test %>%
  group_by(traitement, type, date) %>%
  summarise(
    mean_potentiel_base_Mpa = mean(potentiel_base_Mpa, na.rm = TRUE),
    sd_potentiel_base_Mpa = sd(potentiel_base_Mpa, na.rm = TRUE),
    mean_flora = mean(flora, na.rm = TRUE),
    sd_flora = sd(flora, na.rm = TRUE)
  )

ggplot(summary_data, aes(x = mean_potentiel_base_Mpa, y = mean_flora)) +
  geom_point(aes(shape = interaction(traitement, type), 
                 color = interaction(traitement, type)), size = 3) +
  
  #geom_point(data= na.omit(flora_cap_test), aes(x= moyenne_cap, y = feuille_flora), size=4)+

  
  geom_errorbar(aes(ymin = mean_flora - sd_flora, ymax = mean_flora + sd_flora, 
                    color = interaction(traitement, type)), width = 0.1) +  # Barres d'erreur pour flora
  
  geom_errorbarh(aes(xmin = mean_potentiel_base_Mpa - sd_potentiel_base_Mpa, 
                     xmax = mean_potentiel_base_Mpa + sd_potentiel_base_Mpa, 
                     color = interaction(traitement, type)), height = 0.1) +  # Barres d'erreur horizontales
  
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1) +  # Ligne y=x
  
  geom_smooth(method = "lm", se = FALSE, aes(color = interaction(traitement, type)), linetype = "solid") +  # Régressions linéaires par groupe
  
  scale_shape_manual(values = c("pns.base" = 17, "pns.tige" = 16,  # Formes différentes pour chaque groupe
                                "ps.base" = 15, "ps.tige" = 18)) +
  
  scale_color_manual(values = c("pns.base" = "darkblue", "pns.tige" = "lightblue",  # Couleurs spécifiques par groupe
                                "ps.base" = "orange", "ps.tige" = "red")) +
  
  labs(title = "Mpa vs Moyenne Cap selon Traitement et Type avec Moyennes et Écarts-types",
       x = "Moyenne Potentiel Base Mpa", 
       y = "Moyenne Flora",
       color = "Traitement et Type",
       shape = "Traitement et Type") +  
  
  theme_minimal() +
  
  scale_y_continuous(limits = c(-1.8, 0), breaks = seq(-1.8, 0, by = 0.2)) +
  scale_x_continuous(limits = c(-1.8, 0), breaks = seq(-1.8, 0, by = 0.2))


regression_results <- flora_cap_test %>%
  group_by(traitement, type) %>%
  summarise(
    model = list(lm(flora ~ potentiel_base_Mpa, data = cur_data())),
    .groups = 'drop'
  ) %>%
  mutate(

        R2 = map_dbl(model, ~summary(.x)$r.squared),
   
        
    RMSE = map_dbl(model, ~{
      resid <- residuals(.x) # Obtenir les résidus
      sqrt(mean(resid^2))    # Racine carrée de la moyenne des carrés des résidus
    })
  )

print(regression_results)
