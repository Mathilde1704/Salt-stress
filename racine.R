
library(ggplot2)
library(dplyr)
library(tidyverse)
library(patchwor)

setwd("F:/POST_DOC_SALT'EAU/données/villeroy_2024/racine")
racine =read.csv("F:/POST_DOC_SALT'EAU/données/villeroy_2024/racine/racines.csv", sep=';', dec=".")


racine_ps_pfort <- subset(racine, caracteristique %in% c("pieds_fort"))
racine_ps_pfaible <- subset(racine, caracteristique %in% c("pieds_faible"))

racine__pns_p1 <- subset(racine, caracteristique %in% c("p1"))
racine_pns_p2 <- subset(racine, caracteristique %in% c("p2"))

racine_ps_mort <- subset(racine, caracteristique %in% c("	
pieds_mort"))


a= ggplot(racine__pns_p1, aes(x = prfondeur_num , y = sodium_ppm_M.S, size = diametre_mm)) +
  geom_point() +
  scale_color_manual(values = c("ps" = "red", "pns" = "blue")) + 
  scale_y_continuous(limits = c(2300, 11140), breaks = seq(2300, 11140, 1000)) +
  
  scale_x_continuous(limits = c(20, 70), breaks = seq(20, 70, 10)) +
  labs(
    x = "Profondeur moyenne (cm)",
    y = "Sodium (ppm)",
    size = "Diamètre des racines (mm)"  
  ) +
  theme_minimal() +
  theme(legend.position = "none")+
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) 

b=ggplot(racine_pns_p2, aes(x = prfondeur_num , y = sodium_ppm_M.S, size = diametre_mm)) +
  geom_point() +
  scale_color_manual(values = c("ps" = "red", "pns" = "blue")) + 
  scale_y_continuous(limits = c(2300, 11140), breaks = seq(2300, 11140, 1000)) +
  scale_x_continuous(limits = c(20, 70), breaks = seq(20, 70, 10)) +
  labs(
    x = "Profondeur moyenne (cm)",
    y = "Sodium (ppm)",
    size = "Diamètre des racines (mm)"  # Taille des points liée au diamètre des racines
  ) +
  theme_minimal()+
  theme(legend.position = "none")  +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(axis.title.y = element_blank()) 
b


library(ggplot2)

# Lire le fichier CSV
df <- read.csv("F:/POST_DOC_SALT'EAU/données/villeroy_2024/racine/racine_2.csv", sep=";")

# Créer le graphique
ggplot(df, aes(x=type, y=sodium_ppm_M.S, fill=profondeur)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~diametre) +
  labs(title="Quantité de sodium en fonction du traitement pour chaque profondeur et chaque diamètre",
       x="Type de traitement (PS ou PNS)", 
       y="Sodium (ppm)") +
  theme_minimal()

c=ggplot(racine_ps_pfort, aes(x = prfondeur_num , y = sodium_ppm_M.S, size = diametre_mm)) +
  geom_point() +
  scale_color_manual(values = c("ps" = "red", "pns" = "blue")) + 
  scale_y_continuous(limits = c(2300, 11140), breaks = seq(2300, 11140, 1000)) +
  scale_x_continuous(limits = c(20, 70), breaks = seq(20, 70, 10)) +
  labs(
    x = "Profondeur moyenne (cm)",
    y = "Sodium (ppm)",
    size = "Diamètre des racines (mm)"  # Taille des points liée au diamètre des racines
  ) +
  theme_minimal()+
  theme(legend.position = "none")
c
d=ggplot(racine_ps_pfaible, aes(x = prfondeur_num , y = sodium_ppm_M.S, size = diametre_mm)) +
  geom_point() +
  scale_color_manual(values = c("ps" = "red", "pns" = "blue")) + 
  scale_y_continuous(limits = c(2300, 11140), breaks = seq(2300, 11140, 1000)) +
  scale_x_continuous(limits = c(20, 70), breaks = seq(20, 70, 10)) +
  labs(
    x = "Profondeur moyenne (cm)",
    y = "Sodium (ppm)",
    size = "Diamètre des racines (mm)"  # Taille des points liée au diamètre des racines
  ) +
  theme_minimal()+
  theme(legend.position = "none")+
  theme(axis.text.y = element_blank()) +
  theme(axis.title.y = element_blank()) 
  

e=ggplot(racine_ps_mort, aes(x = prfondeur_num , y = sodium_ppm_M.S, size = diametre_mm)) +
  geom_point() +
  scale_color_manual(values = c("ps" = "red", "pns" = "blue")) + 
  scale_y_continuous(limits = c(2300, 11140), breaks = seq(2300, 11140, 1000)) +
  scale_x_continuous(limits = c(20, 70), breaks = seq(20, 70, 10)) +
  labs(
    x = "Profondeur moyenne (cm)",
    y = "Sodium (ppm)",
    size = "Diamètre des racines (mm)"  # Taille des points liée au diamètre des racines
  ) +
  theme_minimal()


combined_plot <- (a | b ) / (c | d)
print(combined_plot)
