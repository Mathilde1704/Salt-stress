
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


setwd("F:/POST_DOC_SALT'EAU/données/villeroy_2024/analyse_petiolaire_limbe")
petiole_limbe =read.csv("F:/POST_DOC_SALT'EAU/données/villeroy_2024/analyse_petiolaire_limbe/analyse_petiole_limbe.csv", sep=';', dec=".")

ggplot(petiole_limbe, aes(x = organe, y = Na, fill = Parcelle)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Concentration de Na selon l'organe et la parcelle",
       x = "Parcelle",
       y = "Concentration de Na",
       fill = "Organe") +
  theme_minimal()


petiole_limbe <- petiole_limbe %>%
  mutate(Na_scaled = ifelse(organe == "petiole", Na / 2, Na))

ggplot(petiole_limbe, aes(x = organe, y = Na_scaled, fill = Parcelle)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(
    name = "Concentration de Na (limbe)", 
    sec.axis = sec_axis(~ . * 2, name = "Concentration de Na (pétiole)")
  ) +
  labs(title = "Concentration de Na selon l'organe et la parcelle",
       x = "Organe",
       fill = "Parcelle") +
  theme_minimal()+
  scale_fill_manual(values = c("SALE" = "red", "PAS SALE" = "blue")) +
  geom_hline(yintercept = 1500, color = "black", linetype = "dashed", size = 1) 

acide =read.csv("F:/POST_DOC_SALT'EAU/données/villeroy_2024/metabolo_primaire.csv", sep=';', dec=".")


ggplot(acide, aes(x = date, y = metabo_primaire_g.L, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("PNS" = "blue", "PS" = "red")) +
  labs(title = "Concentration de Na selon l'organe et la parcelle",
       x = "Date",
       fill = "Type") +
  theme_minimal()

ggplot(acide, aes(x = date, y = metabol_g.baies, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("PNS" = "blue", "PS" = "red")) +
  labs(title = "Concentration de Na selon l'organe et la parcelle",
       x = "Date",
       fill = "Type") +
  theme_minimal()+
  scale_y_continuous(limits = c(0, 0.4), breaks = seq(0, 0.4, 0.1))
