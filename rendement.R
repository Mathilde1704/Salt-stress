library(ggplot2)
library(readr)
library(dplyr)
library(scales)
library(agricolae)
library(multcompView)
library(emmeans)


rendement = read.csv("F:/POST_DOC_SALT'EAU/données/villeroy_2024/rendements.csv", sep=";")

rameaux <- aggregate(rameaux ~ traitement, rendement, mean)

test<-lm(rameaux ~ traitement  ,data=rendement)
anova(test)
summary(test)

ggplot(rendement, aes(x = traitement, y = rameaux, color = traitement, group = traitement)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("blue", "red")) 
  




grappe <- aggregate(grappe ~ traitement, rendement, mean)

test<-lm(grappe ~ traitement  ,data=rendement)
anova(test)
summary(test)


ggplot(rendement, aes(x = traitement, y = grappe, color = traitement, group = traitement)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("blue", "red")) 

ggplot(rendement, aes(x = traitement, y = grappe, color = traitement, group = traitement)) +
  geom_boxplot(outlier.shape = NA) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("blue", "red"))+
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, 2)) 
  

ggplot(rendement, aes(x = traitement, y = nb_grappe_tige, color = traitement, group = traitement)) +
  geom_boxplot(outlier.shape = NA) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("blue", "red"))+
  scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1.0, 0.1)) 



ggplot(rendement, aes(x = traitement, y = pourcentgae_mortalite, fill = traitement, group = traitement)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("blue", "red"))+
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 10)) 

tronc = read.csv("F:/POST_DOC_SALT'EAU/données/villeroy_2024/tronc/tronc.csv", sep=";")


ggplot(tronc, aes(x = traitement, y = diametre_mm, color = traitement, group = traitement)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("blue", "red"))
  #scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, 2)) 

tronc %>%
  group_by(traitement) %>%
  summarise(mediane = median(diametre_mm, na.rm = TRUE))


test<-lm(diametre_mm ~ traitement  ,data=tronc)
anova(test)
summary(test)
