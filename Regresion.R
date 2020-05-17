library(ggplot2) #carga la ibreria 
library(caret)
library(dplyr)

setwd("/")
setwd("Users/Anuar Dante/Desktop/encuesta-Seminario-master/")

survey <- read.csv("survey_cleaned.csv", sep = ",", header = T)

prop.table(table(survey$rechazo_en_entrevista))
features <- c(
  "edad", 
  "rango_indice", 
  "nuevas_habilidades_autodidacta",
  "rendimiento_academico", #survey$area_preferida
  #"area_preferida",
  "estudiante_foraneo",
  "trabaja",
  "rechazo_en_entrevista"
)
set <- survey[, names(survey) %in% features ]
set$rechazo_en_entrevista  <- as.factor(set$rechazo_en_entrevista)

model <- glm(rechazo_en_entrevista ~ ., data = set, family = "binomial")
importances <- varImp(model)
importances$colum <- row.names(importances)
importances <- importances %>% arrange(-Overall)
importances

ggplot(set) +
  aes(x= rango_indice, fill=factor(rechazo_en_entrevista))+
  geom_bar(position='fill')+
  theme(axis.text.x = element_text(angle = 45))+
  scale_fill_manual(values=c("#999999","#E69F00"))

ggplot(set) +
  aes(x= estudiante_foraneo, fill=factor(rechazo_en_entrevista))+
  geom_bar(position='fill')+
  theme(axis.text.x = element_text(angle = 45))+
  scale_fill_manual(values=c("#999999","#E69F00"))

ggplot(set) +
  aes(x= trabaja, fill=factor(rechazo_en_entrevista))+
  geom_bar(position='fill')+
  theme(axis.text.x = element_text(angle = 45))+
  scale_fill_manual(values=c("#999999","#E69F00"))

ggplot(set) +
  aes(x= edad, fill=factor(rechazo_en_entrevista))+
  geom_bar(position='stack')+
  theme(axis.text.x = element_text(angle = 45))+
  scale_fill_manual(values=c("#999999","#E69F00"))

ggplot(set) +
  aes(x= rendimiento_academico, fill=factor(rechazo_en_entrevista))+
  geom_bar(position='stack')+
  theme(axis.text.x = element_text(angle = 45))+
  scale_fill_manual(values=c("#999999","#E69F00"))

#"rechazo_en_entrevista"          "edad"  
#"rango_indice"                   "rendimiento_academico"
#"practica_primera_opcion"        "empresa_segun_actividad"