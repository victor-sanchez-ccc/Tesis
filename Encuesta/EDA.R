setwd("/")
setwd("Users/pc/unah/I P 2020/seminario/repositorio-git/encuesta-Seminario/")

column_names <- read.csv("column_names_a_tratar.csv", header = T, sep = ";", encoding = "UTF-8") ##archivo csv con las columnas limpiadas

survey <- read.csv("encuesta_con_respuestas.csv", header = T, sep = ",", encoding = "UTF-8") #archivo de encuesta original
survey <- survey[,!(names(survey) %in% c("Marca.temporal"))]  #eliminando marca temporal

column_names$traduccion <- as.character(column_names$traduccion)  #column_names contiene el csv para tratar las columnas

names(survey) <- column_names$traduccion  #se cambian los nombres de las columnas, por el nombre de las variables asignadas en column_names_a_tratar

head(survey)

write.csv(survey, "survey_cleaned.csv", row.names = F)  #se crea un nuevo csv
