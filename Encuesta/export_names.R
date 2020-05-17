setwd("/")
setwd("Users/pc/unah/I P 2020/seminario/repositorio-git/encuesta-Seminario/")

survey <- read.csv("encuesta_con_respuestas.csv", header = T, sep = ",", encoding = "UTF-8") ##leyendo el doc de la encuesta con sus respuestas

my.names <- names(survey)  ##creando variable my.names para asignar nombres de las columnas de la encuesta

columnas_a_tratar <- my.names[!(my.names %in% c("Marca.temporal"))] ## variable columnas_a_tratar para asignar la encuesta pero quitando marca temporal

df <- data.frame(columna.name = columnas_a_tratar) ## se crea un dataframe

write.csv(df, "column_names.csv", row.names = FALSE) #creamos un archivo csv


