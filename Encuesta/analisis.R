setwd("/")
setwd("Users/pc/unah/I P 2020/seminario/encuesta/")

encuesta <- read.csv("encuesta.csv", header = T, sep = ",", encoding = "UTF-8")

!(names(encuesta) %in% c("X.Cuál.es.su.correo.electrónico...Opcional."))



encuesta <- encuesta[,!(names(encuesta) %in% c("Marca.temporal"))]

columnas_a_tratar <- encuesta

names(encuesta)

##-----------------------------------------------------------------------------------------------------
##-----------------------------------------------------------------------------------------------------


## mtcars

## mtcars$mpg

## names(mtcars)  obtener nombres de columnas

## str(mtcars)  regresa el tipo de dato de todas las columnas

##summary(mtcars)  resumen estadistico 

##sort(mtcars$cyl) retorna las observaciones de esa columna pero en orden

mtcars$cyl <- as.factor(mtcars$cyl)##factor es una columna que tiene niveles se usa cuando son valores discretos
summary(mtcars)


mtcars$gear <- as.factor(mtcars$gear)
