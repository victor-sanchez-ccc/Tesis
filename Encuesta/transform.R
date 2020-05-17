setwd("/")
setwd("Users/Vicktor Sanz/Desktop/Tesis/encuesta-Seminario")

#install.packages("dplyr") #instalamos la libreria dplyr


nrow(survey)

length(names(survey))

survey <- read.csv("survey_cleaned.csv", sep = ",", header = T)

survey_original <- read.csv("survey_cleaned.csv", sep = ",", header = T)

test <- as.data.frame(prop.table(table((survey$area_preferida  ))))

summary(test$Freq)

encuesta.summay <- c()

for( myname in names(survey)){
  
  s <- summary(as.data.frame(prop.table(table(myname))))
  df_temp <- data.frame( 
    column.name=c(myname),
    s$Freq
  )
  encuesta.summay <- rbind(encuesta.summay,df_temp)
}

encuesta.summay


summary(as.data.frame(prop.table(table(survey))))

for( myname in names(survey)){
  
  print(is.na(survey[,myname]))
  
}

library("ggplot2")
ggplot(survey$empresa_segun_actividady) +
  aes(x = empresa_segun_actividad, fill = "Sectores laborales de mayor interes en los estudiantes") +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))


#-----------------------------------columna de practica actualmente---------------------------------------------------

survey$practica_actualmente <- as.factor(survey$practica_actualmente)

table(survey$practica_actualmente) # muestra la frecuencia de los estudiantes

prop.table(table(survey$practica_actualmente)) #muestra porcentajes de las frecuencias

as.data.frame(prop.table(table(survey$practica_actualmente))) # devuelve la info en forma de columna

df_perc_practica_actualmente <- as.data.frame(prop.table(table(survey$practica_actualmente)))#asignamos el resultado 
                                          #de la linea anterior a la variable df

library(dplyr) #carga la ibreria 

df_perc_practica_actualmente <- df_perc_practica_actualmente %>% arrange(-Freq) #ordena los datos en orden descendente 
                                          #con el signo (-) y lo deja guardado en df_perc

boxplot(df_perc_practica_actualmente$Freq)

hist(df_perc_practica_actualmente$Freq)

qqnorm(df_perc_practica_actualmente$Freq)

#-------------------------------------------columna de Practica en primera opcion---------------------------------------

survey$practica_primera_opcion <- as.factor(survey$practica_primera_opcion)

table(survey$practica_primera_opcion)

prop.table(table(survey$practica_primera_opcion))

as.data.frame(prop.table(table(survey$practica_primera_opcion)))

df_perc_primera_opcion <- as.data.frame(prop.table(table(survey$practica_primera_opcion)))

df_perc_primera_opcion <- df_perc_primera_opcion %>% arrange(-Freq)


boxplot(df_perc_primera_opcion$Freq)
hist(df_perc_primera_opcion$Freq)
qqnorm(df_perc_primera_opcion$Freq)


df_perc_primera_opcion[df_perc_primera_opcion$Var1 %in% c("Si"), "estado_estudiante"] <- "Si hace practica en su primera opcion"
df_perc_primera_opcion[df_perc_primera_opcion$Var1 %in% c("No"), "estado_estudiante"] <- "No hace practica en su primera opcion"
df_perc_primera_opcion[is.na(df_perc_primera_opcion$"estado_estudiante"), "estado_estudiante"]<- "no hace practica"

df_perc_primera_opcion <- df_perc_primera_opcion %>% select(Var1,estado_estudiante)

survey <- left_join(survey,df_perc_primera_opcion,by=c("practica_primera_opcion"="Var1"))

survey <- survey[,!(names(survey) %in% c("practica_primera_opcion"))]

#-------------------------------------------columna practica cursando clases--------------------------------------------

survey$practica_cursando_clases <- as.factor(survey$practica_cursando_clases)

table(survey$practica_cursando_clases)

prop.table(table(survey$practica_cursando_clases))

as.data.frame(prop.table(table(survey$practica_cursando_clases)))

df_perc_cursa_clases <- as.data.frame(prop.table(table(survey$practica_cursando_clases)))

df_perc_cursa_clases <- df_perc_cursa_clases %>% arrange(-Freq)


boxplot(df_perc_cursa_clases$Freq)
hist(df_perc_cursa_clases$Freq)
qqnorm(df_perc_cursa_clases$Freq)


df_perc_cursa_clases[df_perc_cursa_clases$Var1 %in% c("Si"), "clases_durante_practica"] <- "Si cursa clases durante practica"
df_perc_cursa_clases[df_perc_cursa_clases$Var1 %in% c("No"), "clases_durante_practica"] <- "No cursa clases durante practica"
df_perc_cursa_clases[is.na(df_perc_cursa_clases$"clases_durante_practica"), "clases_durante_practica"]<- "no hace practica"

df_perc_cursa_clases <- df_perc_cursa_clases %>% select(Var1,clases_durante_practica)

survey <- left_join(survey,df_perc_cursa_clases,by=c("practica_cursando_clases"="Var1"))

survey <- survey[,!(names(survey) %in% c("practica_cursando_clases"))]


#----------------------------------------------columna Rango indice-----------------------------------------------

survey$rango_indice <- as.factor(survey$rango_indice)

table(survey$rango_indice)

prop.table(table(survey$rango_indice))

as.data.frame(prop.table(table(survey$rango_indice)))

df_perc_rango_indice <- as.data.frame(prop.table(table(survey$rango_indice)))

df_perc_rango_indice <- df_perc_rango_indice %>% arrange(-Freq)

boxplot(df_perc_rango_indice$Freq)

hist(df_perc_rango_indice$Freq)

qqnorm(df_perc_rango_indice$Freq)

#----------------------------------------------columna rendimiento academico-------------------------------------

survey$rendimiento_academico <- as.factor(survey$rendimiento_academico) 

table(survey$rendimiento_academico)

prop.table(table(survey$rendimiento_academico))

as.data.frame(prop.table(table(survey$rendimiento_academico)))

df_perc_rendimiento_academico <- as.data.frame(prop.table(table(survey$rendimiento_academico)))

df_perc_rendimiento_academico <- df_perc_rendimiento_academico %>% arrange(-Freq)

boxplot(df_perc_rendimiento_academico$Freq)

hist(df_perc_rendimiento_academico$Freq)

qqnorm(df_perc_rendimiento_academico$Freq)


#----------------------------------------------columna Empresas por Actividad-----------------------------------------------

survey$empresa_segun_actividad  <- as.factor(survey$empresa_segun_actividad )

table(survey$empresa_segun_actividad )

prop.table(table(survey$empresa_segun_actividad ))

as.data.frame(prop.table(table(survey$empresa_segun_actividad )))

df_perc_empresa_segun_actividad  <- as.data.frame(prop.table(table(survey$empresa_segun_actividad )))

df_perc_empresa_segun_actividad  <- df_perc_empresa_segun_actividad  %>% arrange(-Freq)

boxplot(df_perc_empresa_segun_actividad $Freq)

hist(df_perc_empresa_segun_actividad $Freq)

qqnorm(df_perc_empresa_segun_actividad $Freq)


#----------------------------------------------columna Empresas Area preferida-----------------------------------------------

survey$area_preferida   <- as.factor(survey$area_preferida )

table(survey$area_preferida )

prop.table(table(survey$area_preferida  ))

as.data.frame(prop.table(table(survey$area_preferida ))*100)

df_perc_area_preferida   <- as.data.frame(prop.table(table(survey$area_preferida ))*100)

df_perc_area_preferida   <- df_perc_area_preferida  %>% arrange(-Freq)

summary(df_perc_area_preferida)

boxplot(df_perc_area_preferida  $Freq)

hist(df_perc_area_preferida  $Freq)

qqnorm(df_perc_area_preferida  $Freq)


#----------------------------------------------columna Empresas Info sobre la empresa-----------------------------------------------

survey$info_sobre_empresa   <- as.factor(survey$info_sobre_empresa   )

table(survey$info_sobre_empresa )

prop.table(table(survey$info_sobre_empresa  ))

as.data.frame(prop.table(table(survey$area_preferida ))*100)

df_perc_info_sobre_empresa   <- as.data.frame(prop.table(table(survey$info_sobre_empresa ))*100)

df_perc_info_sobre_empresa   <- df_perc_info_sobre_empresa  %>% arrange(-Freq)

boxplot(df_perc_info_sobre_empresa  $Freq)

hist(df_perc_info_sobre_empresa $Freq)

qqnorm(df_perc_info_sobre_empresa  $Freq)

#----------------------------------------------columna importancia de la tecnologia-----------------------------------------------

survey$importancia_uso_tecnologia  <- as.factor(survey$importancia_uso_tecnologia    )

table(survey$importancia_uso_tecnologia  )

prop.table(table(survey$importancia_uso_tecnologia   ))

as.data.frame(prop.table(table(survey$importancia_uso_tecnologia  ))*100)

df_perc_importancia_uso_tecnologia   <- as.data.frame(prop.table(table(survey$importancia_uso_tecnologia  ))*100)

df_perc_importancia_uso_tecnologia    <- df_perc_importancia_uso_tecnologia   %>% arrange(-Freq)

boxplot(df_perc_importancia_uso_tecnologia   $Freq)

hist(df_perc_importancia_uso_tecnologia  $Freq)

qqnorm(df_perc_importancia_uso_tecnologia   $Freq)


#----------------------------------------------columna acceso a ofertas exclusivas-----------------------------------------------

survey$acceso_ofertas  <- as.factor(survey$acceso_ofertas )

table(survey$acceso_ofertas ) # muestra la frecuencia de los estudiantes

prop.table(table(survey$acceso_ofertas )) #muestra porcentajes de las frecuencias

as.data.frame(prop.table(table(survey$acceso_ofertas ))*100) # devuelve la info en forma de columna

df_perc_acceso_ofertas  <- as.data.frame(prop.table(table(survey$acceso_ofertas )))#asignamos el resultado 
#de la linea anterior a la variable df


df_perc_acceso_ofertas  <- df_perc_acceso_ofertas  %>% arrange(-Freq) #ordena los datos en orden descendente 
#con el signo (-) y lo deja guardado en df_perc

boxplot(df_perc_acceso_ofertas $Freq)

hist(df_perc_acceso_ofertas$Freq)


#----------------------------------------------columna  ofertas exclusivas son una ayuda-----------------------------------------------

survey$ofertas_exclusivas_estudiantes  <- as.factor(survey$ofertas_exclusivas_estudiantes )

table(survey$ofertas_exclusivas_estudiantes ) # muestra la frecuencia de los estudiantes

prop.table(table(survey$ofertas_exclusivas_estudiantes )) #muestra porcentajes de las frecuencias

as.data.frame(prop.table(table(survey$ofertas_exclusivas_estudiantes ))*100) # devuelve la info en forma de columna

df_perc_ofertas_exclusivas_estudiantes  <- as.data.frame(prop.table(table(survey$ofertas_exclusivas_estudiantes ))*100)#asignamos el resultado 
#de la linea anterior a la variable df


df_perc_ofertas_exclusivas_estudiantes  <- df_perc_ofertas_exclusivas_estudiantes  %>% arrange(-Freq) #ordena los datos en orden descendente 
#con el signo (-) y lo deja guardado en df_perc

boxplot(df_perc_ofertas_exclusivas_estudiantes $Freq)

hist(df_perc_ofertas_exclusivas_estudiantes$Freq)


#----------------------------------------------columna  ofertas exclusivas son una ayuda-----------------------------------------------

survey$notificacion_oferta  <- as.factor(survey$notificacion_oferta  )

table(survey$notificacion_oferta ) # muestra la frecuencia de los estudiantes

prop.table(table(survey$notificacion_oferta )) #muestra porcentajes de las frecuencias

as.data.frame(prop.table(table(survey$notificacion_oferta ))*100) # devuelve la info en forma de columna

df_perc_notificacion_oferta  <- as.data.frame(prop.table(table(survey$notificacion_oferta ))*100)#asignamos el resultado 
#de la linea anterior a la variable df


df_perc_notificacion_oferta  <- df_perc_notificacion_oferta  %>% arrange(-Freq) #ordena los datos en orden descendente 
#con el signo (-) y lo deja guardado en df_perc

boxplot(df_perc_notificacion_oferta $Freq)

hist(df_perc_notificacion_oferta$Freq)


#----------------------------------------------------------------------------------------------------------------

mis_variables_a_tratar <- survey %>% select(practica_actualmente, estado_estudiante, clases_durante_practica, rango_indice, rendimiento_academico)

#----------------------------------------------------------------------------------------------------------------

as.data.frame(prop.table(table(survey$practica_actualmente)) *100)

#---------------------grafica practica actualmente---------------------

grf1 <- barplot(prop.table(table(survey$practica_actualmente)), las=1, main = "Estudiantes haciendo practica")
text(grf1, c(0,0), round(prop.table(table(survey$practica_actualmente)), 3))

#---------------------grafica de su primera opcion---------------------

grf2 <- barplot(prop.table(table(survey$estado_estudiante)), las=1, main = "estudiantes haciendo practica en su primer opcion")
text(grf2, c(0,0), round(prop.table(table(survey$estado_estudiante)), 3))

#---------------------grafica cursando clases con practica-------------

grf3 <- barplot(prop.table(table(survey$clases_durante_practica)), las=1, main = "estudiantes cursando clases en practica")
text(grf3, c(0,0), round(prop.table(table(survey$clases_durante_practica)), 3))

#---------------------grafica rango de indice--------------------------

grf4 <- barplot(prop.table(table(survey$rango_indice)), las=1, main = "rangos de indice de los estudiantes")
text(grf4, c(0,0), round(prop.table(table(survey$rango_indice)), 3))

#---------------------grafica rendimiento academico--------------------

grf5 <- barplot(prop.table(table(survey$rendimiento_academico)), las=1, main = "rendimiento academico segun estudiantes")
text(grf5, c(0,0), round(prop.table(table(survey$rendimiento_academico)), 3))


#---------------------grafica Empresas por Actividad--------------------

as.data.frame(prop.table(table(survey$empresa_segun_actividad )))

par(mar=c(20,4,4,4))

grf6 <- barplot(prop.table(table(survey$empresa_segun_actividad)), las=3, main = "Sectores laborales de mayor interes en los estudiantes")
text(grf5, c(0,0), round(prop.table(table(survey$empresa_segun_actividad)), 3))

install.packages("ggplot2", dependencies = TRUE)
library("ggplot2")
ggplot(survey$empresa_segun_actividady) +
  aes(x = empresa_segun_actividad, fill = "Sectores laborales de mayor interes en los estudiantes") +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

#---------------------grafica area de interes de estudiantes dentro de la carrera de ingenieria en sistemas--------------------

par(mar=c(20,4,4,4))

grf7 <- barplot(prop.table(table(survey$area_preferida)), las=3, main = "area de interes de estudiantes dentro de la carrera de ingenieria en sistemas")
text(grf5, c(0,0), round(prop.table(table(survey$area_preferida)), 5))


#---------------------grafica area de interes de estudiantes dentro de la carrera de ingenieria en sistemas--------------------

par(mar=c(20,4,4,4))

grf7 <- barplot(prop.table(table(survey$area_preferida)), las=3, main = "areas de interes de estudiantes dentro de la carrera de ingenieria en sistemas")
text(grf5, c(0,0), round(prop.table(table(survey$area_preferida)), 5))

#---------------------grafica area de interes de estudiantes dentro de la carrera de ingenieria en sistemas--------------------

par(mar=c(20,4,4,4))

grf8 <- barplot(prop.table(table(survey$importancia_uso_tecnologia)), las=3, main = "Importancia de la tecnologia en la busqueda de Empleo")
text(grf5, c(0,0), round(prop.table(table(survey$info_sobre_empresa)), 5))

par(mar=c(20,4,4,4))

grf9 <- barplot(prop.table(table(survey$acceso_ofertas)), las=3, main = "Usaría un sitio web o aplicación gratuita, para obtener acceso a ofertas laborales")
text(grf5, c(0,0), round(prop.table(table(survey$acceso_ofertas)), 5))


grf10 <- barplot(prop.table(table(survey$ofertas_exclusivas_estudiantes)), las=3, main = "Tener ofertas exclusivas representaria una ayuda en la obtencion de trabajo")
text(grf5, c(0,0), round(prop.table(table(survey$acceso_ofertas)), 5))

grf11 <- barplot(prop.table(table(survey$notificacion_oferta)), las=3, main = "Estudiantes que les gustaria recibir ofertas laborales por correo")
text(grf5, c(0,0), round(prop.table(table(survey$acceso_ofertas)), 5))
#--------------------------------------------------------------

