# Librerías ====
library(tidyverse)


# Lecturas ====
cuestionario <- read.csv("data/raw/cuestionario_de_riesgos_psicosociales.csv") %>% 
  transmute(.data = cuestionario, Marca.temporal = NULL) %>% janitor::clean_names()


# Limpieza ====
#nombres <- c("Plantel", "Sexo", "Edad", "Grado", "Datos_hogar", "Trabaja", 
             #"Seguro_en_escuela", "Auxilio_en_escuela", "Seguro_en_casa", 
             #"Auxilio_en_casa", "Sentimientos", "Interés_profesionales_salud", 
             #"Situación_emocional", "Frecuencia_bienestar", "Temas_emociones", 
             #"Relaciones_sexuales", "Deacuerdo_o_no", "Edad_rel_sex", 
             #"Usaste_condón", "Tienes_ETS", "Síntomas_ETS", "Sexualidad_estrés", 
             #"Temas_sexualidad", "Peleas", "Alcohol", "Novio_novia", 
             #"Tiempo_noviazgo", "Eval_relación", "Daño_físico", "Solicitud_ayuda", 
             #"Consumo_drogas", "Sustancia", "Consumo_problema", "Profesional_adicciones")

#colnames(cuestionario) <- nombres

#Para saber qué Planteles participaron en la encuesta
#factor1 <- factor(cuestionario$Plantel)
#table(factor1)

#Planteles Centro: 7, 11, 12, 19
#CECyTE Plantel No. 07 - Heriberto Kehoe Vincent
#CECyTE Plantel No. 11 - Buenavista
#CECyTE Plantel No. 12 - Miguel Hidalgo
#CECyTE Plantel No. 19 - Villa del Cielo

#planteles_centro <- data.frame(
#  cuestionario = unique(cuestionario$Plantel),
#  Municipio = c("Centro")
#)


# Seleccionar únicamente los planteles del municipio de Centro, que es el municipio que nos
# interesa principalmente
planteles_centro <- cuestionario %>% 
                filter(cuestionario$selecciona_tu_plantel == "CECyTE Plantel No. 07 - Heriberto Kehoe Vincent" 
                       | cuestionario$selecciona_tu_plantel == "CECyTE Plantel No. 11 - Buenavista" 
                       | cuestionario$selecciona_tu_plantel == "CECyTE Plantel No. 12 - Miguel Hidalgo", 
                       .preserve = TRUE)

planteles_centro %>% 
  summarise(across(everything(), ~sum(!is.na(.x)))) %>% 
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "missing") %>% 
  ggplot() + 
  geom_col(aes(y = variable, x = missing))
                


### Resultados

#Resultados de la encuesta realizada y aplicada en conjunto con el 
#Centro de Estudios Científicos y Tecnológicos de Tabasco (CECyTE). 
#En esta etapa del análisis de resultados nos enfocamos en los datos que 
#arrojaron los planteles 07, 11 y 12 que se encuentran dentro del municipio de Centro.

#### ¿Cuántos hombres y mujeres participaron en Centro?

factor2 <- factor(planteles_centro$Sexo)
table(factor2)

#### ¿Cuál es la edad de los participantes?

factor3 <- factor(planteles_centro$Edad)
table(factor3)

