#Calculo de poder para Ordered Logistic Regression

library(tidyverse)
library(Hmisc)
library(MASS)


PorAmb <- readRDS("DatosAvance.rds")  %>%dplyr::filter(!is.na(Ambiente_bienestar))%>%
  mutate(Ambiente_encuesta= case_when(
    Sitio %in% c("CASA", "LAS SIRENAS", "PARQUE EL LITRE", "PLAZA RECREO", "PLAZA GABRIELA MISTRAL")~"VERDE",
    Sitio %in% c("LAGHU YOGA", "RENACA CENTRO")~"URBANO",
    Sitio %in% c("LAGUNA VERDE", "PLAYA NEGRA", "HUMEDAL RENACA", "LAS SALINAS")~"PLAYA NATURAL",
    Sitio %in% c("PLAYA AMARILLA", "SECTOR 5", "LOS LILENES")~"PLAYA INTERVENIDA",
    Sitio %in% c("ROCA OCEANICA")~"ROCA NATURAL",
    Sitio %in% c("CLUB YATES", "MUELLE BARON")~"ROCA INTERVENIDA")) %>% 
  mutate(Ambiente=fct_relevel(Ambiente, "Urbano", "Verde", "RocaInt", "PlayaInt", "PlayaNat"))

#Amb.plr <- polr(Ambiente_bienestar ~Relacion_MarNCP + Distancia_residenciaKm + Nota_conocimiento + Ambiente + AgradoCH ,  data = PorAmb)


#Probabilidad por grupos valores respuesta
Prbs <- table(PorAmb$Ambiente_bienestar)
Prbs <- Prbs / sum(Prbs)
Prbs <- as.numeric(Prbs)


Resp <- factor(replicate(1000, sample(1:7, 1, prob = Prbs)),
               ordered = TRUE, levels = 1:7)


#Modelo nulo

alpha <- polr(Resp ~ 1)$zeta


logit <- function(p) log(1/(1/p-1))
expit <- function(x) 1/(1/exp(x)+1)
