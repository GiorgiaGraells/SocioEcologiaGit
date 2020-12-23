#pruebas regresion logistica

SF <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=4' = qlogis(mean(y >= 4)),
    'Y>=5' = qlogis(mean(y >= 5)),
    'Y>=6' = qlogis(mean(y >= 6)),
    'Y>=7' = qlogis(mean(y >= 7)))
}


(s <- with(PorAmb, summary(as.numeric(Ambiente_bienestar) ~ Ambiente + Distancia_residenciaKm + 
                             Nota_conocimiento + Relacion_MarNCP, fun=SF)))
