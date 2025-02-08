#EXPLORACION DATOS PARA MANUSCRITO FRONTIERS MARINE SCIENCE
#POST REVISION 1 SEPT 2023

library(tidyverse)
library(caret)
library(gbm)

























#####
# gbm exploracion
Avance2 <- read_rds("DatosAvance2.rds") %>% mutate(Ambiente=fct_relevel(Ambiente, "Urbano", "Verde", "RocaInt", "PlayaInt", "PlayaNat"))

ParaGBM <- Avance2[complete.cases(Avance2),] %>% dplyr::select(-ID_encuesta)

##todas las variables
set.seed(2023)
trainIndex <- createDataPartition(ParaGBM$Beneficio_sp_amb, p = .8, 
                                  list = FALSE, 
                                  times = 1)

Train <- ParaGBM[trainIndex,]

Test <- ParaGBM[-trainIndex,]


fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)


gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

gbmFit1 <- train(Beneficio_sp_amb ~ ., data = Train, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = T,
                 tuneGrid = gbmGrid)

saveRDS(gbmFit1, "GBMFit.rds")

postResample(pred = predict(gbmFit1$finalModel, Test),
             obs = Test$Beneficio_sp_amb)



#Solo con variables ue considera el manuscrito

Avance2 <- read_rds("DatosAvance2.rds") %>% mutate(Ambiente=fct_relevel(Ambiente, "Urbano", "Verde", "RocaInt", "PlayaInt", "PlayaNat"))
ParaGBM <- Avance2[complete.cases(Avance2),] %>% dplyr::select(-ID_encuesta)

ParaGBM <- ParaGBM %>% dplyr::select(-Encuestador, -Ambiente_bienestar, -Frec_visita, -Frec_infancia, -FrecPercibida) %>%
  janitor::clean_names() %>% mutate_if(is.character, ~make.names(.x))


set.seed(2023)
trainIndex <- createDataPartition(ParaGBM$beneficio_sp_amb, p = .8, 
                                  list = FALSE, 
                                  times = 1)

Train <- ParaGBM[trainIndex,]

Test <- ParaGBM[-trainIndex,]


fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)


gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

gbmFit1 <- train(beneficio_sp_amb ~ edad+ ingresos+ ingreso_percap+ distancia_residencia_km+ nota_conocimiento+ 
                   integrantes_familia+ 
                   ingresos+ origen+ conoce_nombre +ocu_agrupado+ relacion_mar_ncp, data = Train, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = T,
                 tuneGrid = gbmGrid)


