#predicciones para clmm

# 1. PREGUNTA AMBIENTE
# 1.1. Prueba con best model

pred <- function(eta, theta, cat = 1:(length(theta) + 1), inv.link = plogis) {
  Theta <- c(-1000, theta, 1000)
  sapply(cat, function(j) inv.link(Theta[j + 1] - eta) - inv.link(Theta[j] - eta))}



mat2 <- expand.grid(AgradoCH = c(0, BestModel$beta[1], BestModel$beta[2], BestModel$beta[3], BestModel$beta[4]), #5 categorias
                   Ambiente = c(0, BestModel$beta[5], BestModel$beta[6], BestModel$beta[7], BestModel$beta[8], BestModel$beta[9]), #6 categorias
                   Nota_conocimiento = BestModel$beta[10]*(1:7))                                                    #continuo

MatNames <- expand.grid(AgradoCH = PorAmb$AgradoCH %>% unique() %>% sort(),
                        Ambiente = PorAmb$Ambiente %>% unique() %>% sort(),
                        Nota_conocimiento = 1:7,
                        stringsAsFactors = F)

BestModel

grid = mat2
model = BestModel


co <- model$coefficients[1:length(model$y.levels)-1]
pre.mat <- pred(eta=rowSums(grid), theta=co) %>% as.data.frame()

colnames(pre.mat) <- paste0("Prob", 1:7)


pre.mat <- bind_cols(MatNames, pre.mat) %>% 
  pivot_longer(starts_with("Prob"), names_to = "Bienestar", values_to = "Probabilidad") %>% 
  mutate(Bienestar = str_remove_all(Bienestar,"Prob"))


ggplot(pre.mat, aes(x = Nota_conocimiento, y = Probabilidad)) +
  geom_path(aes(color = Bienestar, lty = AgradoCH)) + 
  facet_wrap(~Ambiente) + 
  theme_bw()

ggplot(pre.mat, aes(x = Nota_conocimiento, y = Probabilidad)) +
  geom_path(aes(color = Bienestar)) + 
  facet_grid(AgradoCH~Ambiente) + 
  theme_bw()

pre.mat2 <- pre.mat %>% 
  mutate(Bienestar = ifelse(Bienestar %in% c("1", "2", "3", "4"), "<=4", Bienestar)) %>% 
  group_by(Bienestar, AgradoCH, Ambiente, Nota_conocimiento) %>% 
  summarise(Probabilidad = sum(Probabilidad)) %>% 
  ungroup()

ggplot(pre.mat2, aes(x = Nota_conocimiento, y = Probabilidad)) +
  geom_path(aes(color = Bienestar, lty = AgradoCH)) + 
  facet_wrap(~Ambiente) + 
  theme_bw()

ggplot(pre.mat2, aes(x = Nota_conocimiento, y = Probabilidad)) +
  geom_path(aes(color = Bienestar)) + 
  facet_grid(AgradoCH~Ambiente) + 
  theme_bw()

pre.mat3 <- pre.mat %>% 
  mutate(Bienestar = ifelse(Bienestar %in% c("1", "2", "3", "4", "5"), "<=5", Bienestar)) %>% 
  group_by(Bienestar, AgradoCH, Ambiente, Nota_conocimiento) %>% 
  summarise(Probabilidad = sum(Probabilidad)) %>% 
  ungroup()

ggplot(pre.mat3, aes(x = Nota_conocimiento, y = Probabilidad)) +
  geom_path(aes(color = Bienestar, lty = AgradoCH)) + 
  facet_wrap(~Ambiente) + 
  theme_bw()

ggplot(pre.mat3, aes(x = Nota_conocimiento, y = Probabilidad)) +
  geom_path(aes(color = Bienestar)) + 
  facet_grid(AgradoCH~Ambiente) + 
  theme_bw()


# 1.2. Prueba con promedio de modelos

Coefs <- Promedio$coefficients %>% 
  as.data.frame()

Coefs <- Coefs[1,]

# 1.2.1. Variando conocimiento

mat2conocimiento <- expand.grid(AgradoCH = c(0, Coefs$AgradoCHEstética, Coefs$AgradoCHFamiliaridad, Coefs$`AgradoCHNada le agrada`, Coefs$AgradoCHSimbolismo), 
                    Ambiente = c(0, Coefs$AmbientePlayaNat, Coefs$AmbienteRocaInt, Coefs$AmbienteRocaNat, Coefs$AmbienteUrbano, Coefs$AmbienteVerde),
                    Nota_conocimiento = Coefs$Nota_conocimiento*(1:7),
                    median(PorAmb$Distancia_residenciaKm)*Coefs$Distancia_residenciaKm)

MatNames <- expand.grid(AgradoCH = PorAmb$AgradoCH %>% unique() %>% sort(),
                        Ambiente = PorAmb$Ambiente %>% unique() %>% sort(),
                        Nota_conocimiento = 1:7,
                        Distancia_residenciaKm = median(PorAmb$Distancia_residenciaKm),
                        stringsAsFactors = F)


grid = mat2conocimiento

## Sacar los interceptos
co <- co <- Coefs[,1:6] %>% as.numeric()
pre.mat <- pred(eta=rowSums(grid), theta=co) %>% as.data.frame()

colnames(pre.mat) <- paste0("Prob", 1:7)


pre.mat <- bind_cols(MatNames, pre.mat) %>% 
  pivot_longer(starts_with("Prob"), names_to = "Bienestar", values_to = "Probabilidad") %>% 
  mutate(Bienestar = str_remove_all(Bienestar,"Prob"))

saveRDS(pre.mat, "PredBestMod_BienestarAmb_Conocimiento.rds")

ggplot(pre.mat, aes(x = Nota_conocimiento, y = Probabilidad)) +
  geom_path(aes(color = Bienestar, lty = AgradoCH)) + 
  facet_wrap(~Ambiente) + 
  theme_bw()

g

#plotly::ggplotly(g)

ggplot(pre.mat, aes(x = Nota_conocimiento, y = Probabilidad)) +
  geom_path(aes(color = Bienestar)) + 
  facet_grid(AgradoCH~Ambiente) + 
  theme_bw()

ggplot(pre.mat, aes(x = Nota_conocimiento, y = Probabilidad)) +
  geom_area(aes(fill = Bienestar)) + 
  facet_grid(AgradoCH~Ambiente) + 
  theme_bw()

pre.mat2 <- pre.mat %>% 
  mutate(Bienestar = ifelse(Bienestar %in% c("1", "2", "3", "4"), "<=4", Bienestar)) %>% 
  group_by(Bienestar, AgradoCH, Ambiente, Nota_conocimiento) %>% 
  summarise(Probabilidad = sum(Probabilidad)) %>% 
  ungroup()

ggplot(pre.mat2, aes(x = Nota_conocimiento, y = Probabilidad)) +
  geom_path(aes(color = Bienestar, lty = AgradoCH)) + 
  facet_wrap(~Ambiente) + 
  theme_bw()

ggplot(pre.mat2, aes(x = Nota_conocimiento, y = Probabilidad)) +
  geom_path(aes(color = Bienestar)) + 
  facet_grid(AgradoCH~Ambiente) + 
  theme_bw()

pre.mat3 <- pre.mat %>% 
  mutate(Bienestar = ifelse(Bienestar %in% c("1", "2", "3", "4", "5"), "<=5", Bienestar)) %>% 
  group_by(Bienestar, AgradoCH, Ambiente, Nota_conocimiento) %>% 
  summarise(Probabilidad = sum(Probabilidad)) %>% 
  ungroup()

ggplot(pre.mat3, aes(x = Nota_conocimiento, y = Probabilidad)) +
  geom_path(aes(color = Bienestar, lty = AgradoCH)) + 
  facet_wrap(~Ambiente) + 
  theme_bw()

ggplot(pre.mat3, aes(x = Nota_conocimiento, y = Probabilidad)) +
  geom_path(aes(color = Bienestar)) + 
  facet_grid(AgradoCH~Ambiente) + 
  theme_bw()


# 1.2.2. Varia Distancia

mat2distancia <- expand.grid(AgradoCH = c(0, Coefs$AgradoCHEstética, Coefs$AgradoCHFamiliaridad, Coefs$`AgradoCHNada le agrada`, Coefs$AgradoCHSimbolismo), 
                                Ambiente = c(0, Coefs$AmbientePlayaNat, Coefs$AmbienteRocaInt, Coefs$AmbienteRocaNat, Coefs$AmbienteUrbano, Coefs$AmbienteVerde),
                                Nota_conocimiento = Coefs$Nota_conocimiento*median(PorAmb$Nota_conocimiento),
                             Distancia_residenciaKm = seq(min(PorAmb$Distancia_residenciaKm), max(PorAmb$Distancia_residenciaKm), length.out = 10)*Coefs$Distancia_residenciaKm)

MatNames <- expand.grid(AgradoCH = PorAmb$AgradoCH %>% unique() %>% sort(),
                        Ambiente = PorAmb$Ambiente %>% unique() %>% sort(),
                        Nota_conocimiento = median(PorAmb$Nota_conocimiento),
                        Distancia_residenciaKm = seq(min(PorAmb$Distancia_residenciaKm), max(PorAmb$Distancia_residenciaKm), length.out = 10),
                        stringsAsFactors = F)


grid = mat2distancia


## Sacar los interceptos
co <- co <- Coefs[,1:6] %>% as.numeric()
pre.mat <- pred(eta=rowSums(grid), theta=co) %>% as.data.frame()

colnames(pre.mat) <- paste0("Prob", 1:7)


pre.mat <- bind_cols(MatNames, pre.mat) %>% 
  pivot_longer(starts_with("Prob"), names_to = "Bienestar", values_to = "Probabilidad") %>% 
  mutate(Bienestar = str_remove_all(Bienestar,"Prob"))

saveRDS(pre.mat, "PredBestMod_BienestarAmb_Distancia.rds")


ggplot(pre.mat, aes(x = Distancia_residenciaKm, y = Probabilidad)) +
  geom_path(aes(color = Bienestar, lty = AgradoCH)) + 
  facet_wrap(~Ambiente) + 
  theme_bw()

ggplot(pre.mat, aes(x = Distancia_residenciaKm, y = Probabilidad)) +
  geom_path(aes(color = Bienestar)) + 
  facet_grid(AgradoCH~Ambiente) + 
  theme_bw()

pre.mat2 <- pre.mat %>% 
  mutate(Bienestar = ifelse(Bienestar %in% c("1", "2", "3", "4"), "<=4", Bienestar)) %>% 
  group_by(Bienestar, AgradoCH, Ambiente, Distancia_residenciaKm) %>% 
  summarise(Probabilidad = sum(Probabilidad)) %>% 
  ungroup()

ggplot(pre.mat2, aes(x = Distancia_residenciaKm, y = Probabilidad)) +
  geom_path(aes(color = Bienestar, lty = AgradoCH)) + 
  facet_wrap(~Ambiente) + 
  theme_bw()

ggplot(pre.mat2, aes(x = Distancia_residenciaKm, y = Probabilidad)) +
  geom_path(aes(color = Bienestar)) + 
  facet_grid(AgradoCH~Ambiente) + 
  theme_bw()

pre.mat3 <- pre.mat %>% 
  mutate(Bienestar = ifelse(Bienestar %in% c("1", "2", "3", "4", "5"), "<=5", Bienestar)) %>% 
  group_by(Bienestar, AgradoCH, Ambiente, Distancia_residenciaKm) %>% 
  summarise(Probabilidad = sum(Probabilidad)) %>% 
  ungroup()

ggplot(pre.mat3, aes(x = Distancia_residenciaKm, y = Probabilidad)) +
  geom_path(aes(color = Bienestar, lty = AgradoCH)) + 
  facet_wrap(~Ambiente) + 
  theme_bw() +
  scale_x_log10()

ggplot(pre.mat, aes(x = Distancia_residenciaKm, y = Probabilidad)) +
  geom_path(aes(color = Bienestar)) + 
  facet_grid(AgradoCH~Ambiente) + 
  theme_bw() +
  scale_x_log10()



# 2. PREGUNTA ESPECIES
# 2.1. Prueba con best model

pred <- function(eta, theta, cat = 1:(length(theta) + 1), inv.link = plogis) {
  Theta <- c(-1000, theta, 1000)
  sapply(cat, function(j) inv.link(Theta[j + 1] - eta) - inv.link(Theta[j] - eta))}

mat2 <- expand.grid(Ambiente = c(0, BestModelSp$beta[1], BestModelSp$beta[2], BestModelSp$beta[3], BestModelSp$beta[4],  BestModelSp$beta[5]), #6 categorias
                    Especie = c(0, BestModelSp$beta[6], BestModelSp$beta[7], BestModelSp$beta[8], BestModelSp$beta[9], BestModelSp$beta[10]), #6 categorias
                    FrecPercibida = BestModelSp$beta[11]*(0:7),
                    Ambiente_Especie = c(rep(0, 12), BestModelSp$beta[12:length(BestModelSp$beta)]))                                                    #continuo


Interacciones <- expand.grid(Ambiente = (PorSp$Ambiente %>% unique() %>% sort())[-1],
                             Especie = (PorSp$Especie %>% unique() %>% sort())[-1], stringsAsFactors = F) %>% tidyr::unite(col = Ambiente_Especie, Ambiente:Especie)

Base <- data.frame(Ambiente ="PlayaInt", Especie = PorSp$Especie %>% unique() %>% sort())
Base2 <- data.frame(Ambiente = PorSp$Ambiente %>% unique() %>% sort(), Especie = "Columba")

Base <- bind_rows(Base, Base2) %>% tidyr::unite(col = Ambiente_Especie, Ambiente:Especie)

Interacciones <- bind_rows(Base, Interacciones)

MatNames <- expand.grid(Ambiente = PorSp$Ambiente %>% unique() %>% sort(),
                        Especie = PorSp$Especie %>% unique() %>% sort(),
                        FrecPercibida = 0:7,
                        Ambiente_Especie = Interacciones$Ambiente_Especie,
                        stringsAsFactors = F) %>% separate(Ambiente_Especie, into = c("V1","V2"), remove = F)

Index <- c(1:nrow(MatNames))[MatNames$Ambiente == MatNames$V1 & MatNames$Especie == MatNames$V2]


mat2 <- mat2[Index,]

MatNames <- MatNames[Index,] %>% dplyr::select(-V1, -V2)

BestModelSp

grid = mat2
model = BestModelSp


co <- model$coefficients[1:length(model$y.levels)-1]
pre.mat <- pred(eta=rowSums(grid), theta=co) %>% as.data.frame()

colnames(pre.mat) <- paste0("Prob", 1:7)

##incluir interacciones para q calcen numero de filas
pre.mat <- bind_cols(MatNames, pre.mat) %>% 
  pivot_longer(starts_with("Prob"), names_to = "Bienestar", values_to = "Probabilidad") %>% 
  mutate(Bienestar = str_remove_all(Bienestar,"Prob")) %>% 
  arrange(FrecPercibida)

saveRDS(pre.mat, "PredBestMod_BienestarAmbSp.rds")



ggplot(pre.mat, aes(x = FrecPercibida, y = Probabilidad)) +
  geom_path(aes(color = Bienestar, lty = Especie)) + 
  facet_wrap(~Ambiente) + 
  theme_bw()

ggplot(pre.mat, aes(x = FrecPercibida, y = Probabilidad)) +
  geom_path(aes(color = Bienestar)) + 
  facet_grid(Especie~Ambiente) + 
  theme_bw()
