#
# Arreglos post Path analisis

Val2 <- Val

Val2$ValIndex <- foot_plsV$scores[,5]
Val2 <- Val2 %>% mutate(Habitat=ifelse(Ambiente %in% c("PlayaNat", "PlayaInt", "RocaNat", "RocaInt"), "Marino", "Terrestre"))

Val2 <- Val2 %>% mutate(Especie=fct_relevel(Especie, "Columba", "Turdus", "Zonotrichia", "Larus", "Pele", "Phalacrocorax"))
#ggplot(Val2, aes(x = Ambiente, y = ValIndex)) + geom_violin(aes(fill=Habitat)) +geom_jitter()+ facet_wrap(~ Especie)
ggplot(Val2, aes(x = Ambiente, y = ValIndex)) + geom_boxplot(aes(fill=Habitat)) + facet_wrap(~ Especie) + scale_fill_manual(values = c("#263147", "#7C4700")) + geom_hline(yintercept = 0, lty=3) + ylab("Indice de valorización")

