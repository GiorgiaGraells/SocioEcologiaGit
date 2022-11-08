
#Frecuency comparison

#"Observed": Frecuencia percibida por las personas 
#"Percieved": Frecuence obtained from bird abundance 


library(tidyverse)


OccdataPrim <- readRDS("/home/giorgia/Documents/Doctorado tesis/Monitoreo aves/MonitoreoVisualGit/Occdata_regPRIM.rds") %>% 
  select(matches("Columba_livia|Zonotrichia_capensis|Turdus_falcklandii|Larus_dominicanus|Phalacrocorax_brasilianus|Pelecanus_thagus")) %>% 
  rownames_to_column(var = "Site") %>% 
  pivot_longer(-Site, names_to = "Species", values_to = "Presence") %>% 
  mutate(Species = str_remove_all(Species, "[:digit:]")) %>% 
  group_by(Species, Site) %>% 
  summarise(Presence = sum(Presence), n = n()) %>% 
  ungroup() %>% 
  mutate(Frecuency = Presence/n) %>% 
  mutate(Ambiente= case_when(
    Site %in% c("PLAZA LAS SIRENAS", "PARQUE EL LITRE", "PLAZA RECREO", "PLAZA GABRIELA MISTRAL","PARQUE ITALIA","PLAZA LAS MISAS")~"Semi-natural inland",
    Site %in% c("PLAZA ANIBAL PINTO","RENACA CENTRO", "TERMINAL","CENTRO CIVICO","CONGRESO","PASEO DEL ALTO")~"Modified inland",
    Site %in% c("LAGUNA VERDE", "PLAYA NEGRA", "HUMEDAL RENACA", "PLAYA LAS SALINAS","PLAYA LA BOCA","PLAYA SAN MATEO")~"Natural beach",
    Site %in% c("PLAYA AMARILLA", "SECTOR 5", "PLAYA LOS LILENES", "CALETA PORTALES","PLAYA LOS PLACERES","SAN MARTIN")~"Modified beach",
    Site %in% c("ROCA OCEANICA", "SECTOR COSTA BRAVA", "VIRGEN NEGRA","RENACA -2","MIRADOR DE CORDOBA","MIRADOR VIENTO SUR")~"Natural rocky shore",
    Site %in% c("CLUB DE YATES", "MUELLE BARON", "AVENIDA PERU","CALETA EL MEMBRILLO" , "CALETA SAN PEDRO","MARINA RECREO")~"Modified rocky shore")) %>% 
  mutate(Treatment = "Observed") %>% 
  dplyr::select("Species", "Frecuency", "Treatment", "Ambiente")



AvesFrecPerc <- read_rds("DatosAvance2.rds") %>% 
  mutate(Ambiente=fct_relevel(Ambiente, "Urbano", "Verde", "RocaInt", "PlayaInt", "PlayaNat"))

AvesFrecPerc <- AvesFrecPerc %>% dplyr::select(Ambiente,FrecPercibida, Especie) 


AvesFrecPerc <- AvesFrecPerc %>% 
  mutate(Ambiente = case_when(Ambiente == "Urbano" ~ "Modified inland", 
                              Ambiente == "Verde" ~ "Semi-natural inland",
                              Ambiente == "RocaInt" ~ "Modified rocky shore", 
                              Ambiente == "PlayaInt" ~ "Modified beach", 
                              Ambiente == "PlayaNat" ~ "Natural beach",  
                              Ambiente == "RocaNat" ~ "Natural rocky shore"),
         Species = case_when(Especie  == "Columba"  ~ "Columba_livia", 
                             Especie  == "Zonotrichia" ~ "Zonotrichia_capensis", 
                             Especie  ==  "Larus" ~ "Larus_dominicanus",
                             Especie  ==  "Turdus" ~ "Turdus_falcklandii", 
                             Especie ==  "Phalacrocorax" ~ "Phalacrocorax_brasilianus", 
                             Especie ==  "Pele" ~ "Pelecanus_thagus")) %>% 
  mutate(Frecuency = FrecPercibida/7) %>% 
  mutate(Treatment = "Percieved") %>% 
  dplyr::select("Species", "Frecuency", "Treatment", "Ambiente") %>% 
  dplyr::filter(!is.na(Frecuency))


FrecComparison <- bind_rows(OccdataPrim, AvesFrecPerc) 


ggplot(AvesAbundPrim, aes(x = Ambiente, y = Abundance)) + 
  geom_violin() + 
  facet_wrap(~Species, scales = "free_y")

ggplot(FrecComparison, aes(x = Ambiente, y = Frecuency, fill = Treatment)) + 
  geom_boxplot() + 
#  geom_jitter() +
  facet_wrap(~Species)+ theme(axis.text.x = element_text(angle=45, vjust= 1, hjust=1))

TablaFrecComp <- FrecComparison %>% 
  group_by(Species, Treatment, Ambiente) %>% 
  summarise(Frecuency = median(Frecuency)) %>% 
  pivot_wider(names_from = "Treatment", values_from = "Frecuency")


saveRDS(TablaFrecComp, "TablaFrecComp.rds")
