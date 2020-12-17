#Bienestar preliminar

library(tidyverse)
library(ggplot2)

Bien <- read_csv("/home/giorgia/Documents/Doctorado tesis/Encuesta/EncuestaAvesRegistro/respuestas_COMPLETO.csv") %>%  select(-contains("_otro")) 
Bien <- Bien %>% mutate_if(is.character, str_to_upper)
  
Bien <- Bien %>% mutate(Relacion_MarNCP= case_when(
    Relacion_mar %in% c("BANARME EN EL MAR", "BANARSE", "BANARSE EN EL MAR", "CAMINANDO ORILLA", "CAMINAR", "TOMAR SOL", "RELAJO CAMINANO PLAYA", 
                        "CAMINAR FOOGRAFIA", "CAMINAR PLAYA", "CONTEMPLAR EL MAR", "CORRER", "DEPORTE", "DESCANSAR", "PASEAR PLAYA", "PASEAR",
                        "DESCANSAR EN LA ARENA", "DESCANSO PLAYA", "ENTRENAMIENTO O DESCANSO PLAYA","TROTAR, BANO PLAYA", "LEER Y SACAR FOTOS",
                        "IR A LA PLAYA,RELAJO", "KAKAY CAMINAR DEPORTE", "LEER Y CONTEMPLAR EL MAR","PASEAR PERROS CAMINAR, MIRAR MAR")~"NO MATERIAL- Exp. fís. y psic.",
    Relacion_mar %in% c("COMER EMPANDA" )~"MATERIAL- Alimentación",
    Relacion_mar %in% c("COMPARTIR CON AMIGOS", "JARDINEAR CASA", "LIMPIAR Y CONTEMPLAR", "MIRAREL MAR, RECUERDOS INFANCIA")~"NO MATERIAL- Sop. identidad",
    Relacion_mar %in% c("DISFRUTA AIRE MARINO", "RESPIRAR")~"REGULACIÓN- Reg. cal. aire",
    Relacion_mar %in% c("RECOGER CONCHITAS Y CAMINAR", "TRABAJO")~"MATERIAL- Materiales y asistencia",
    Relacion_mar %in% c("ACT NAUTICAS VELA SUP URF", "BUCEO", "NAVEGACION", "VER EL MAR", "TRABAJO Y DEPORTE")~"NO MATERIAL- Aprendizaje e insp."))






Bien <- Bien %>% mutate(Ocu_agrupado= case_when(
  Ocupacion %in% c("ASEO", "Cuidador autos", "ESTACIONAMIENTO CENTRO CONERCIAL", "GUARDIA DE SEGURIDAD",
                   "INSTRUCTOR NAUTICO", "INSTRUCTORA BUCEO", "MANIOBRAS NAUTICAS", "MUCAMA", "OPERADOR",
                   "PORTERIA VIGILANCIA", "PROGRAMADOR", "PROMOTORA VENTAS", "RECEPCIONISTA", "TEC MECANICO",
                   "TRABAJADOR", "TRABAJADORA", "TRABJ PUERTO DEP")~"Técnico/ operacional",
  Ocupacion %in% c("ANTROPOLOGA", "BIOLOGA", "BIOLOGA MARINA", "BIOLOGO", "BIOLOGO MARINO", "EDUCADORA PARVULOS",
                   "MEDICO", "PROFESORA", "QUIMICO", "SICOLOGA")~"Profesional",
  Ocupacion %in% c("ADMINISTRADOR","GERENTE PUERTO DEPORTIVO")~"Supervisor/gerente",
  Ocupacion %in% c("DUENA DE CASA", "JUBILADO", "PENSIONADA")~"Inactivo",
  Ocupacion %in% c("ARTESANO","EMPRESARIA", "EMPRESARIO", "INDEPENDIENTE", "NEGOCIO", "PARTICULAR", "PEQUENO EMPRESARIO",
                   "PROFESOR YOGA")~"Independiente",
  Ocupacion %in% c("ESTUDIANTE", "ESTUDIANTE DE ARTE","ESTUDIANTE DE U" )~"Estudiante"))


Bien <- Bien %>% mutate(AmbOrigen= case_when(
  Sitio %in% c("CASA", "LAS SIRENAS", "PARQUE EL LITRE", "PLAZA RECREO", "PLAZA GABRIELA MISTRAL")~"VERDE",
  Sitio %in% c("LAGHU YOGA", "RENACA CENTRO")~"URBANO",
  Sitio %in% c("LAGUNA VERDE", "PLAYA NEGRA", "HUMEDAL RENACA", "LAS SALINAS")~"PLAYA NATURAL",
  Sitio %in% c("PLAYA AMARILLA", "SECTOR 5", "LOS LILENES")~"PLAYA INTERVENIDA",
  Sitio %in% c("ROCA OCEANICA")~"ROCA NATURAL",
  Sitio %in% c("CLUB YATES", "MUELLE BARON")~"ROCA INTERVENIDA"))

### Vamos a alargar las frecuencias percibidas
Resp<- Bien %>% select(ID_encuesta, Relacion_MarNCP, Ocu_agrupado,Encuestador, Sitio, Residencia_costa, Distancia_residenciaKm, Edad, 
                       Genero, Ingresos, Integrantes_familia, Nota_conocimiento, Participacion_avistamiento, AmbOrigen)


#################################################

#Juntando las variables

respBien <- Bien %>% select(ID_encuesta, starts_with("Bienestar")) %>% pivot_longer(cols = starts_with("Bienestar"), names_to = "Bienestar_amb", values_to = "Ambiente_bienestar") %>% dplyr::filter(!is.na(Ambiente_bienestar))  %>% mutate(Ambiente = str_remove_all(Bienestar_amb, "Bienestar_aves_"))  %>% select(-Bienestar_amb) %>% mutate(Ambiente=ifelse(Ambiente=="Verdes", "Verde", Ambiente))
respAgrado <- Bien %>% select(ID_encuesta, starts_with("Agrado_aves")) %>% pivot_longer(cols = starts_with("Agrado_aves"), names_to = "Agrado_aves", values_to = "Agrado") %>% dplyr::filter(!is.na(Agrado))  %>% mutate(Ambiente = str_remove_all(Agrado_aves, "Agrado_aves_"))  %>% select(-Agrado_aves)
respDesgrado <- Bien %>% select(ID_encuesta, starts_with("Desagrado_aves")) %>% pivot_longer(cols = starts_with("Desagrado_aves"), names_to = "Desagrado_aves", values_to = "Desagrado") %>% dplyr::filter(!is.na(Desagrado))  %>% mutate(Ambiente = str_remove_all(Desagrado_aves, "Desagrado_aves_"))  %>% select(-Desagrado_aves)

respFrecVis <- Bien %>% select(ID_encuesta, starts_with("Frec_visita")) %>% pivot_longer(cols = starts_with("Frec_visita"), names_to = "Ambiente_Frec_visita", values_to = "Frec_visita") %>% dplyr::filter(!is.na(Frec_visita))  %>% mutate(Ambiente = str_remove_all(Ambiente_Frec_visita, "Frec_visita_"))  %>% select(-Ambiente_Frec_visita)
respFrecInf <- Bien %>% select(ID_encuesta, starts_with("Frec_infancia")) %>% pivot_longer(cols = starts_with("Frec_infancia"), names_to = "Ambiente_Frec_infancia", values_to = "Frec_infancia") %>% dplyr::filter(!is.na(Frec_infancia))  %>% mutate(Ambiente = str_remove_all(Ambiente_Frec_infancia, "Frec_infancia_"))  %>% select(-Ambiente_Frec_infancia)

###################
#union de variables

Avance <- full_join(Resp, respBien)  %>% full_join(respAgrado) %>%  full_join(respDesgrado) %>% 
   full_join(respFrecVis) %>% full_join(respFrecInf) %>% distinct()

#####################################
#modificacion variables para analisis
Avance <- Avance %>% mutate(Residencia_costa= ifelse(Residencia_costa==2 , 0, 1), Distancia_residenciaKm= (1/(Distancia_residenciaKm+1)))

Avance <- Avance %>% mutate(Frec_infancia= case_when(Frec_infancia == 1 ~ 1,
                                                                 Frec_infancia == 2 ~ -1,
                                                                 Frec_infancia == 3 ~ 0))

Avance <- Avance %>% mutate(Genero= case_when(Genero == 1 ~ "Femenino",
                                                     Genero == 2 ~ "Masculino"))

Avance <- Avance %>% mutate(Residencia_costa= case_when(Residencia_costa == 1 ~ "Reside en la costa",
                                              Residencia_costa == 2 ~ "No reside en la costa"))


Avance <- Avance %>% mutate(Frec_visita= case_when(Frec_visita ==1 ~1,
                                                               Frec_visita==2 ~ 0.25,
                                                               Frec_visita==3 ~0.04,
                                                               Frec_visita==4 ~ 0.01))

Avance <- Avance %>% mutate(Ingresos= case_when(Ingresos ==1 ~150000,
                                                            Ingresos==2 ~ 425000,
                                                            Ingresos==3 ~1000000,
                                                            Ingresos==4 ~ 1925000,
                                                            Ingresos==5 ~ 3400000))

Avance <- Avance %>% mutate(Ingreso_percap= Ingresos/Integrantes_familia)


Avance <- Avance %>% mutate(Participacion_avistamiento= case_when(Participacion_avistamiento == 1 ~ 0,
                                                                              Participacion_avistamiento == 2 ~ 0.25,
                                                                              Participacion_avistamiento == 3 ~ 0.50,
                                                                              Participacion_avistamiento == 4 ~ 1))


Avance <- Avance %>% mutate(AgradoCH= case_when(
  Agrado %in% 1~"Estética",
  Agrado %in% 2~"Comportamiento",
  Agrado %in% 3~"Familiaridad",
  Agrado %in% 4~"Simbolismo",
  Agrado %in% 5~"Nada le agrada",
  Agrado %in% 6~"Otro"
  ))

Avance <- Avance %>% mutate(DesagradoCH= case_when(
  Desagrado %in% 1~"Estética",
  Desagrado %in% 2~"Comportamiento",
  Desagrado %in% 3~"Familiaridad",
  Desagrado %in% 4~"Simbolismo",
  Desagrado %in% 5~"Nada le desagrada"))


saveRDS(Avance, "DatosAvance.rds")

#############################################################
#############################################################
#correr desde aca

Avance <- readRDS("DatosAvance.rds")  %>%dplyr::filter(!is.na(Ambiente_bienestar)) #%>%
  mutate(Ambiente_encuesta= case_when(
  Sitio %in% c("CASA", "LAS SIRENAS", "PARQUE EL LITRE", "PLAZA RECREO", "PLAZA GABRIELA MISTRAL")~"VERDE",
  Sitio %in% c("LAGHU YOGA", "RENACA CENTRO")~"URBANO",
  Sitio %in% c("LAGUNA VERDE", "PLAYA NEGRA", "HUMEDAL RENACA", "LAS SALINAS")~"PLAYA NATURAL",
  Sitio %in% c("PLAYA AMARILLA", "SECTOR 5", "LOS LILENES")~"PLAYA INTERVENIDA",
  Sitio %in% c("ROCA OCEANICA")~"ROCA NATURAL",
  Sitio %in% c("CLUB YATES", "MUELLE BARON")~"ROCA INTERVENIDA"))

# Figuras por ambiente

Avance <- Avance %>% mutate(Ambiente=fct_relevel(Ambiente, "Urbano", "Verde", "RocaInt", "PlayaInt", "PlayaNat"))
ggplot(Avance, aes(x=Ambiente, y=Ambiente_bienestar)) + geom_violin() +geom_jitter(aes(color=Ambiente))+theme_classic()+
  xlab("Ambientes")+ ylab("Bienestar al ver aves")

#figuras con boxplot y 95%IC
# if the notches of two boxes do not overlap, this suggests that the medians are significantly different.
Avance <- Avance %>% mutate(Ambiente=fct_relevel(Ambiente, "Urbano", "RocaInt", "PlayaInt", "Verde","RocaNat"))
ggplot(Avance, aes(x=Ambiente, y=Ambiente_bienestar)) + geom_boxplot(notch=TRUE) +theme_classic()+
  xlab("Ambientes")+ ylab("Bienestar al ver aves") 

#considerando el ambiente donde se realizó la encuesta

Avance <- Avance %>% mutate(AmbOrigenta=fct_relevel(Ambiente_encuesta, "URBANO","PLAYA INTERVENIDA","ROCA INTERVENIDA","VERDE", "PLAYA NATURAL")) %>% 
  mutate(AmbOrigen=fct_relevel(AmbOrigenta, "URBANO","PLAYA INTERVENIDA","ROCA INTERVENIDA","VERDE", "PLAYA NATURAL"))

ggplot(Avance, aes(x=Ambiente, y=Ambiente_bienestar)) + geom_violin() +geom_jitter(aes(color=AmbOrigen))+theme_classic()+
  xlab("Ambiente preguntado")+ ylab("Bienestar al ver aves")+ facet_wrap(~AmbOrigen)+theme(axis.text.x = element_text(angle=45, vjust= 1, hjust=1))

ggplot(Avance, aes(x=Ambiente, y=Ambiente_bienestar)) + geom_boxplot() +theme_classic()+
  xlab("Ambiente preguntado")+ ylab("Bienestar al ver aves")+ facet_wrap(~AmbOrigen)+theme(axis.text.x = element_text(angle=45, vjust= 1, hjust=1))

 
# el mejor grafico

Avance <- Avance %>%  mutate(Ambiente=fct_relevel(Ambiente, "Urbano", "RocaInt", "PlayaInt", "Verde","RocaNat")) %>% 
  mutate(AmbOrigen=fct_relevel(AmbOrigen, "URBANO","ROCA INTERVENIDA","PLAYA INTERVENIDA","VERDE", "ROCA NATURAL"))
 
ggplot(Avance, aes(x=Ambiente, y=Ambiente_bienestar)) + geom_boxplot() + theme_classic()+
  xlab("Ambiente consultado")+ ylab("Bienestar al ver aves")+ 
  facet_wrap(~AmbOrigen)+theme(axis.text.x = element_text(angle=45, vjust= 1, hjust=1))



##################

#Bienestar vs frecuencia de visita 

#por ambiente preguntado
ggplot(Avance, aes(x=Frec_visita, y=Ambiente_bienestar))+ geom_smooth(method=lm, se=FALSE,aes(color=Ambiente)) +
  geom_point(aes(color=Ambiente))+ xlab("Frecuencia de visita") +ylab("Bienestar al ver aves")+ theme_classic()
#las personas que suelen visitar mas un ambiente suelen percibir un mayor bienestar al visitarlo
#mientras mas natural el ambiente, las personas perciben alto bienestar aun cuando no lo visiten con mucha frecuencia

#considerando donde se realizo la encuesta
ggplot(Avance, aes(x=Frec_visita, y=Ambiente_bienestar))+ geom_smooth(method=lm, se=FALSE,aes(color=Ambiente)) +
  geom_point(aes(color=Ambiente))+ xlab("Frecuencia de visita") +ylab("Bienestar al ver aves")+ facet_wrap(~AmbOrigen)+
  theme_classic()

#Bienestar vs frecuencia de visita , considerando donde se realizo la encuesta
ggplot(Avance, aes(x=AmbOrigenta, y=Frec_visita))+  geom_violin() +geom_jitter(aes(color=Ambiente))+
 xlab("Ambientes") +ylab("Frecuencia de visita")+ facet_wrap(~Ambiente_bienestar)+
  theme_classic()+theme(axis.text.x = element_text(angle=45, vjust= 1, hjust=1))

ggplot(Avance, aes(x=Ambiente, y=Ambiente_bienestar)) + geom_violin() +geom_jitter(aes(color=Frec_visita))+theme_classic()+
  xlab("Ambiente")+ ylab("Bienestar al ver aves")+ facet_wrap(~Frec_visita,4)+theme(axis.text.x = element_text(angle=45, vjust= 1, hjust=1))

#considerando amb preuntado y donde se realizó la encuesta
ggplot(Avance, aes(x=Ambiente, y=Ambiente_bienestar)) + geom_violin() +geom_jitter(aes(color=AmbOrigenta))+theme_classic()+
  xlab("Ambiente")+ ylab("Bienestar al ver aves")+ facet_wrap(~Frec_visita,4)+theme(axis.text.x = element_text(angle=45, vjust= 1, hjust=1))

#considerando amb preuntado y donde se realizó la encuesta
ggplot(Avance, aes(x=Ambiente, y=Ambiente_bienestar)) + geom_violin() +geom_jitter(aes(color=Frec_visita))+theme_classic()+
  xlab("Ambiente")+ ylab("Bienestar al ver aves")+ facet_wrap(~AmbOrigenta)+theme(axis.text.x = element_text(angle=45, vjust= 1, hjust=1))



#con logit
#ggplot(Avance, aes(x=Frec_visita, y= log((Ambiente_bienestar-1)/(6-(Ambiente_bienestar-1)))) )+ geom_smooth(method=lm, se=FALSE,aes(color=Ambiente)) +
#   geom_point(aes(color=Ambiente))+ xlab("Frecuencia de visita") +ylab("Bienestar al ver aves")

 ################## 
#Bienestar vs distancia residencia

ggplot(Avance, aes(x=Distancia_residenciaKm, y=Ambiente_bienestar)) + geom_smooth(method=lm, se=FALSE,aes(color=Ambiente))+
  geom_point(aes(color=Ambiente))+ xlab("Distancia residencia del mar 1/(Km+1)") +ylab("Bienestar al ver aves")+ theme_classic()
# roca nat y playa nat son similares y siempre  valorados
# mientras mas cerca de la costa vivan las personas mas valoran area verde, roca int y  playa int
# mientras mas cercano a la costa vivan las personas menos valoran el ver aves en zonas urbanas


#Bienestar vs relacion con el marNCP
Avance <- Avance %>% mutate(Relacion_MarNCP=fct_relevel(Relacion_MarNCP, "NO MATERIAL- Exp. fís. y psic.",  "MATERIAL- Materiales y asistencia",  
                                                        "NO MATERIAL- Sop. identidad","NO MATERIAL- Aprendizaje e insp.",
                                                         "REGULACIÓN- Reg. cal. aire", "MATERIAL- Alimentación"))
ggplot(Avance, aes(x=Relacion_MarNCP, y=Ambiente_bienestar))+ geom_violin() +geom_jitter(aes(color=Ambiente))+
  xlab("Relación con el mar") +ylab("Bienestar al ver aves")+ theme_classic()+theme(axis.text.x = element_text(angle=70, vjust= 1, hjust=1))
  

ggplot(Avance, aes(x=Ambiente, y=Ambiente_bienestar)) + geom_violin() +geom_jitter(aes(color=Relacion_MarNCP))+
  theme_classic()+ xlab("Ambientes")+ ylab("Bienestar al ver aves")+theme(axis.text.x = element_text(angle=60, vjust= 1, hjust=1))
Avance <- Avance %>% mutate(Relacion_MarNCP=fct_relevel(Relacion_MarNCP, "MATERIAL- Materiales y asistencia","MATERIAL- Alimentación", "REGULACIÓN- Reg. cal. aire",
                                                        "NO MATERIAL- Exp. fís. y psic.", "NO MATERIAL- Sop. identidad","NO MATERIAL- Aprendizaje e insp."))
ggplot(Avance, aes(x=Ambiente, y=Ambiente_bienestar)) + geom_boxplot()+facet_wrap(~Relacion_MarNCP)+
  theme_classic()+ xlab("Ambientes")+ ylab("Bienestar al ver aves")+theme(axis.text.x = element_text(angle=60, vjust= 1, hjust=1))



#Relacion con el mar y contribución de las aves por ambiente

ggplot(Avance, aes(x=Ambiente, y=Ambiente_bienestar)) + geom_violin() +geom_jitter(aes(color=AgradoCH))+
  theme_classic()+ xlab("Ambientes")+ ylab("Bienestar al ver aves")+theme(axis.text.x = element_text(angle=60, vjust= 1, hjust=1))

Avance2 <-Avance  %>% dplyr::filter(AgradoCH!="Nada le agrada")
ggplot(Avance2, aes(x=Ambiente, y=Ambiente_bienestar)) + geom_violin() +geom_jitter(aes(color=AgradoCH))+
  theme_classic()+ xlab("Ambientes")+ ylab("Bienestar al ver aves")+theme(axis.text.x = element_text(angle=60, vjust= 1, hjust=1))


ggplot(Avance, aes(x=Ambiente, y=Ambiente_bienestar)) + geom_violin() +geom_jitter(aes(color=DesagradoCH))+
  theme_classic()+ xlab("Ambientes")+ ylab("Bienestar al ver aves")+theme(axis.text.x = element_text(angle=60, vjust= 1, hjust=1))

Avance2 <-Avance  %>% dplyr::filter(DesagradoCH!="Nada le desagrada")
ggplot(Avance2, aes(x=Ambiente, y=Ambiente_bienestar)) + geom_violin() +geom_jitter(aes(color=DesagradoCH))+
  theme_classic()+ xlab("Ambientes")+ ylab("Bienestar al ver aves")+theme(axis.text.x = element_text(angle=60, vjust= 1, hjust=1))

