#Valoración por especie preliminar

library(tidyverse)

#############
Val <- read_csv("/home/giorgia/Documents/Doctorado tesis/Encuesta/EncuestaAvesRegistro/respuestas_COMPLETO.csv") %>%  select(-contains("_otro")) 
Val <- Val %>% mutate_if(is.character, str_to_upper)

Val <- Val %>% mutate(Relacion_MarNCP= case_when(
  Relacion_mar %in% c("BANARME EN EL MAR", "BANARSE", "BANARSE EN EL MAR", "CAMINANDO ORILLA", "CAMINAR", "TOMAR SOL", "RELAJO CAMINANO PLAYA", 
                      "CAMINAR FOOGRAFIA", "CAMINAR PLAYA", "CONTEMPLAR EL MAR", "CORRER", "DEPORTE", "DESCANSAR", "PASEAR PLAYA", "PASEAR",
                      "DESCANSAR EN LA ARENA", "DESCANSO PLAYA", "ENTRENAMIENTO O DESCANSO PLAYA","TROTAR, BANO PLAYA", "LEER Y SACAR FOTOS",
                      "IR A LA PLAYA,RELAJO", "KAKAY CAMINAR DEPORTE", "LEER Y CONTEMPLAR EL MAR","PASEAR PERROS CAMINAR, MIRAR MAR")~"NO MATERIAL- Exp. físicas y psicológicas",
  Relacion_mar %in% c("COMER EMPANDA" )~"MATERIAL- Alimentación",
  Relacion_mar %in% c("COMPARTIR CON AMIGOS", "JARDINEAR CASA", "LIMPIAR Y CONTEMPLAR", "MIRAREL MAR, RECUERDOS INFANCIA")~"NO MATERIAL- Soporte de identidad",
  Relacion_mar %in% c("DISFRUTA AIRE MARINO", "RESPIRAR")~"REGULACIÓN- Regulación calidad del aire",
  Relacion_mar %in% c("RECOGER CONCHITAS Y CAMINAR", "TRABAJO")~"MATERIAL- Materiales y asistencia",
  Relacion_mar %in% c("ACT NAUTICAS VELA SUP URF", "BUCEO", "NAVEGACION", "VER EL MAR", "TRABAJO Y DEPORTE")~"NO MATERIAL- Aprendizaje e inspiración"))

Val <- Val %>% mutate(Ocu_agrupado= case_when(
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

Val <- Val %>% mutate(AmbOrigen= case_when(
  Sitio %in% c("CASA", "LAS SIRENAS", "PARQUE EL LITRE", "PLAZA RECREO", "PLAZA GABRIELA MISTRAL")~"VERDE",
  Sitio %in% c("LAGHU YOGA", "RENACA CENTRO")~"URBANO",
  Sitio %in% c("LAGUNA VERDE", "PLAYA NEGRA", "HUMEDAL RENACA", "LAS SALINAS")~"PLAYA NATURAL",
  Sitio %in% c("PLAYA AMARILLA", "SECTOR 5", "LOS LILENES")~"PLAYA INTERVENIDA",
  Sitio %in% c("ROCA OCEANICA")~"ROCA NATURAL",
  Sitio %in% c("CLUB YATES", "MUELLE BARON")~"ROCA INTERVENIDA"))

### Vamos a alargar las frecuencias percibidas
Resp<- Val %>% select(ID_encuesta, Relacion_MarNCP, Ocu_agrupado, Encuestador, Sitio, Residencia_costa, Distancia_residenciaKm, Edad, 
                       Genero, Ingresos, Integrantes_familia, Nota_conocimiento, Participacion_avistamiento, AmbOrigen)


####
###################
#union de variables

Avance2 <- full_join(Resp, respBien)  %>% full_join(respAgrado) %>%  full_join(respDesgrado) %>%  full_join(respFrecVis) %>% full_join(respFrecInf) %>%
  full_join(respConoce) %>% full_join(respNombre) %>% full_join(respBenef) %>%   full_join(respFrecPer) %>%  
  full_join(respOrigen) %>% distinct() %>%  dplyr::filter(!is.na(Encuestador))


####
#modificacion variables para analisis
Avance2 <- Avance2 %>% mutate(Residencia_costa= ifelse(Residencia_costa==2 , 0, 1), Distancia_residenciaKm= (1/(Distancia_residenciaKm+1)))

Avance2 <- Avance2 %>% mutate(Frec_infancia= case_when(Frec_infancia == 1 ~ 1,
                                                     Frec_infancia == 2 ~ -1,
                                                     Frec_infancia == 3 ~ 0))

Avance2 <- Avance2 %>% mutate(Genero= case_when(Genero == 1 ~ "Femenino",
                                              Genero == 2 ~ "Masculino"))

Avance2 <- Avance2 %>% mutate(Residencia_costa= case_when(Residencia_costa == 1 ~ "Reside en la costa",
                                                        Residencia_costa == 0 ~ "No reside en la costa"))


Avance2 <- Avance2 %>% mutate(Frec_visita= case_when(Frec_visita ==1 ~1,
                                                   Frec_visita==2 ~ 0.25,
                                                   Frec_visita==3 ~0.04,
                                                   Frec_visita==4 ~ 0.01))

Avance2 <- Avance2 %>% mutate(Ingresos= case_when(Ingresos ==1 ~150000,
                                                Ingresos==2 ~ 425000,
                                                Ingresos==3 ~1000000,
                                                Ingresos==4 ~ 1925000,
                                                Ingresos==5 ~ 3400000))

Avance2 <- Avance2 %>% mutate(Ingreso_percap= Ingresos/Integrantes_familia)


Avance2 <- Avance2 %>% mutate(Participacion_avistamiento= case_when(Participacion_avistamiento == 1 ~ 0,
                                                                  Participacion_avistamiento == 2 ~ 0.25,
                                                                  Participacion_avistamiento == 3 ~ 0.50,
                                                                  Participacion_avistamiento == 4 ~ 1))


Avance2 <- Avance2 %>% mutate(AgradoCH= case_when(
  Agrado %in% 1~"Estética",
  Agrado %in% 2~"Comportamiento",
  Agrado %in% 3~"Familiaridad",
  Agrado %in% 4~"Simbolismo",
  Agrado %in% 5~"Nada le agrada"))

Avance2 <- Avance2 %>% mutate(DesagradoCH= case_when(
  Desagrado %in% 1~"Estética",
  Desagrado %in% 2~"Comportamiento",
  Desagrado %in% 3~"Familiaridad",
  Desagrado %in% 4~"Simbolismo",
  Desagrado %in% 5~"Nada le desagrada"))

Avance2 <-Avance2 %>% mutate(Conoce_nombre= ifelse(Especie=="Larus" & Nombre_especie %in% c("GAVIOTA DOMINICANA", "GAVIOTA"), 1,
                                                   ifelse(Especie=="Columba" & Nombre_especie %in% c("PALOMA", "PALOMA COMUN", "PALOMA URBANA"), 1,
                                                          ifelse(Especie=="Pele" & Nombre_especie %in% c("PELICANO"), 1,       
                                                                 ifelse(Especie=="Phalacrocorax" & Nombre_especie %in% c("CORMORAN", "PATO NEGRO", "CORMORAN YECO", "PATO YECO", "YECO"), 1, 
                                                                        ifelse(Especie=="Turdus" & Nombre_especie %in% c("ZORZAL"), 1,
                                                                               ifelse(Especie=="Zonotrichia" & Nombre_especie %in% c("CHINCOL"), 1,
                                                                                      0)))))))

Avance2 <- Avance2 %>% mutate(Conoce_origen= ifelse(Origen==1 & Especie %in%  c("Columba", "Turdus", "Zonotrichia"), 1,
                                                    ifelse(Origen==2 & Especie %in%  c("Larus","Pele","Phalacrocorax"),1,0)  ))

Avance2 <- Avance2 %>% mutate(Origen= case_when( Origen== 1 ~ "Terrestre",
                                                 Origen == 2 ~ "Marino"))

Avance2 <- Avance2 %>% mutate(Conoce_especie= ifelse(Conoce_especie==2 , 0, 1))


#Arreglar data = en beneficio de ver ave por ambiente para q quede en NA ya q fue una falta de respuesta
#Asi uedan todos los valores de satisfaccion entre 1 y 7

Avance2 %>% dplyr::mutate(Beneficio_sp_amb = ifelse(Beneficio_sp_amb == 0, NA, Beneficio_sp_amb))




saveRDS(Avance2, "DatosAvance2.rds")



##############
Avance2 <- read_rds("DatosAvance2.rds") %>% mutate(Ambiente=fct_relevel(Ambiente, "Urbano", "Verde", "RocaInt", "PlayaInt", "PlayaNat"))
Avance2 <- Avance2 %>% mutate(Especie=fct_relevel(Especie, "Columba", "Zonotrichia", "Turdus", "Larus", "Phalacrocorax"))

#general 600x400

ggplot(Avance2, aes(x=Especie, y=Beneficio_sp_amb))+ geom_boxplot(notch=T) +geom_jitter(aes(color=Especie)) +
  theme_classic()+
  xlab("Especies")+ ylab("Bienestar especies")


ggplot(Avance2, aes(x=Ambiente, y=Beneficio_sp_amb)) + geom_violin() +geom_jitter(aes(color=Especie))+theme_classic()+
  xlab("Ambientes")+ ylab("Bienestar al ver aves")
ggplot(Avance2, aes(x=Ambiente, y=Beneficio_sp_amb)) +geom_jitter(aes(color=Especie))+ geom_boxplot(notch=T) + facet_wrap(~Especie)+theme_classic()+
  xlab("Ambientes")+ ylab("Bienestar especies-ambiente")

ggplot(Avance2, aes(x=Ambiente, y=Beneficio_sp_amb)) + geom_boxplot(notch=T) + facet_wrap(~Especie)+theme_classic()+
  xlab("Ambientes")+ ylab("Bienestar especies-ambiente")
#

ggplot(Avance2, aes(x=FrecPercibida, y=Beneficio_sp_amb))+ geom_smooth(method=lm, se=FALSE,aes(color=Especie))+
  geom_point(aes(color=Especie)) +  xlab("Frecuencia percibida por especie")+ylab("Beneficio percibido por especie") +theme_classic()
ggplot(Avance2, aes(x=FrecPercibida, y=Beneficio_sp_amb))+ geom_smooth(method=lm, se=FALSE,aes(color=Ambiente))+
  geom_point(aes(color=Ambiente))+theme_classic()


ggplot(Avance2, aes(x=FrecPercibida, y=Beneficio_sp_amb))+ geom_smooth(method=lm, se=FALSE,aes(color=Genero))+
  geom_point(aes(color=Genero))+theme_classic()

ggplot(Avance2, aes(x=FrecPercibida, y=Beneficio_sp_amb))+ geom_smooth(method=lm, se=FALSE,aes(color=Residencia_costa))+
  geom_point(aes(color=Residencia_costa))+theme_classic()

ggplot(Avance2, aes(x=FrecPercibida, y=Beneficio_sp_amb))+ geom_smooth(method=lm, se=FALSE,aes(color=Sitio))+
  geom_point(aes(color=Sitio))+theme_classic()



ggplot(Avance2, aes(x=FrecPercibida, y=Beneficio_sp_amb))+ geom_smooth(method=lm, se=FALSE,aes(color=ID_encuesta))+
  geom_point(aes(color=ID_encuesta))+theme_classic()


ggplot(Avance2, aes(x=FrecPercibida, y=Beneficio_sp_amb))+ geom_smooth(method=lm, se=FALSE,aes(color=Relacion_MarNCP))+
  geom_point(aes(color=Relacion_MarNCP))+theme_classic()

ggplot(Avance2, aes(x=FrecPercibida, y=Beneficio_sp_amb))+ geom_smooth(method=lm, se=FALSE,aes(color=Ocu_agrupado))+
  geom_point(aes(color=Ocu_agrupado))+theme_classic()


Avance2 <- Avance2 %>% mutate(Ambiente=fct_relevel(Ambiente, "Urbano", "Verde", "RocaInt", "PlayaInt", "PlayaNat"))
ggplot(Avance2, aes(x=FrecPercibida, y=Beneficio_sp_amb))+ geom_smooth(method=lm, se=FALSE,aes(color=Ambiente))+
  geom_point(aes(color=Ambiente))+theme_classic()+ xlab("Frecuencia percibida")+ylab("Bienestar percibido por especie por ambiente")

ggplot(Avance2, aes(x=FrecPercibida, y=Beneficio_sp_amb))+ geom_smooth(method=lm, se=FALSE,aes(color=Especie))+
  geom_point(aes(color=Especie))+theme_classic()+ xlab("Frecuencia percibida")+ylab("Bienestar percibido por especie por ambiente")


Avance2 <- Avance2 %>% mutate(Ambiente=fct_relevel(Ambiente, "Urbano", "Verde", "RocaInt", "PlayaInt", "PlayaNat"))
ggplot(Avance2, aes(x=Ambiente, y=Beneficio_sp_amb))+ geom_violin() +geom_jitter(aes(color=Especie))+theme_classic()+
  xlab("Ambientes")+theme_classic()

#Conocimiento

ggplot(Avance2, aes(x=Nota_conocimiento, y=Beneficio_sp_amb))+ geom_smooth(method=lm, se=FALSE,aes(color=Especie))+
  geom_point(aes(color=Especie))+theme_classic()+ xlab("Nota autoevaluación de conocimiento")+
  ylab("Beneficio percibido por especie por ambiente")

ggplot(Avance2, aes(x=Participacion_avistamiento, y=Beneficio_sp_amb))+ geom_smooth(method=lm, se=FALSE,aes(color=Especie))+
  geom_point(aes(color=Especie))+theme_classic()



#conocimiento especie

ggplot(Avance2, aes(x=Conoce_nombre, y=Beneficio_sp_amb))+ geom_smooth(method=lm, se=FALSE,aes(color=Especie))+
  geom_point(aes(color=Especie))+theme_classic()+ ylab("Beneficio percibido por especie")+
  xlab("Conoce el nombre de la especie")

ggplot(Avance2, aes(x=Conoce_especie, y=Beneficio_sp_amb))+ geom_smooth(method=lm, se=FALSE,aes(color=Especie))+
  geom_point(aes(color=Especie), alpha=10)+theme_classic()+ ylab("Beneficio percibido por especie")+
  xlab("Conoce a la especie")

Avance2 %>% group_by(Especie, Conoce_especie) %>% summarise(N = n())

 
ggplot(Avance2, aes(x=Conoce_origen, y=Beneficio_sp_amb))+ geom_smooth(method=lm, se=FALSE,aes(color=Especie))+  
  geom_point(aes(color=Especie))+theme_classic()+ ylab("Beneficio percibido por especie")+
  xlab("Conoce origen de la especie")

########################################
#############figuras para cada ave

#EXtraer data de cada ave

Avance2Larus <- Avance2 %>% mutate(Ambiente=fct_relevel(Ambiente, "Urbano", "Verde", "RocaInt", "PlayaInt", "PlayaNat")) %>% 
  dplyr::filter(Especie=="Larus")
Avance2Pele <- Avance2 %>% mutate(Ambiente=fct_relevel(Ambiente, "Urbano", "Verde", "RocaInt", "PlayaInt", "PlayaNat")) %>% 
  dplyr::filter(Especie=="Pele")
Avance2Phalacrocorax <- Avance2 %>% mutate(Ambiente=fct_relevel(Ambiente, "Urbano", "Verde", "RocaInt", "PlayaInt", "PlayaNat")) %>% 
  dplyr::filter(Especie=="Phalacrocorax")

Avance2Columba <- Avance2 %>% mutate(Ambiente=fct_relevel(Ambiente, "Urbano", "Verde", "RocaInt", "PlayaInt", "PlayaNat")) %>% 
  dplyr::filter(Especie=="Columba")
Avance2Turdus <- Avance2 %>% mutate(Ambiente=fct_relevel(Ambiente, "Urbano", "Verde", "RocaInt", "PlayaInt", "PlayaNat")) %>% 
  dplyr::filter(Especie=="Turdus")
Avance2Zonotrichia <- Avance2 %>% mutate(Ambiente=fct_relevel(Ambiente, "Urbano", "Verde", "RocaInt", "PlayaInt", "PlayaNat")) %>% 
  dplyr::filter(Especie=="Zonotrichia")

Avance2Marino <- rbind(Avance2Pele, Avance2Phalacrocorax )
Avance2Terrestre <- rbind(Avance2Turdus, Avance2Zonotrichia)

#figuras por ambiente

ggplot(Avance2Larus, aes(x=FrecPercibida, y=Beneficio_sp_amb))+ geom_smooth(method=lm, se=FALSE,aes(color=Ambiente))+
  geom_point(aes(color=Ambiente)) +  xlab("Frecuencia percibida gaviota")+ylab("Beneficio percibido")+theme_classic()

ggplot(Avance2Columba, aes(x=FrecPercibida, y=Beneficio_sp_amb))+ geom_smooth(method=lm, se=FALSE,aes(color=Ambiente))+
  geom_point(aes(color=Ambiente)) +  xlab("Frecuencia percibida paloma")+ylab("Beneficio percibido")+theme_classic()

ggplot(Avance2Marino, aes(x=FrecPercibida, y=Beneficio_sp_amb))+ geom_smooth(method=lm, se=FALSE,aes(color=Ambiente))+
  geom_point(aes(color=Ambiente)) +  xlab("Frecuencia percibida especies marinas")+ylab("Beneficio percibido por especie")+
  theme_classic()
#ggplot(Avance2Marino, aes(x=FrecPercibida, y=Beneficio_sp_amb))+ geom_smooth(method=lm, se=FALSE,aes(color=Origen))+
#  geom_point(aes(color=Origen)) +  xlab("Frecuencia percibida especies marinas")+ylab("Beneficio percibido")+theme_classic()

ggplot(Avance2Terrestre, aes(x=FrecPercibida, y=Beneficio_sp_amb))+ geom_smooth(method=lm, se=FALSE,aes(color=Ambiente))+
  geom_point(aes(color=Ambiente)) +  xlab("Frecuencia percibida especies terrestres")+ylab("Beneficio percibido ")+theme_classic()

#ggplot(Avance2Terrestre, aes(x=FrecPercibida, y=Beneficio_sp_amb))+ geom_smooth(method=lm, se=FALSE,aes(color=Origen))+
#  geom_point(aes(color=Origen)) +  xlab("Frecuencia percibida especies terrestres")+ylab("Beneficio percibido ")+theme_classic()

  
#facet wrap  (900x500)
  
ggplot(Avance2, aes(x=FrecPercibida, y=Beneficio_sp_amb))+geom_smooth(method=lm, se=FALSE,aes(color=Genero))+geom_point(aes(color=Genero))+ 
    xlab("Frecuencia percibida por especie")+ylab("Beneficio percibido por especie") + facet_wrap(~Especie)+theme_classic()
  
  Avance2 <- Avance2 %>% mutate(Especie=fct_relevel(Especie, "Columba", "Zonotrichia", "Turdus", "Larus", "Phalacrocorax"))
  
  ggplot(Avance2, aes(x=FrecPercibida, y=Beneficio_sp_amb))+geom_smooth(method=lm, se=FALSE,aes(color=Ambiente))+geom_point(aes(color=Ambiente))+ 
    xlab("Frecuencia avistamiento percibida por especie")+ylab("Bienestar percibido por especie") + facet_wrap(~Especie)+theme_classic()
  
  ###conocimiento aves
  Avance2$Conoce_especie <- as.character( Avance2$Conoce_especie)
  Avance2$Conoce_nombre <- as.character( Avance2$Conoce_nombre)
  
    ggplot(Avance2, aes(x=FrecPercibida, y=Beneficio_sp_amb))+geom_smooth(method=lm, se=FALSE,aes(color=Conoce_nombre))+geom_point(aes(color=Conoce_nombre))+ 
    xlab("Frecuencia percibida por especie")+ylab("Beneficio percibido por especie") + facet_wrap(~Especie)+theme_classic()
  
  ggplot(Avance2, aes(x=FrecPercibida, y=Beneficio_sp_amb))+geom_smooth(method=lm, se=FALSE,aes(color=Conoce_especie))+geom_point(aes(color=Conoce_especie))+ 
    xlab("Frecuencia percibida por especie")+ylab("Beneficio percibido por especie") + facet_wrap(~Especie)+theme_classic()
  
 
  
  #####graficos por presencia real de la especie y su detectabilidad
  ggplot(Avance2, aes(x=FrecPercibida, y=Beneficio_sp_amb))+geom_smooth(method=lm, se=FALSE,aes(color=Ambiente))+geom_point(aes(color=Ambiente))+ 
    xlab("Frecuencia percibida por especie")+ylab("Beneficio percibido por especie") + facet_wrap(~Especie)+theme_classic()
  
  