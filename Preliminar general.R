  #Resultados encuestas
  #analisis preliminar general-figuras
  
  library(tidyverse)

  #####################
  DF <- read_csv("/home/giorgia/Documents/Doctorado tesis/SocioEcologiaGit/EncuestaAvesRegistro/respuestas_COMPLETO.csv") %>%  select(-contains("_otro")) 
  
  #### Modificacion variables para analisis
  
  DF <- DF %>% mutate_if(is.character, str_to_upper)
  
  DF <- DF %>% mutate(AmbOrigen= case_when(
            Sitio %in% c("CASA", "LAS SIRENAS", "PARQUE EL LITRE", "PLAZA RECREO", "PLAZA GABRIELA MISTRAL")~"VERDE",
            Sitio %in% c("LAGHU YOGA", "RENACA CENTRO")~"URBANO",
            Sitio %in% c("LAGUNA VERDE", "PLAYA NEGRA", "HUMEDAL RENACA", "LAS SALINAS")~"PLAYA NATURAL",
            Sitio %in% c("PLAYA AMARILLA", "SECTOR 5", "LOS LILENES")~"PLAYA INTERVENIDA",
            Sitio %in% c("ROCA OCEANICA")~"ROCA NATURAL",
            Sitio %in% c("CLUB YATES", "MUELLE BARON")~"ROCA INTERVENIDA"))
  
  DF <- DF %>% mutate(Residencia_costa= ifelse(Residencia_costa==2 , 0, 1), Distancia_residencia2= (1/(Distancia_residenciaKm+1)))
  
  DF <- DF %>% mutate(Genero= case_when(Genero == 1 ~ "Femenino",
                                                Genero == 2 ~ "Masculino"))
  
  DF <- DF %>% mutate(Residencia_costa= case_when(Residencia_costa == 1 ~ "Reside en la costa",
                                                          Residencia_costa == 2 ~ "No reside en la costa"))
  
  DF <- DF %>% mutate(Ingresos= case_when(Ingresos ==1 ~150000,
                                                  Ingresos==2 ~ 425000,
                                                  Ingresos==3 ~1000000,
                                                  Ingresos==4 ~ 1925000,
                                                  Ingresos==5 ~ 3400000))
  
  DF <- DF %>% mutate(Ingreso_percap= Ingresos/Integrantes_familia)
  
  DF <- DF %>% mutate(Participacion_avistamiento= case_when(Participacion_avistamiento == 1 ~ 0,
                                                                    Participacion_avistamiento == 2 ~ 0.25,
                                                                    Participacion_avistamiento == 3 ~ 0.50,
                                                                    Participacion_avistamiento == 4 ~ 1))
  
 # No se a q corresponde esta parte (27 julio 2020)
## 
  DF <- DF %>% mutate(AgradoCH= case_when(
    Agrado %in% 1~"Estética (color, forma, tamaño)",
    Agrado %in% 2~"Comportamiento (canto, vuelo, alimentación)",
    Agrado %in% 3~"Familiaridad (el ave es cercana o acerca a la naturaleza)",
    Agrado %in% 4~"Simbolismo (representa creencia o gatilla recuerdo)",
    Agrado %in% 5~"No hay nada que le agrade"))
  
  DF <- DF %>% mutate(DesagradoCH= case_when(
    Desagrado %in% 1~"Estética (color, forma, tamaño)",
    Desagrado %in% 2~"Comportamiento (ruido, percha, desechos)",
    Desagrado %in% 3~"Familiaridad (intrusiva)",
    Desagrado %in% 4~"Simbolismo (representa algo malo o gatilla malos recuerdos)",
    Desagrado %in% 5~"No hay nada que le desagrade"))
###
    
  DF <- DF %>% mutate(Ocu_agrupado= case_when(
    Ocupacion %in% c("ASEO", "CUIDADOR AUTOS", "ESTACIONAMIENTO CENTRO CONERCIAL", "GUARDIA DE SEGURIDAD",
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
  
  DF <- DF %>% mutate(Relacion_MarNCP= case_when(
    Relacion_mar %in% c("BANARME EN EL MAR", "BANARSE", "BANARSE EN EL MAR", "CAMINANDO ORILLA", "CAMINAR", "TOMAR SOL", "RELAJO CAMINANO PLAYA", 
                        "CAMINAR FOOGRAFIA", "CAMINAR PLAYA", "CONTEMPLAR EL MAR", "CORRER", "DEPORTE", "DESCANSAR", "PASEAR PLAYA", "PASEAR",
                        "DESCANSAR EN LA ARENA", "DESCANSO PLAYA", "ENTRENAMIENTO O DESCANSO PLAYA","TROTAR, BANO PLAYA", "LEER Y SACAR FOTOS",
                        "IR A LA PLAYA,RELAJO", "KAKAY CAMINAR DEPORTE", "LEER Y CONTEMPLAR EL MAR","PASEAR PERROS CAMINAR, MIRAR MAR")~"NO MATERIAL- Exp. físicas y psicológicas",
    Relacion_mar %in% c("COMER EMPANDA" )~"MATERIAL- Alimentación",
    Relacion_mar %in% c("COMPARTIR CON AMIGOS", "JARDINEAR CASA", "LIMPIAR Y CONTEMPLAR", "MIRAREL MAR, RECUERDOS INFANCIA")~"NO MATERIAL- Soporte de identidad",
    Relacion_mar %in% c("DISFRUTA AIRE MARINO", "RESPIRAR")~"REGULACIÓN- Regulación calidad del aire",
    Relacion_mar %in% c("RECOGER CONCHITAS Y CAMINAR", "TRABAJO")~"MATERIAL- Materiales y asistencia",
    Relacion_mar %in% c("ACT NAUTICAS VELA SUP URF", "BUCEO", "NAVEGACION", "VER EL MAR", "TRABAJO Y DEPORTE")~"NO MATERIAL- Aprendizaje e inspiración"))
  
  
  
  saveRDS(DF, "DF_socio.rds")
  
  
##############################
  
DF <- readRDS("DF_socio.rds") 
  
###
  
  DF2 <- DF %>% group_by(Sitio, AmbOrigen) %>% summarise(n = n()) %>% 
    ungroup() %>% mutate(Sitio = fct_reorder(as.factor(Sitio),n, .desc = TRUE))
  

ggplot(DF2, aes(x = fct_reorder(AmbOrigen,n, .desc=TRUE), y = n, group = Sitio)) + geom_col(aes(fill=AmbOrigen), position="dodge", color="black") + 
  geom_text(aes(label = Sitio), size=3.5, position=position_dodge(0.9), angle=90,vjust=0.5, hjust=-0.04 ) + 
  labs(y="Número de encuestas", x="Sitios")  + theme_classic()+ ylim(c(0,17))+theme(legend.position = "none")+ 
  scale_fill_brewer(palette = "Dark2") + theme(text=element_text(size=18,  family="Arial"))

####
DF3 <- DF %>% group_by( AmbOrigen) %>% summarise(n = n()) %>% mutate(AmbOrigen= str_wrap(AmbOrigen, 10))
ggplot(DF3, aes(x = fct_reorder(AmbOrigen,n, .desc=TRUE), y = n)) + geom_col(aes(fill=AmbOrigen), position="dodge", color="black") + 
  labs(y="Número de encuestas", x=" ")  + theme_classic()+ ylim(c(0,15))+theme(legend.position = "none")+ 
  scale_fill_brewer(palette = "Dark2") + theme(text=element_text(size=18,  family="Arial"))+
  theme(axis.text.x = element_text(angle=0, vjust= 1, hjust=0.5))


#####graficos 700x400

#distancia residencia
ggplot(DF) + geom_histogram(aes(Distancia_residenciaKm), color="darkblue", bins = 6)+ labs(y="Número de encuestas", x="Residencia del mar (Km)")  + theme_classic() +
  scale_fill_brewer(palette = "Dark2") + theme(text=element_text(size=18,  family="Arial"))

hist(DF$Distancia_residenciaKm, xlab = "Distancia residencia del mar (Km)", xlim = c(0,200), main=" ", ylab= "Número de encuestas")

#Res<- DF %>% group_by(Distancia_residenciaKm) %>% summarise(N = n())
#ggplot(Res, aes(x = Distancia_residenciaKm, y=N)) + geom_point(aes( Distancia_residenciaKm)) + stat_smooth(aes(Distancia_residenciaKm), method = "lm", color ="black", formula=y ~ x + I(x^2), alpha=0.5)+
#  theme_classic()+ ylab("Número de encuestas")+ xlab("Residencia del mar (Km)")+ scale_fill_brewer(palette="Dark2") + theme(text=element_text(size=18,  family="Arial"))

#Ocupación
DF <- DF %>% mutate(Ocu_agrupado= str_wrap(Ocu_agrupado, 15))
ggplot(DF) + geom_bar(aes(Ocu_agrupado), color="black")+ labs(y="Número de encuestas", x="Ocupación")  + theme_classic() +
  theme(axis.text.x = element_text(angle=0, vjust= 1, hjust=0.5))+ scale_fill_brewer(palette = "Dark2") + 
  theme(text=element_text(size=18,  family="Arial"))


#Ver ingresos percapita
ggplot(DF) + geom_histogram(aes(Ingreso_percap), color="darkred", bins = 6)+ labs(y="Número de encuestas", x="Promedio ingreso per cápita")  + theme_classic() +
  scale_fill_brewer(palette = "Dark2") + theme(text=element_text(size=18,  family="Arial"))

hist(DF$Ingreso_percap, xlab = "Promedio ingreso familiar per cápita", main=" ", ylab= "Número de encuestas")

#nota
hist(DF$Nota_conocimiento, xlab = "Nota conocimiento en aves", main=" ", ylab= "Número de encuestas")
nota <- DF %>% group_by(Nota_conocimiento) %>% summarise(N = n())

#ingresos

ING<- DF %>% group_by(Ingreso_percap) %>% summarise(N = n())

ggplot(ING) + geom_col(aes(x=Ingreso_percap, y=N))+ labs(y="Número de encuestas", x="Ingresos percapita")  + theme_classic() +
  scale_fill_brewer(palette = "Dark2") + theme(text=element_text(size=18,  family="Arial")) +scale_x_continuous(labels = scales::comma)

ggplot(ING, aes(x = Ingreso_percap, y=N)) + geom_point(aes( Ingreso_percap)) + 
  stat_smooth(aes(Ingreso_percap), method = "lm", color ="black", formula=y ~ x + I(x^2), alpha=0.5)+
  theme_classic()+ ylab("Número de encuestas")+ xlab("Ingreso per cápita")+ scale_fill_brewer(palette="Dark2") +
  theme(text=element_text(size=18,  family="Arial"))




#edad
anos<- DF %>% group_by(Edad) %>% summarise(N = n())
ggplot(anos) + geom_histogram(aes(Edad), bins = 10, color ="black")+ xlim(20, 80)+ ylim(0,10) +theme_classic()+
  ylab("Número de encuestados")+ theme(axis.text.x = element_text(angle=0, vjust= 1, hjust=0.5))###### ELEGIDO



ggplot(anos, aes(x = Edad, y=N)) + geom_point(aes(Edad)) + stat_smooth(aes(Edad), method = "lm", color ="black", formula=y ~ x + I(x^2), alpha=0.5)+ 
  theme_classic()+ ylab("Número de encuestas")+ xlab("Edad de encuestados")+ scale_fill_brewer(palette="Dark2") + theme(text=element_text(size=18,  family="Arial"))

ggplot(DF) + geom_histogram(aes(Edad), color="darkgreen", bins = 6)+ labs(y="Número de encuestas", x="Edad")  + theme_classic() +
  scale_fill_brewer(palette = "Dark2") + theme(text=element_text(size=18,  family="Arial"))


#genero
ggplot(DF) + geom_bar(aes(Genero), color ="black")+  ylim(0,27) +theme_classic()+
  ylab("Número de encuestados") + ###### ELEGIDO
xlab("Género")



#relacion con el mar

####relacion con el mar
DF <- DF %>% mutate(Relacion_MarNCP= str_wrap(Relacion_MarNCP, 15))
ggplot(DF) + geom_bar(aes(Relacion_MarNCP), color="black")+ labs(y="Número de encuestas", x="Contribución de la naturaleza a las personas")  + theme_classic() +
  theme(axis.text.x = element_text(angle=0, vjust= 1, hjust=0.5))+ scale_fill_brewer(palette = "Dark2") + 
  theme(text=element_text(size=18,  family="Arial"))

DF_NCP <- DF %>% 
  mutate(Relacion_MarNCP=fct_relevel(Relacion_MarNCP, "NO MATERIAL- Exp. físicas y psicológicas", "NO MATERIAL- Soporte de identidad", "NO MATERIAL- Aprendizaje e inspiración",
                                     "MATERIAL- Materiales y asistencia", "MATERIAL- Alimentación" , "REGULACIÓN- Regulación calidad del aire" )) %>% 
  mutate(Relacion_MarNCP= str_wrap(Relacion_MarNCP, 15)) 

ggplot(DF_NCP) + geom_bar(aes(Relacion_MarNCP), color ="black")+  ylim(0,40) +theme_classic()+
  ylab("Número de encuestados") + ###### ELEGIDO
  xlab("Relación con el mar")+theme(axis.text.x = element_text(angle=0, vjust= 1, hjust=0.5))

#ave favorita
DF <- DF %>%  str(ifelse(Ave_favorita== "NO", "NINGUNA", Ave_favorita))

ggplot(DF) + geom_bar(aes(Ave_favorita), color ="black")+  ylim(0,15) +theme_classic()+
  ylab("Número de encuestados") + ###### ELEGIDO
  xlab("Ave favorita")+
  theme(axis.text.x = element_text(angle=90, vjust= 0.5, hjust=1))


#ingresos

ggplot(DF) + geom_bar(aes(Ingresos))+ labs(y="Número de encuestas", x="Ingresos")  + theme_classic() +
  scale_fill_brewer(palette = "Dark2") + theme(text=element_text(size=18,  family="Arial")) +scale_x_continuous(labels = scales::comma)

ing<- DF %>% group_by(Ingresos) %>% summarise(N = n())
ggplot(ing, aes(x = Ingresos, y=N)) + geom_point(aes(Ingresos)) + 
  stat_smooth(aes(Ingresos), method = "lm", color ="black", formula=y ~ x + I(x^2), alpha=0.5)+ theme_classic()+
  ylab("Number of articles")+ xlab("Ingresos")+ scale_fill_brewer(palette="Dark2") + theme(text=element_text(size=18,  family="Arial"))+
  scale_x_continuous(labels = scales::comma)




