#PREPARACION DATOS MATRIZ 

library(tidyverse)
resp<- read_csv("/home/giorgia/Documents/Doctorado tesis/Encuesta/EncuestaAvesRegistro/respuestas_COMPLETO.csv")

### Vamos a alargar las frecuencias percibidas
DatosListos <- resp %>% select(ID_encuesta, Encuestador, Sitio, Residencia_costa, Distancia_residenciaKm, Edad, Genero, Ingresos, Integrantes_familia,Ocupacion, 
                               Nota_conocimiento, Participacion_avistamiento)
#################################################
#################################################
#arreglo para hipotesis 1: bienestar por ambiente
#################################################

#Por ambiente
respBien <- resp %>% select(ID_encuesta, starts_with("Bienestar")) %>% pivot_longer(cols = starts_with("Bienestar"), names_to = "Bienestar_amb", values_to = "Ambiente_bienestar") %>% dplyr::filter(!is.na(Ambiente_bienestar))  %>% mutate(Ambiente = str_remove_all(Bienestar_amb, "Bienestar_aves_"))  %>% select(-Bienestar_amb) %>% mutate(Ambiente=ifelse(Ambiente=="Verdes", "Verde", Ambiente))
respAgrado <- resp %>% select(ID_encuesta, starts_with("Agrado_aves")) %>% pivot_longer(cols = starts_with("Agrado_aves"), names_to = "Agrado_aves", values_to = "Agrado") %>% dplyr::filter(!is.na(Agrado))  %>% mutate(Ambiente = str_remove_all(Agrado_aves, "Agrado_aves_"))  %>% select(-Agrado_aves)
respDesgrado <- resp %>% select(ID_encuesta, starts_with("Desagrado_aves")) %>% pivot_longer(cols = starts_with("Desagrado_aves"), names_to = "Desagrado_aves", values_to = "Desagrado") %>% dplyr::filter(!is.na(Desagrado))  %>% mutate(Ambiente = str_remove_all(Desagrado_aves, "Desagrado_aves_"))  %>% select(-Desagrado_aves)

respFrecVis <- resp %>% select(ID_encuesta, starts_with("Frec_visita")) %>% pivot_longer(cols = starts_with("Frec_visita"), names_to = "Ambiente_Frec_visita", values_to = "Frec_visita") %>% dplyr::filter(!is.na(Frec_visita))  %>% mutate(Ambiente = str_remove_all(Ambiente_Frec_visita, "Frec_visita_"))  %>% select(-Ambiente_Frec_visita)
respFrecInf <- resp %>% select(ID_encuesta, starts_with("Frec_infancia")) %>% pivot_longer(cols = starts_with("Frec_infancia"), names_to = "Ambiente_Frec_infancia", values_to = "Frec_infancia") %>% dplyr::filter(!is.na(Frec_infancia))  %>% mutate(Ambiente = str_remove_all(Ambiente_Frec_infancia, "Frec_infancia_"))  %>% select(-Ambiente_Frec_infancia)

#por especie
respConoce <- resp %>% select(ID_encuesta, starts_with("Conoce")) %>% pivot_longer(cols = starts_with("Conoce"), names_to = "Especie_conoce", values_to = "Conoce_especie") %>% dplyr::filter(!is.na(Conoce_especie))  %>% mutate(Especie = str_remove_all(Especie_conoce, "Conoce_"))  %>% select(-Especie_conoce)
respNombre <- resp %>% select(ID_encuesta, starts_with("Nombre")) %>% pivot_longer(cols = starts_with("Nombre"), names_to = "Especie_nombre", values_to = "Nombre_especie")%>% dplyr::filter(!is.na(Nombre_especie)) %>% mutate(Especie = str_remove_all(Especie_nombre, "Nombre_"))  %>% select(-Especie_nombre)

#por especie y ambiente

#origen por especie
respOrigen <- resp %>% select(ID_encuesta, starts_with("Origen")) %>% pivot_longer(cols = starts_with("Origen"), names_to = "Especie_origen", values_to = "Origen")  %>% dplyr::filter(!is.na(Origen))  %>% mutate(Especie = str_remove_all(Especie_origen, "Origen_"))  %>% select(-Especie_origen) 

###################
#union de variables

RespEnc_Bien <- full_join(DatosListos, respBien)  %>% full_join(respAgrado) %>%  full_join(respDesgrado) %>% 
  full_join(respConoce) %>% full_join(respNombre) %>% full_join(respOrigen) %>% 
                full_join(respFrecVis) %>% full_join(respFrecInf) %>% distinct()

#####################################
#modificacion variables para analisis
RespEnc_Bien <- RespEnc_Bien %>% mutate(Residencia_costa= ifelse(Residencia_costa==2 , 0, 1), Distancia_residenciaKm= (1/(Distancia_residenciaKm+1)))

RespEnc_Bien <- RespEnc_Bien %>% mutate(Frec_infancia= case_when(Frec_infancia == 1 ~ 1,
                                                                              Frec_infancia == 2 ~ -1,
                                                                              Frec_infancia == 3 ~ 0))
RespEnc_Bien <- RespEnc_Bien %>% mutate(Frec_visita= case_when(Frec_visita ==1 ~1,
                                                            Frec_visita==2 ~ 0.25,
                                                            Frec_visita==3 ~0.04,
                                                            Frec_visita==4 ~ 0.01))

RespEnc_Bien <- RespEnc_Bien %>% mutate(Ingresos= case_when(Ingresos ==1 ~150000,
                                                          Ingresos==2 ~ 425000,
                                                          Ingresos==3 ~1000000,
                                                          Ingresos==4 ~ 1925000,
                                                          Ingresos==5 ~ 3400000))

RespEnc_Bien <- RespEnc_Bien %>% mutate(Ingreso_percap= Ingresos/Integrantes_familia)


RespEnc_Bien <- RespEnc_Bien %>% mutate(Participacion_avistamiento= case_when(Participacion_avistamiento == 1 ~ 0,
                                                                              Participacion_avistamiento == 2 ~ 0.25,
                                                                              Participacion_avistamiento == 3 ~ 0.50,
                                                                              Participacion_avistamiento == 4 ~ 1))

RespEnc_Bien <-RespEnc_Bien %>% mutate(Conoce_nombre= ifelse(Especie=="Larus" & Nombre_especie %in% c("GAVIOTA DOMINICANA", "GAVIOTA"), 1,
                                                           ifelse(Especie=="Columba" & Nombre_especie %in% c("PALOMA", "PALOMA COMUN", "PALOMA URBANA"), 1,
                                                                  ifelse(Especie=="Pele" & Nombre_especie %in% c("PELICANO"), 1,       
                                                                         ifelse(Especie=="Phalacrocorax" & Nombre_especie %in% c("CORMORAN", "PATO NEGRO", "CORMORAN YECO", "PATO YECO", "YECO"), 1, 
                                                                                ifelse(Especie=="Turdus" & Nombre_especie %in% c("ZORZAL"), 1,
                                                                                       ifelse(Especie=="Zonotrichia" & Nombre_especie %in% c("CHINCOL"), 1,
                                                                                              0)))))))

RespEnc_Bien <-RespEnc_Bien  %>% mutate(Conoce_origen= ifelse(Origen==1 & Especie %in%  c("Columba", "Turdus", "Zonotrichia"), 1,
                                                            ifelse(Origen==2 & Especie %in%  c("Larus","Pele","Phalacrocorax"),1,0)  ))

RespEnc_Bien <-RespEnc_Bien  %>% mutate(Conoce_especie= ifelse(Conoce_especie==2 , 0, 1))



write_csv(RespEnc_Bien, "RespEnc_Bien.csv")

###################################################
###################################################
#arreglo para hipotesis 2: valorizacion de especies
###################################################

#por especie
respConoce <- resp %>% select(ID_encuesta, starts_with("Conoce")) %>% pivot_longer(cols = starts_with("Conoce"), names_to = "Especie_conoce", values_to = "Conoce_especie") %>% dplyr::filter(!is.na(Conoce_especie))  %>% mutate(Especie = str_remove_all(Especie_conoce, "Conoce_"))  %>% select(-Especie_conoce)
respNombre <- resp %>% select(ID_encuesta, starts_with("Nombre")) %>% pivot_longer(cols = starts_with("Nombre"), names_to = "Especie_nombre", values_to = "Nombre_especie")%>% dplyr::filter(!is.na(Nombre_especie)) %>% mutate(Especie = str_remove_all(Especie_nombre, "Nombre_"))  %>% select(-Especie_nombre)

#por especie y ambiente
respBenef <- resp %>% select(ID_encuesta, starts_with("Benef")) %>% pivot_longer(cols = starts_with("Benef"), names_to = "Especie_ambiente_benef", values_to = "Beneficio_amb")  %>% dplyr::filter(!is.na(Beneficio_amb))      %>%      mutate(Especie = str_split(string =Especie_ambiente_benef, pattern = "_", simplify = T, n = 3)[,2], Ambiente = str_split(string =Especie_ambiente_benef, pattern = "_", simplify = T, n = 3)[,3]) %>% select(-Especie_ambiente_benef)
respFrecPer <- resp %>% select(ID_encuesta, starts_with("FrecPercib")) %>% pivot_longer(cols = starts_with("FrecPercib"), names_to = "Especie_Ambiente_Frec", values_to = "FrecPercibida") %>% dplyr::filter(!is.na(FrecPercibida)) %>% mutate(Especie = str_split(string =Especie_Ambiente_Frec, pattern = "_", simplify = T, n = 3)[,2],  Ambiente = str_split(string =Especie_Ambiente_Frec, pattern = "_", simplify = T, n = 3)[,3]) %>% select(-Especie_Ambiente_Frec)

#origen por especie
respOrigen <- resp %>% select(ID_encuesta, starts_with("Origen")) %>% pivot_longer(cols = starts_with("Origen"), names_to = "Especie_origen", values_to = "Origen")  %>% dplyr::filter(!is.na(Origen))  %>% mutate(Especie = str_remove_all(Especie_origen, "Origen_"))  %>% select(-Especie_origen) 

####################
#union de variables:

RespEnc_Val <- full_join(DatosListos,respConoce) %>% full_join(respNombre) %>% full_join(respBenef) %>% 
            full_join(respFrecPer) %>%  full_join(respOrigen) %>%   distinct() %>% dplyr::filter(!is.na(Encuestador))


#####################################
#modificacion variables para analisis

RespEnc_Val <-RespEnc_Val %>% mutate(Conoce_nombre= ifelse(Especie=="Larus" & Nombre_especie %in% c("GAVIOTA DOMINICANA", "GAVIOTA"), 1,
                                                    ifelse(Especie=="Columba" & Nombre_especie %in% c("PALOMA", "PALOMA COMUN", "PALOMA URBANA"), 1,
                                                    ifelse(Especie=="Pele" & Nombre_especie %in% c("PELICANO"), 1,       
                                                    ifelse(Especie=="Phalacrocorax" & Nombre_especie %in% c("CORMORAN", "PATO NEGRO", "CORMORAN YECO", "PATO YECO", "YECO"), 1, 
                                                    ifelse(Especie=="Turdus" & Nombre_especie %in% c("ZORZAL"), 1,
                                                    ifelse(Especie=="Zonotrichia" & Nombre_especie %in% c("CHINCOL"), 1,
                                                                  0)))))))

RespEnc_Val <- RespEnc_Val %>% mutate(Conoce_origen= ifelse(Origen==1 & Especie %in%  c("Columba", "Turdus", "Zonotrichia"), 1,
                                                      ifelse(Origen==2 & Especie %in%  c("Larus","Pele","Phalacrocorax"),1,0)  ))

RespEnc_Val <- RespEnc_Val %>% mutate(Residencia_costa= ifelse(Residencia_costa==2 , 0, 1), Distancia_residenciaKm= (1/(Distancia_residenciaKm+1)))

RespEnc_Val <- RespEnc_Val %>% mutate(Conoce_especie= ifelse(Conoce_especie==2 , 0, 1))


RespEnc_Val <- RespEnc_Val %>% mutate(Participacion_avistamiento= case_when(Participacion_avistamiento == 1 ~ 0,
                                                                            Participacion_avistamiento == 2 ~ 0.25,
                                                                            Participacion_avistamiento == 3 ~ 0.50,
                                                                            Participacion_avistamiento == 4 ~ 1))

RespEnc_Val <- RespEnc_Val %>% mutate(Ingresos= case_when(Ingresos==1 ~150000,
                                                          Ingresos==2 ~ 425000,
                                                          Ingresos==3 ~1000000,
                                                          Ingresos==4 ~ 1925000,
                                                          Ingresos==5 ~ 3400000))
    
RespEnc_Val <- RespEnc_Val %>% mutate(Ingreso_percap= Ingresos/Integrantes_familia)

write_csv(RespEnc_Val, "RespEnc_Val.csv")                                     
                                       
                                       
                                       
                                       
                            
