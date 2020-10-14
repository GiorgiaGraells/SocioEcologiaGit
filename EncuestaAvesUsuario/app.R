#ENCUESTA AVES USUARIO
#
# Encuesta modificada -prim 2019
# donde se consideran las 6 especies y 2 ambientes para preguntas x sp.

#set.seed(as.numeric(Sys.time())) : incluido 21-12-2019 par aleatorizar correctamente


library(shiny)
library(shinyjs)

maxSpp <- 6
maxAmbNr <- 6
Size <- 2
Ambientes <- 1:maxAmbNr
set.seed(as.numeric(Sys.time())) #incluido 21-12 par aleatorizar correctamente
RandomH <-lapply(rep(maxAmbNr, 6), sample, size = Size, replace = FALSE)

# Define the fields we want to save from the form
fields <- c("Nombre_encuestado", "ID_encuesta")


saveData <- function(input) {
  # put variables in a data frame
  data <- data.frame(matrix(nrow=1,ncol=0))
  for (x in fields) {
    var <- input[[x]]
    if (length(var) > 1 ) {
      # handles lists from checkboxGroup and multiple Select
      data[[x]] <- list(var)
    } else {
      # all other data types
      data[[x]] <- var
    }
  }
  data$submit_time <- date()
  
  
  # Write the file to the local system
  readr::write_rds(data,paste0("CI_temp/CI_", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"), ".rds"))
}
  
loadData <- function() {
  # read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  
  if (length(files) == 0) {
    # create empty data frame with correct columns
    field_list <- c(fields, "submit_time")
    data <- data.frame(matrix(ncol = length(field_list), nrow = 0))
    names(data) <- field_list
  } else {
    data <- lapply(files, function(x) readRDS(x)) 
    
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
  }
  
  data
}


###otra funcion para session:

resetForm <- function(session) {
  
  updateTextInput(session, "Nombre_encuestado", value = "")
  updateTextInput(session, "ID_encuesta", value = "")
  
}



shinyApp(
  ui = tagList(
    shinyjs::useShinyjs(),
    navbarPage("Encuesta percepcion aves",
               
               tabPanel("Consentimiento informado",
                        
                        mainPanel( 
                          fluidRow(
                            column(8,
                                   "CARTA DE CONSENTIMIENTO INFORMADO ENCUESTA"),
                            column(4, 
                                   img(src = "logopuc.png", height = 90, width = 70)),
                            p(strong("Usted ha sido invitado a participar en un estudio sobre las percepciones de las aves en ciudades costeras de la región de Valparaíso.")),
                            p("Esta investigación se encuentra a cargo de la investigadora Dra (c). Giorgia Graells, y corresponde a la tesis de Doctorado en Ciencias Biológicas, mención Ecología, de la Pontificia Universidad Católica de Chile, financiada por la comisión Nacional de Investigación Científica y Tecnológica, CONICYT"),        
                            p("El objeto de esta carta es ayudarlo a tomar la decisión de participar en la presente investigación que se presenta con una encuesta. Esta encuesta tiene por objejtivo conocer su percepción y valorización de las aves presentes en distintos ambientes presentes en su ciudad. Para esto usted fue seleccionado al azar entre los transeúntes."),          
                            p("Si usted decide participar de esta encuesta, será consultado sobre algunos DATOS PERSONALES, CONOCIMIENTO Y PERCEPCIONES SOBRE AVES Y LOS AMBIENTES DONDE SE LES PUEDE ENCONTRAR."),
                            p(strong("Para esto se utilizarán dos tablets, una que será manejada por usted y que presentará distintas imágenes sobre las cuales se le harán algunas preguntas. Mientras q la segunda tablet poseerá el cuestionario y será donde el encuestador irá rellenando con sus respuestas.")),  
                            p("Su participación no le tomará más de 20 minutos aprox. que corresponde a lo que dura este cuestionario. Ud. no será contactado nuevamente una vez terminada la encuesta."),
                            p("No existen riesgos ni costos asociados a la participación de esta actividad. No existen beneficios directos asociados a la participación de esta actividad."),          
                            p("Los investigadores mantendrán la CONFIDENCIALIDAD con respecto a cualquier información obtenida en este estudio. Los datos que usted entregará serán guardados en un computador y serán sólo verificados y analizados por el investigador principal."),          
                            p("Usted NO está obligado de ninguna manera a participar en este estudio. Si accede a participar, puede dejar de hacerlo en cualquier momento sin repercusión alguna."),          
                            p("Si tiene cualquier pregunta acerca de esta investigación, puede contactar a Giorgia Graells de la Pontificia Universidad Católica de Chile al email: gygraell@uc.cl. Si desea, una vez que la investigación sea completada, usted puede ser contactado para hacerle llegar los resultados finales de este estudio."),
                            p("Si usted tiene alguna consulta o preocupación respecto a sus derechos como participante de este estudio, puede contactar a la presidenta del Comité de Ética de Ciencias Sociales, Artes y Humanidades de la Pontificia Universidad Católica de Chile, profesora María Elena Gronemeyer F., al siguiente email: eticadeinvestigacion@uc.cl.")         
                          ),
                          wellPanel(
                           
                            textInput("Nombre_encuestado", label=h5("Nombre participante", ""))),
                          
                          wellPanel(
                            textInput("ID_encuesta", label=h5("ID encuesta (iniciales más fecha nacimiento, por ejemplo: GG14071981)", "")),
                            actionButton("Submit", "Acepto participar"))
                          
                          
                        )
               ), #cerrado tab panel 1
               
               tabPanel("Preguntas sobre aves",
                        fluidRow(
                          column(width = 12,
                                 uiOutput('mytabs')
                          ) )
                        
               ),#cerrado tab panel 2
               
               tabPanel("Origen aves",
                        fluidRow(
                          column(width = 12,
                                 uiOutput('birdtabs')
                          ) )
                 
               ),
               tabPanel("Percepción ambientes",
                        fluidRow(
                          column(width = 12,
                                 uiOutput('envtabs')
                          ) )
                        
                 
               )
               
    ) #cerrado de navbar Panel    
    
  ), #cerrado ui
  
  server <- function(input, output, session) {
    
    
    global <- reactiveValues(nr = 1)    
    
    observeEvent(input$whichTab, {
      global$nr <- 1 
    })
    
    observeEvent(input$New_Button1, {
      global$nr <- min(global$nr + 1, Size)
    })
    
    observeEvent(input$New_Button2, {
      global$nr <- min(global$nr + 1, Size)
    })
    
    observeEvent(input$New_Button3, {
      global$nr <- min(global$nr + 1, Size)
    })
    
    observeEvent(input$New_Button4, {
      global$nr <- min(global$nr + 1, Size)
    })
    
    observeEvent(input$New_Button5, {
      global$nr <- min(global$nr + 1, Size)
    })
    
    observeEvent(input$New_Button6, {
      global$nr <- min(global$nr + 1, Size)
    })
    
    
    #####################################
    # Tabs breeds
    
    output$mytabs = renderUI({
      Especies <- c(1:6)
      Random <- sample(Especies, 6)
      myTabs = lapply(Random, function(i){
        
        
        tabPanel(paste("Especies", i),
                 
                 ###
                 fluidPage(
                   fluidRow(
                     column(5,
                            img(src = paste0("Especie",i, ".jpg"), height = 501, width = 374)),
                     a(id =paste0("togglePhotohabitat",i), wellPanel("Ver ambiente")),
                     renderUI({
                       column(7, 
                              shinyjs::hidden(div(id = paste0("Photohabitat",i), 
                                                  uiOutput(paste0("PlotAmbiente", i)),
                                                  #textOutput(paste0("TextAmbiente", i)),
                                                  actionButton(inputId = paste0("New_Button", i), "Siguiente ambiente")))
                              
                       )
                       
                     })
                   )
                 ))
        ####
      })
      
      
      do.call(tabsetPanel, c(id = "whichTab", myTabs))
    })
    
    
    shinyjs::onclick("togglePhotohabitat1",
                     shinyjs::toggle(id = "Photohabitat1", anim = TRUE))
    shinyjs::onclick("togglePhotohabitat2",
                     shinyjs::toggle(id = "Photohabitat2", anim = TRUE))
    shinyjs::onclick("togglePhotohabitat3",
                     shinyjs::toggle(id = "Photohabitat3", anim = TRUE))
    shinyjs::onclick("togglePhotohabitat4",
                     shinyjs::toggle(id = "Photohabitat4", anim = TRUE))
    shinyjs::onclick("togglePhotohabitat5",
                     shinyjs::toggle(id = "Photohabitat5", anim = TRUE))
    shinyjs::onclick("togglePhotohabitat6",
                     shinyjs::toggle(id = "Photohabitat6", anim = TRUE))
    
    
    output$PlotAmbiente1 <- renderUI({
      img(src = paste0("Ambiente", RandomH[[1]][global$nr],".jpg"), height = 280, width = 472)
    })

    output$PlotAmbiente2 <- renderUI({
      img(src = paste0("Ambiente", RandomH[[2]][global$nr],".jpg"), height = 280, width = 472)
    })
    

    output$PlotAmbiente3 <- renderUI({
      img(src = paste0("Ambiente", RandomH[[3]][global$nr],".jpg"), height = 280, width = 472)
    })
    

    output$PlotAmbiente4 <- renderUI({
      img(src = paste0("Ambiente", RandomH[[4]][global$nr],".jpg"), height = 280, width = 472)
    })
    

    output$PlotAmbiente5 <- renderUI({
      img(src = paste0("Ambiente", RandomH[[5]][global$nr],".jpg"), height = 280, width = 472)
    })
    

    output$PlotAmbiente6 <- renderUI({
      img(src = paste0("Ambiente", RandomH[[6]][global$nr],".jpg"), height = 280, width = 472)
    })
    

    
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$Submit, {
      saveData(input)
      resetForm(session)
    })
    
    # clear the fields
    observeEvent(input$clear, {
      resetForm(session)
    })
    
    Especies <- c(1:6)
    RandomB <- sample(Especies, 6)
    
    output$birdtabs = renderUI({
      
      column(12, img(src = paste0("Especie",RandomB[globalB$bird],".jpg"), height = 501, width = 374),
      actionButton(inputId = "New_species", "Siguiente especie"))
})
    
    globalB <- reactiveValues(bird = 1)    
    

    observeEvent(input$New_species, {
      globalB$bird <- min(globalB$bird + 1, maxSpp)
    })
    #######################
    ###envtab#############
    
    RandomEnv <- sample(Ambientes, 6)
    
    output$envtabs = renderUI({
      
      column(12, img(src = paste0("Ambiente",RandomEnv[globalEnv$env],".jpg"), height = 420, width = 708),
             actionButton(inputId = "New_Env", "Siguiente ambiente"))
    })
    
    globalEnv <- reactiveValues(env = 1)    
    
    
    observeEvent(input$New_Env, {
      globalEnv$env <- min(globalEnv$env + 1, maxAmbNr)
    })
    
  }


  
) 