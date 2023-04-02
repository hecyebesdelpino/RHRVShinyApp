#Password token:

 # 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

file_validation<-function(path){
  # 1. Check if path really exists
  if (dir.exists(path) != TRUE){
    stop("\nThe path ", path, " does not exist")
  }else{
    cat("\nThe path ", path, " exists ")
  }
  
  # 2. The path contains files:
  if ((length(list.files(path))>0) != TRUE){
    stop("but there are no files in it")
  }else{
    cat("and there are files in it\n\n")
  }
}

preparing_analysis<-function(file, rrs, format){
  hrv.data = CreateHRVData()
  hrv.data = SetVerbose(hrv.data, TRUE)
  
  hrv.data = tryCatch(
    {
      hrv.data = LoadBeat(fileType = format, HRVData = hrv.data,  Recordname = file,
                          RecordPath = rrs)
      hrv.data
    },
    error=function(cond) {
      stop(paste("The file \"", file, "\" could not be loaded. Check if the file is in the correct format; the specified format was \"", format,"\".",sep=""))
    })
  
  if(hrv.data$Verbose == TRUE){
    message(c("Loading recording ", file))
  }
  hrv.data=BuildNIHR(hrv.data)
  hrv.data=FilterNIHR(hrv.data)
  hrv.data$Beat = hrv.data$Beat[2: nrow(hrv.data$Beat),]
  return(hrv.data)
}

#Calls an RHRV function with hrv.data after cleaning the parameters
easy_call <- function(hrv.data, mf, ...) {
  args.list = plotrix::clean.args(list(...), mf)
  args.list$HRVData = hrv.data
  do.call(mf, args.list)
}

# Creating time analysis data frames
time_analysis<-function(format, files, class, rrs2, ...){
  dataFrame = data.frame()
  for (file in files) {
    hrv.data = preparing_analysis(format, file = file, rrs = rrs2)
    hrv.data = easy_call(hrv.data, CreateTimeAnalysis, ...)
    results=hrv.data$TimeAnalysis[]
    name_file = list ("filename" = file)
    group = list ("group" = class)
    # group_name = list("group" = group)
    row_list = c (name_file, results, group)
    df=as.data.frame(row_list)
    dataFrame=rbind(dataFrame, df)
  }
  dataFrame
}


library(shiny)
library(RHRV)

ui <- navbarPage(
  title = "Heart Rate Variability", 
  
  
  
  #prueba de codigo
  # Crea una opción de navegación para la ventana de inicio
  tabPanel("Home", 
           h1("Welcome to the HRV App"), 
           h2("Select what you want to do"),
           actionButton(inputId = "botonTime", "Click for Time analysis"),
           actionButton(inputId = "botonFreq", "Click for Frequency analysis")
  ),
  # Crea una opción de navegación para la ventana 2
  tabPanel("Time analysis", 
           h1("Do you want to perform a time analysis?"),
           numericInput(inputId = "primer_numero", "Write the first number", value = 0),
           numericInput(inputId = "segundo_numero", "Write the second number", value = 0),
           actionButton(inputId = "Sumar",  "Sumar"),
           textOutput("resultado")
  ),
  # Crea una opción de navegación para la ventana 3
  tabPanel("Frequency analysis", 
           h1("Do you want to perform a frequency analysis?")
  ),
  
  # Crea una opción de navegación para la ventana 3
  tabPanel("Load Data", h1 = "Please load data",
           fileInput(inputId = "fileSelector",
                     label = "Load Data", 
                     multiple = FALSE,
                     placeholder = "No file selected",
                     accept = ".txt",
                     width = "100%"),
           actionButton("Analizar", "Mostrar analisis"),
           textOutput("cuadroAnalisis")
  )
  
  
  

)


server <- function(input, output, session) {
  # Agrega la lógica para ir a la ventana 2 cuando se presiona el botón
  observeEvent(input$botonTime, {
    updateNavbarPage(session, "Heart Rate Variability", selected = "Time analysis")
  })
  observeEvent(input$botonFreq, {
    updateNavbarPage(session, "Heart Rate Variability", selected = "Frequency analysis")
  })
  
  observeEvent(input$sumar , {
    resultado <- input$primer_numero + input$segundo_numero
    output$resultado <- renderText(resultado)
  })
  
  datos <- reactive({
    archivoCargado <- input$fileSelector
    output$archivosCargados <- renderText(name(archivoCargado))
  })
    
  observeEvent(input$Analizar, {
    output$cuadroAnalisis <- time_analysis(format = "rr", files = input$fileSelector, class = time_analysis())
    
  })
 
}

shinyApp(ui = ui, server = server)

rrs <- "/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/"

time_analysis(format = "RR", files = "nsr001_rr_secs.txt", class = time_analysis(), rrs)


hrv.data = preparing_analysis( "nsr001_rr_secs.txt","/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/", "RR")
hrv.data = time_analysis(format = "RR", hrv.data, class = time_analysis())
time_analysis<-function(format, files, class, rrs2, ...){
