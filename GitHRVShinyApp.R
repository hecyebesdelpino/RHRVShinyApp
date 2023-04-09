#Password token:

 #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
 
 #Nuevo
 #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

#Nuevo 2
 #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

#Nuevo 3
#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

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
  return (dataFrame)
}


library(shiny)
library(RHRV)
#Library ShinyJS permits enable and disable botons
library(shinyjs)

#_______________________________________________________________________________
################################################################################
##USER##########################################################################
################################################################################
#_______________________________________________________________________________
if(interactive()){
ui <- navbarPage(
  title = "Heart Rate Variability", 
  #__HOME_______________________________________________________________________
  tabPanel("Home", 
           h1("Welcome to the HRV App"), 
           h2("Select what you want to do"),
           actionButton(inputId = "botonTime", "Click for Time analysis"),
           actionButton(inputId = "botonFreq", "Click for Frequency analysis")
  ),
  
  #__TIME ANALYSIS______________________________________________________________
  tabPanel("Time analysis", 
           h1("Do you want to perform a time analysis?"),
           numericInput(inputId = "primer_numero", "Write the first number", value = 0),
           numericInput(inputId = "segundo_numero", "Write the second number", value = 0),
           actionButton(inputId = "Sumar",  "Sumar"),
           textOutput("resultado")
  ),
  
  #__FREQUENCY ANALYSIS_________________________________________________________
  tabPanel("Frequency analysis", 
           h1("Do you want to perform a frequency analysis?")
  ),
  
  #__LOAD DATA__________________________________________________________________
  tabPanel("Load Data", h1 = "Please load data",
<<<<<<< HEAD
           selectInput(inputId = "file_type_options", c("Ascii", "ECG", "RR"), label = "Select the type of file"),
=======
           selectInput(inputId = "file_type_options", c("Ascii", "ECG", "Beat"), label = "Select the type of file"),
>>>>>>> 82b1728b1188e795da0aa4b4af661fb8b1a8f403
           
         
           conditionalPanel(
             condition = "input.file_type_options == 'Ascii'",
             fileInput(inputId = "fileSelector",
                       label = "Load Data", 
                       multiple = FALSE,
                       placeholder = "No file selected",
                       accept = ".txt",
                       width = "100%")
                      ),
           
           conditionalPanel(
             condition = "input.file_type_options == 'ECG'",
             fileInput(inputId = "fileSelector",
                       label = "Load Data", 
                       multiple = FALSE,
                       placeholder = "No file selected",
                       accept = ".ecg",
                       width = "100%")
           ),
           
           
           conditionalPanel(
<<<<<<< HEAD
             condition = "input.file_type_options == 'RR'",
=======
             condition = "input.file_type_options == 'Beat'",
>>>>>>> 82b1728b1188e795da0aa4b4af661fb8b1a8f403
             fileInput(inputId = "fileSelector",
                       label = "Load Data", 
                       multiple = FALSE,
                       placeholder = "No file selected",
<<<<<<< HEAD
                       accept = ".txt",
=======
                       accept = ".beat",
>>>>>>> 82b1728b1188e795da0aa4b4af661fb8b1a8f403
                       width = "100%")
           ),
           
           
           actionButton("Analyze", "Mostrar analisis"),
           #shinyjs::disable("Analyze"), #This permits the button to not be able since a file is selected
           textOutput("cuadroAnalisis"),
           plotOutput("plotNIHR"),
           tableOutput("tabla"),
           tableOutput("tablaHistorial")
  )
 
  
)


#_______________________________________________________________________________
################################################################################
##SERVER########################################################################
################################################################################
#_______________________________________________________________________________

server <- function(input, output, session) {
  #__TIME ANALYSIS______________________________________________________________
  observeEvent(input$botonTime, {
    updateNavbarPage(session, "Heart Rate Variability", selected = "Time analysis")
  })
  
  
  
  #__FREQUENCY ANALYSIS_________________________________________________________
  observeEvent(input$botonFreq, {
    updateNavbarPage(session, "Heart Rate Variability", selected = "Frequency analysis")
  })
  
  
  #__LOAD DATA__________________________________________________________________
  datos <- reactive({
        archivoCargado <- input$fileSelector
        output$archivosCargados <- renderText(name(archivoCargado))
  })
  
 # observeEvent(input$file_type_options, {
#    if (input$file_type_options == "Ascii"){
      #updateFileInfo(session, "fileSelector", accept = ".txt")
  #  } else if (input$file_type_options == "ECG"){
      #updateFileInfo(session, "fileSelector", accept = ".ecg")
    #} else if (input$file_type_options == "Beat"){
    #  updateFileInfo(session, "fileSelector", accept = ".beat")
    #} 
#  })
  
  observeEvent(input$Analyze, {
        data3 = data.frame()
        
        #This permits to enable the button when a file has been selected
        if (!is.null(input$fileSelector)) {
          shinyjs::enable("Analyze")
<<<<<<< HEAD
          if(input$file_type_options == 'Ascii'){
            format_type = "RR"
          } else if(input$file_type_options == 'ECG'){
            format_type = "Ecg"
          } else if(input$file_type_options == 'RR'){
            format_type = "RR"
          }
          hrv.data = preparing_analysis( input$fileSelector$name,"/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/", format = format_type)
=======
          hrv.data = preparing_analysis( input$fileSelector$name,"/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/", "RR")
>>>>>>> 82b1728b1188e795da0aa4b4af661fb8b1a8f403
          data2 = time_analysis(format = "RR", file = input$fileSelector$name, class = "linear", rrs = '/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/')
        }
        
        #Plot the file in the load data file
        output$plotNIHR <- renderPlot({
          PlotNIHR(hrv.data)
        })
          
        #Print the name of the file and its datapath
        output$cuadroAnalisis <-  renderPrint({
          paste0("el archivo cargado es ",  input$fileSelector$name, " y su datapath es ",input$fileSelector$datapath)
        })
        
        #Shows the table with the time analysis
        output$tabla <-  renderTable({
          data2
        })
        
        #Shows the time analysis historial
        output$tablaHistorial <-  renderTable({
          data3 = rbind(data3, data2)
          data3
        })
  })
  
<<<<<<< HEAD
  
  
=======
>>>>>>> 82b1728b1188e795da0aa4b4af661fb8b1a8f403
  #This is just an example using the inputs with numbers
  observeEvent(input$sumar , {
    resultado <- input$primer_numero + input$segundo_numero
    output$resultado <- renderText(resultado)
  })
  
}

shinyApp(ui = ui, server = server)

}



#Codigos de prueba y ejemplos

'''Ejemplo de rbind, este si me funciona, pero no en el server
datas1 = data.frame()
datas4 = time_analysis(format = "RR", file = "nsr001_rr_secs.txt", class = "linear", rrs = '/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/')
datas1 = rbind(datas1, datas4)
'''

hrv.data = preparing_analysis( "nsr001_rr_secs.txt","/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/", "RR")
dataframe = time_analysis(format = "RR", file = "nsr001_rr_secs.txt", class = "linear", rrs = '/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/')

