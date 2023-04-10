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

# Frequency analysis
freq_analysis<-function(format, files, class, rrs2, ...){
  dataFrame = data.frame()
  for (file in files) {
    hrv.data = preparing_analysis(format, file = file, rrs = rrs2)
    hrv.data = easy_call(hrv.data, InterpolateNIHR, ...)
    zero_indexes = which(hrv.data$HR == 0)
    hr_median = median(hrv.data$HR[-zero_indexes])
    hrv.data$HR[zero_indexes] = hr_median
    hrv.data = easy_call(hrv.data, CreateFreqAnalysis, ...)
    hrv.data = easy_call(hrv.data, CalculatePSD, doPlot = F, ...)
    name_file = list ("filename" = file)
    x1 = easy_call(hrv.data, CalculateEnergyInPSDBands, ...)
    names(x1) = c("ULF", "VLF", "LF", "HF")
    group = list ("group" = class)
    row_list = c (name_file, x1, group)
    df = data.frame()
    df = rbind(df, as.data.frame(row_list))
    dataFrame=rbind(dataFrame, df)
  }
  return(dataFrame)
}

#  Wavelet analysis
wavelet_analysis<-function(format, files, class, rrs2, ...){
  dataFrameMWavelet = data.frame()
  for (file in files) {
    hrv.data = preparing_analysis(format, file = file, rrs = rrs2)
    hrv.data = easy_call(hrv.data, InterpolateNIHR, ...)
    zero_indexes = which(hrv.data$HR == 0)
    hr_median = median(hrv.data$HR[-zero_indexes])
    hrv.data$HR[zero_indexes] = hr_median
    
    hrv.data = easy_call(hrv.data, CreateFreqAnalysis, ...)
    hrv.data = SetVerbose(hrv.data, verb)
    hrv.data = easy_call(hrv.data, CalculatePowerBand, ...)
    
    index = length (hrv.data$FreqAnalysis)
    resultsWavelet = hrv.data$FreqAnalysis[[index]]
    resultsWavelet$File = file
    resultsWavelet$HRV = NA
    resultsWavelet$ULF = sum(hrv.data$FreqAnalysis[[index]]$ULF)
    resultsWavelet$VLF = sum(hrv.data$FreqAnalysis[[index]]$VLF)
    resultsWavelet$LF = sum(hrv.data$FreqAnalysis[[index]]$LF)
    resultsWavelet$HF = sum(hrv.data$FreqAnalysis[[index]]$HF)
    resultsWavelet$LFHF = NA
    resultsWavelet$Time = NA
    name_file = list ()
    x1 = as.list(resultsWavelet)
    group = list ("group" = class)
    row_list = c (name_file, x1, group)
    dataFrameMWavelet = rbind(dataFrameMWavelet, as.data.frame(row_list))
    
  }
  return(dataFrameMWavelet)
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
           actionButton(inputId = "botonLinear", "Click for Linear analysis"),
           actionButton(inputId = "botonNonLinear", "Click for Non-Linear analysis")
  ),
  
  #__NON-LINEAR ANALYSIS______________________________________________________________
  tabPanel("Non-Linear Analysis", 
           h1("Do you want to perform a non-linear analysis?"),
           numericInput(inputId = "primer_numero", "Write the first number", value = 0),
           numericInput(inputId = "segundo_numero", "Write the second number", value = 0),
           actionButton(inputId = "Sumar",  "Sumar"),
           textOutput("resultado")
  ),
  
  
  #__LINEAR ANALYSIS____________________________________________________________
  tabPanel("Linear Analysis", h1 = "Please load data",
           selectInput(inputId = "linear_analysis_options", c("Time", "Frequency", "Wavelets"), label = "Select the type of file"),
           
         
           #_____TIME ANALYSIS__________________________________________________
           conditionalPanel(
             condition = "input.linear_analysis_options == 'Time'",
             selectInput(inputId = "file_type_options", c("Ascii", "ECG", "RR"), label = "Select the type of analysis", selected = 'NULL'),
             actionButton("Analyze_Time_Button", "Show Time Analysis"),
             #shinyjs::disable("Analyze_Time_Button"), #This permits the button to not be able since a file is selected
             textOutput("info_time_analysis"),
             plotOutput("plot_time_analysis"),
             tableOutput("table_time_analysis"),
             tableOutput("table_time_history")
           ),
           
           #_____FREQUENCY ANALYSIS_____________________________________________
           conditionalPanel(
             condition = "input.linear_analysis_options == 'Frequency'",
             selectInput(inputId = "file_type_options", c("Ascii", "ECG", "RR"), label = "Select the type of analysis", selected = 'NULL'),
             actionButton("Analyze_Freq_Button", "Show Frequency Analisis"),
             #shinyjs::disable("Analyze_Freq_Button"), #This permits the button to not be able since a file is selected
             textOutput("info_freq_analysis"),
             plotOutput("plot_freq_analysis"),
             tableOutput("table_freq_analysis"),
             tableOutput("table_freq_history")
           ),
           
           conditionalPanel(
             condition = "input.file_type_options == 'Ascii'",
             fileInput(inputId = "fileSelector",
                       label = "Load Data data data", 
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
             condition = "input.file_type_options == 'RR'",
             fileInput(inputId = "fileSelector",
                       label = "Load Data", 
                       multiple = FALSE,
                       placeholder = "No file selected",
                       accept = ".txt",
                       width = "100%")
           )
           
    )
 
  
)


#_______________________________________________________________________________
################################################################################
##SERVER########################################################################
################################################################################
#_______________________________________________________________________________

server <- function(input, output, session) {
  #__LINEAR ANALYSIS____________________________________________________________
  observeEvent(input$botonLinear, {
    updateNavbarPage(session, "Heart Rate Variability", selected = "Linear Analysis")
  })
  
  
  
  #__FREQUENCY ANALYSIS_________________________________________________________
  observeEvent(input$botonNonLinear, {
    updateNavbarPage(session, "Heart Rate Variability", selected = "Non-Linear Analysis")
  })
  
  
  #__TIME ANALYSIS______________________________________________________________
  observeEvent(input$Analyze_Time_Button, {
        data3 = data.frame()
        
        #This permits to enable the button when a file has been selected
        if (!is.null(input$fileSelector)) {
          shinyjs::enable("Analyze_Time_Button")
          hrv.data = preparing_analysis( input$fileSelector$name,"/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/", "RR")
          data2 = time_analysis(format = "RR", file = input$fileSelector$name, class = "linear", rrs = '/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/')
        }
        
        #Plot the file in the load data file
        output$plot_time_analysis <- renderPlot({
          PlotNIHR(hrv.data)
          #PlotHR(hrv.data)
        })
          
        #Print the name of the file and its datapath
        output$info_time_analysis <-  renderPrint({
          paste0("el archivo cargado es ",  input$fileSelector$name, " y su datapath es ",input$fileSelector$datapath)
        })
        
        #Shows the table with the time analysis
        output$table_time_analysis <-  renderTable({
          data2
        })
        
        #Shows the time analysis historial
        output$table_time_history <-  renderTable({
          data3 = rbind(data3, data2)
          data3
        })
        
  })
  
  
  
  #__FREQUENCY ANALYSIS______________________________________________________________
  observeEvent(input$Analyze_Freq_Button, {
    
    #This permits to enable the button when a file has been selected
    if (!is.null(input$fileSelector)) {
      shinyjs::enable("Analyze_Time_Button")
      hrv.data = preparing_analysis( input$fileSelector$name,"/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/", "RR")
      data2 = freq_analysis(format = "RR", file = input$fileSelector$name, class = "linear", rrs = '/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/')
    }
    
    #Plot the file in the load data file
    output$plot_freq_analysis <- renderPlot({
      PlotNIHR(hrv.data)
      #PlotHR(hrv.data)
    })
    
    #Print the name of the file and its datapath
    output$info_freq_analysis <-  renderPrint({
      paste0("el archivo cargado es ",  input$fileSelector$name, " y su datapath es ",input$fileSelector$datapath)
    })
    
    #Shows the table with the time analysis
    output$table_freq_analysis <-  renderTable({
      data2
    })
    
    #Shows the time analysis historial
    output$table_freq_history <-  renderTable({
      data3 = rbind(data3, data2)
      data3
    })
    
  })
  
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
time_analysis(format = "RR", file = "nsr001_rr_secs.txt", size = 200, class = "linear", rrs = '/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/')
freq_analysis(format = "RR", file = "nsr001_rr_secs.txt", class = "linear", type = "wavelet", rrs = '/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/')
wavelet_analysis(format = "RR", file = "nsr001_rr_secs.txt", class = "linear", rrs = '/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/', type = "wavelet")

