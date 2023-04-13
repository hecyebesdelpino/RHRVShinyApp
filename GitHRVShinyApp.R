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
    
    hrv.data = CalculatePowerBand(hrv.data, indexFreqAnalysis = 1, size =300, shift = 30, sizesp = 2048, type = "fourier", ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05, LFmin = 0.05, LFmax = 0.15, HFmin = 0.15, HFmax = 0.4)
    PlotPowerBand(hrv.data, indexFreqAnalysis = 1, ymax = 200, ymaxratio = 1.7)
    
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
    #hrv.data = SetVerbose(hrv.data, verb)
    hrv.data = easy_call(hrv.data, CalculatePowerBand, ...)
    
    hrv.data = CalculatePowerBand(hrv.data, indexFreqAnalysis = 1, type = "wavelet", wavelet = "la8", bandtolerance = 0.01, relative = FALSE)
    PlotPowerBand(hrv.data, indexFreqAnalysis = 1, ymax = 700, ymaxratio = 50)
    
    
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

attempToCalculateTimeLag <- function(hrv.data) {
  lag = 30
  kTimeLag = tryCatch(
    {
      kTimeLag <- CalculateTimeLag(hrv.data, technique = "acf", method = "first.minimum",
                                   lagMax = lag, doPlot=FALSE)
      kTimeLag
    },
    error=function(cond) {
      tryCatch(
        {
          kTimeLag <- CalculateTimeLag(hrv.data, technique = "acf", method = "first.e.decay",
                                       lagMax = lag, doPlot=FALSE)
          kTimeLag
        },
        error=function(cond) {
          
          tryCatch(
            {
              kTimeLag <- CalculateTimeLag(hrv.data, technique = "ami", method = "first.minimum",
                                           lagMax = lag, doPlot=FALSE)
              kTimeLag
            },
            error=function(cond) {
              tryCatch(
                {
                  kTimeLag <- CalculateTimeLag(hrv.data, technique = "ami", method = "first.e.decay",
                                               lagMax = lag, doPlot=FALSE)
                  kTimeLag
                },
                error=function(cond) {
                  if(verb){
                    message("Using default timeLag for current recording...")
                  }
                  30
                }
              )
            }
          )
        }
      )
    }
  )
  if(verb){
    message(c("Time Lag for takens reconstruction: ", kTimeLag))
  }
  kTimeLag
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
  

  #__LINEAR ANALYSIS____________________________________________________________
  tabPanel("Linear Analysis",
           selectInput(inputId = "file_type_options", c("Ascii", "RR", "Polar", "Suunto", "EDFPlus", "Ambit", " "), label = "Select the file type", selected = " "),
           
           conditionalPanel(
             condition = "input.file_type_options == 'Ascii'",
             fileInput(inputId = "fileSelector",
                       label = "Load Data", 
                       multiple = FALSE,
                       placeholder = "No file selected",
                       accept = ".txt",
                       width = "100%"
                       )
           ),
           
           conditionalPanel(
             condition = "input.file_type_options == 'Polar'",
             fileInput(inputId = "fileSelector",
                       label = "Load Data", 
                       multiple = FALSE,
                       placeholder = "No file selected",
                       accept = ".polar",
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
           ),
           
           conditionalPanel(
             condition = "input.file_type_options == 'Suunto'",
             fileInput(inputId = "fileSelector",
                       label = "Load Data", 
                       multiple = FALSE,
                       placeholder = "No file selected",
                       accept = ".txt",
                       width = "100%")
           ),
           
           conditionalPanel(
             condition = "input.file_type_options != ' '",
             selectInput(inputId = "linear_analysis_options", c("Time", "Frequency", "Wavelets", " "), label = "Select the analysis", selected = " ")
           ),
           
           #_____TIME ANALYSIS__________________________________________________
           conditionalPanel(
             condition = "input.file_type_options != ' ' && input.linear_analysis_options == 'Time'",
             sliderInput("window_size_slider", label = "Chose the window size", min = 50, max = 600, step = 1, value = 300),
             actionButton("Analyze_Time_Button", "Show Time Analysis"),
             #shinyjs::disable("window_size_slider"),#This permits the button to not be able since a file is selected
             #shinyjs::disable("Analyze_Time_Button"),
             textOutput("info_time_analysis"),
             plotOutput("plot_time_analysis"),
             tableOutput("table_time_analysis"),
             textOutput("info_time_analysis2"),
             tableOutput("table_time_history")
           ),
           
           #_____FREQUENCY ANALYSIS_____________________________________________
           conditionalPanel(
             condition = "input.file_type_options != ' ' && input.linear_analysis_options == 'Frequency'",
             sliderInput("freq_size_slider", label = "Chose the window size", min = 2, max = 24, step = 1, value = 4),
             actionButton("Analyze_Freq_Button", "Show Frequency Analisis"),
             #shinyjs::disable("Analyze_Freq_Button"), #This permits the button to not be able since a file is selected
             textOutput("info_freq_analysis"),
             plotOutput("plot_freq_analysis"),
             tableOutput("table_freq_analysis"),
             textOutput("info_freq_analysis2"),
             tableOutput("table_freq_history"),
             
           ),
           
           #_____WAVELETS ANALYSIS_____________________________________________
           conditionalPanel(
             condition = "input.file_type_options != ' ' && input.linear_analysis_options == 'Wavelets'",
             actionButton("Analyze_Wave_Button", "Show Wavelets Analisis"),
             #shinyjs::disable("Analyze_Wave_Button"), #This permits the button to not be able since a file is selected
             textOutput("info_wave_analysis"),
             plotOutput("plot_wave_analysis"),
             tableOutput("table_wave_analysis"),
             textOutput("info_wave_analysis2"),
             tableOutput("table_wave_history")
           )
           
    ),
  
  #__NON-LINEAR ANALYSIS______________________________________________________________
  tabPanel("Non-Linear Analysis", 
           h1("Do you want to perform a non-linear analysis?"),
           numericInput(inputId = "primer_numero", "Write the first number", value = 0),
           numericInput(inputId = "segundo_numero", "Write the second number", value = 0),
           actionButton(inputId = "Sumar",  "Sumar"),
           textOutput("resultado")
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
  
  
  
  #__NON-LINEAR ANALYSIS_________________________________________________________
  observeEvent(input$botonNonLinear, {
    updateNavbarPage(session, "Heart Rate Variability", selected = "Non-Linear Analysis")
  })
  
  data3 = data.frame()
  
  #__TIME ANALYSIS______________________________________________________________
  observeEvent(input$Analyze_Time_Button, {
        #data3 = data.frame()
        
        #This permits to enable the button when a file has been selected
        if (!is.null(input$fileSelector)) {
          shinyjs::enable("Analyze_Time_Button")
          shinyjs::enable("window_size_slider")
          hrv.data = preparing_analysis( input$fileSelector$name,"/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/", "RR")
          data2 = time_analysis(format = "RR", file = input$fileSelector$name, size = input$window_size_slider, class = "linear", rrs = '/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/')
        
        
        #Plot the file in the load data file
        output$plot_time_analysis <- renderPlot({
          PlotNIHR(hrv.data)
          #PlotHR(hrv.data)
        })
          
        #Print the name of the file and its datapath
        output$info_time_analysis <-  renderPrint({
          cat(paste0("El archivo cargado es -> ",  input$fileSelector$name, " y su datapath es: ",input$fileSelector$datapath))
        })
        
        #Shows the table with the time analysis
        output$table_time_analysis <-  renderTable({
          data2
        })
        
        #Shows the time analysis historial
        output$table_time_history <-  renderTable({
          output$info_time_analysis2 <-  renderPrint({
            cat(paste0("History"))
          })
          data3 = rbind(data3, data2)
          data3
        })
        }
  })
  
  
  
  #__FREQUENCY ANALYSIS______________________________________________________________
  observeEvent(input$Analyze_Freq_Button, {
    
    #This permits to enable the button when a file has been selected
    if (!is.null(input$fileSelector)) {
      shinyjs::enable("Analyze_Time_Button")
      hrv.data = preparing_analysis( input$fileSelector$name,"/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/", "RR")
      data2 = freq_analysis(format = "RR", file = input$fileSelector$name, freqhr = input$freq_size_slider , class = "linear", type = "fourier", rrs = '/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/')
      
    #Plot the file in the load data file
    output$plot_freq_analysis <- renderPlot({
      #PlotPowerBand(hrv.data2, indexFreqAnalysis = 1, ymax = 200, ymaxratio = 1.7)
      data2 = freq_analysis(format = "RR", file = input$fileSelector$name, freqhr = input$freq_size_slider, class = "linear", type = "fourier", rrs = '/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/')
      
      })
    
    #Print the name of the file and its datapath
    output$info_freq_analysis <-  renderPrint({
      cat(paste0("El archivo cargado es -> ",  input$fileSelector$name, " y su datapath es: ",input$fileSelector$datapath))
    })
    
    #Shows the table with the time analysis
    output$table_freq_analysis <-  renderTable({
      data2
    })
    
    #Shows the time analysis historial
    output$table_freq_history <-  renderTable({
      output$info_freq_analysis2 <-  renderPrint({
       cat(paste("History"))
      })
      data3 = rbind(data3, data2)
      data3
    })
    }
  })
  
  #__WAVELETS ANALYSIS______________________________________________________________
  observeEvent(input$Analyze_Wave_Button, {
    
    #This permits to enable the button when a file has been selected
    if (!is.null(input$fileSelector)) {
      shinyjs::enable("Analyze_Wave_Button")
      hrv.data = preparing_analysis( input$fileSelector$name,"/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/", "RR")
      data2 = wavelet_analysis(format = "RR", type = "wavelet", file = input$fileSelector$name, class = "linear", rrs = '/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/')
    }
    
    #Plot the file in the load data file
    output$plot_wave_analysis <- renderPlot({
      data2 = wavelet_analysis(format = "RR", type = "wavelet", file = input$fileSelector$name, class = "linear", rrs = '/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/')
    })
    
    #Print the name of the file and its datapath
    output$info_wave_analysis <-  renderPrint({
      cat(paste0("El archivo cargado es -> ",  input$fileSelector$name, " y su datapath es: ",input$fileSelector$datapath))
    })
    
    #Shows the table with the time analysis
    output$table_wave_analysis <-  renderTable({
      data2
    })
    
    #Shows the time analysis historial
    output$table_wave_history <-  renderTable({
      output$info_wave_analysis2 <-  renderPrint({
        cat(paste0("History"))
      })
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
freq_analysis(format = "RR", file = "nsr001_rr_secs.txt", class = "linear", type = "fourier", rrs = '/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/')
wavelet_analysis(format = "RR", file = "nsr001_rr_secs.txt", class = "linear", rrs = '/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/', type = "wavelet")


hrv.data2 = easy_call(hrv.data, CreateFreqAnalysis,...)
hrv.data = easy_call(hrv.data, CalculatePSD, doPlot = F,...)

hrv.data2 = preparing_analysis( "nsr001_rr_secs.txt","/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/", "RR")
hrv.data = easy_call(hrv.data2, InterpolateNIHR, ...)
hrv.data2 = easy_call(hrv.data2, CreateFreqAnalysis)
hrv.data2 = CreateFreqAnalysis(hrv.data2)
hrv.data2 = CalculatePowerBand(hrv.data2, indexFreqAnalysis = 1, size =300, shift = 30, sizesp = 2048, type = "fourier", ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05, LFmin = 0.05, LFmax = 0.15, HFmin = 0.15, HFmax = 0.4)
PlotPowerBand(hrv.data2, indexFreqAnalysis = 1, ymax = 200, ymaxratio = 1.7)



hrv.data2 = preparing_analysis( "nsr018_rr_secs.txt","/Users/hecyebesdelpino/Desktop/TFG/NormalEnTXT/", "RR")
hrv.data2 = CreateFreqAnalysis(hrv.data2)
hrv.data2 = CalculatePowerBand(hrv.data2, indexFreqAnalysis = 1, type = "wavelet", wavelet = "la8", bandtolerance = 0.01, relative = FALSE)
PlotPowerBand(hrv.data2, indexFreqAnalysis = 1, ymax = 700, ymaxratio = 50)


