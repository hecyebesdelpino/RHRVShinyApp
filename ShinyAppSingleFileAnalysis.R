#Single file analysis

#TIME
time_single_analysis<-function(fileType, Recordname, RecordPath, size, interval, freqhr, method){
  tryCatch({
    hrv.data = CreateHRVData()
    hrv.data = LoadBeat(fileType = fileType, HRVData = hrv.data, Recordname = Recordname, RecordPath = RecordPath)
    hrv.data = BuildNIHR(hrv.data)
    hrv.data = FilterNIHR(hrv.data)
    hrv.data = InterpolateNIHR(HRVData = hrv.data, freqhr = freqhr, method = method)
    hrv.data$Beat = hrv.data$Beat[2: nrow(hrv.data$Beat),]
    hrv.data = CreateTimeAnalysis(HRVData = hrv.data, size = size, interval = interval)
  
return(hrv.data)
  }, error = function(e) {
    showNotification("Error: Please choose another valid file", type = "error")
  })
  
}


#Fourier
fourier_single_analysis <- function(fileType, Recordname, RecordPath, freqhr, method, size,
                                    shift, ULFmin, ULFmax, VLFmin, VLFmax, LFmin, LFmax, HFmin, HFmax, type){
  tryCatch({
  hrv.data = CreateHRVData()
  hrv.data = LoadBeat(fileType = fileType, HRVData = hrv.data, Recordname = Recordname, RecordPath = RecordPath)
  hrv.data = BuildNIHR(hrv.data)
  hrv.data = FilterNIHR(hrv.data)
  hrv.data$Beat = hrv.data$Beat[2: nrow(hrv.data$Beat),]
  hrv.data = InterpolateNIHR(HRVData = hrv.data, freqhr = freqhr, method = method)
  hrv.data = CreateFreqAnalysis(HRVData = hrv.data)
  hrv.data = CalculatePowerBand(HRVData = hrv.data, indexFreqAnalysis = length(hrv.data$FreqAnalysis), size = size ,
                                shift = shift, scale = method, ULFmin = ULFmin, ULFmax = ULFmax, VLFmin = VLFmin, VLFmax = VLFmax,
                                LFmin = LFmin, LFmax = LFmax, HFmin = HFmin, HFmax = HFmax, type = type)
  return(hrv.data)
}, error = function(e) {
  showNotification("Error: Please choose another valid file", type = "error")
})

}



#WAVELET
wavelet_single_analysis <- function(fileType, Recordname, RecordPath, freqhr, method, size,
                                    shift, ULFmin, ULFmax, VLFmin, VLFmax, LFmin, LFmax, HFmin, HFmax, type, wavelet, bandtolerance) {
  tryCatch({
    hrv.data = CreateHRVData()
    hrv.data = LoadBeat(fileType = fileType, HRVData = hrv.data, Recordname = Recordname, RecordPath = RecordPath)
    hrv.data = BuildNIHR(hrv.data)
    hrv.data = FilterNIHR(hrv.data)
    hrv.data$Beat = hrv.data$Beat[2: nrow(hrv.data$Beat),]
    hrv.data = InterpolateNIHR(HRVData = hrv.data, freqhr = freqhr, method = method)
    hrv.data = CreateFreqAnalysis(HRVData = hrv.data)
    hrv.data = CalculatePowerBand(HRVData = hrv.data, indexFreqAnalysis = length(hrv.data$FreqAnalysis), size = size ,
                                  shift = shift, scale = method, ULFmin = ULFmin, ULFmax = ULFmax, VLFmin = VLFmin, VLFmax = VLFmax,
                                  LFmin = LFmin, LFmax = LFmax, HFmin = HFmin, HFmax = HFmax, type = type, wavelet = wavelet, bandtolerance = bandtolerance)
    return(hrv.data)
  }, error = function(e) {
    showNotification("Error: Please choose another valid file", type = "error")
  })

}



#RESULTS
#MULTIPLE TABLE
multiple_results <- function(resultados_dataframe){
    nueva_tabla <- capture.output(resultados_dataframe)
    nueva_tabla_depurada <- nueva_tabla[nueva_tabla != ""]
    matriz <- matrix(nueva_tabla_depurada, ncol  = 3, nrow = length(nueva_tabla_depurada), byrow = FALSE)
    matriz[,1] <-  sub(".*in\\s+(.*);.*","\\1", matriz[,1])
    matriz[,1] <-  sub(".*for the group.*", " ", matriz[,1])
    matriz[,2] <-  sub(".*group\\s+(\\w+)\\sis.*", "\\1", matriz[,2]) 
    matriz[, 2] <- sub(";.*", "", matriz[, 2])
    #matriz[,2] <-  sub("^There is a statistically.*", "File", matriz[,2]) 
    matriz[,3] <-  sub(".*: ", "", matriz[,3]) 
    matriz[, 3] <- sub(".*; ", "", matriz[, 3])
    matriz[, 3] <- gsub("\\+-", "+/-", matriz[, 3])
    matriz[1][1] <- "RESULTS"
    colnames(matriz) <- c(' ','RESULTS',' ')
return(matriz)
}

# '''
# EXAMPLE OF STACK OVERFLOW
# library(shiny) 
# ui <- fluidPage(
#   titlePanel("Stream the system output"),
#   sidebarLayout(
#     sidebarPanel(
#       actionButton("btn_start",label = "Let's stream"),
#       actionButton("btn_stop",label = "Stop")
#     ),
#     mainPanel(
#       htmlOutput("textstream_output")
#     )
#   )
# )
# server <- function(input, output, session) {
#   rv <- reactiveValues(textstream = c(""),
#                        timer = reactiveTimer(1000),
#                        started = FALSE)
#   observeEvent(input$btn_start, { 
#     rv$started <- TRUE
#     system2("Rscript", "so_script.R", wait = FALSE)
#   })
#   observeEvent(input$btn_stop, { rv$started <- FALSE })
#   observe({
#     rv$timer()
#     if (isolate(rv$started))
#       rv$textstream <- paste(readLines("so_output.txt"), collapse = "<br/>")
#   })
#   output$textstream_output <- renderUI({
#     HTML(rv$textstream)
#   })
# }
# shinyApp(ui = ui, server = server)
# 
# cat('sink(file = "so_output.txt")
#   for (i in 1:10) {
#     cat(format(Sys.time(), format = "%H:%M:%S"), "\n")
#     Sys.sleep(1)
#   }
#   cat("*** EOF ***\n")
#   sink()
# ', file = "so_script.R")
# '''
