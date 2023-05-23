{
library(shiny)
library(RHRV)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
  
setwd("C://Users/hyebe/Desktop/RHRVEasy-master/")
source("RHRVEasy.R")
source("ShinyAppCustomizedStyles.R")
source("ShinyAppRestrictions.R")

##USER##########################################################################
ui <- fluidPage(
    tabsetPanel( id = "tabset",
      
      #__HOME___________________________________________________________________
      tabPanel(
        title = "Heart Rate Variability", 
        h1("Welcome to the HRV App"), 
        p("This app will allow you to obtain some graphical and statistical studies of your HRV samples"),
        p("Just go through the different tabs and try them all"),
        p("This app reads Ascii, RR, Ambit, Suunto and EDFPlus files"),
        p("If you have any problem, please contact us"),
        #tags$img(src = ".", width = "800px"),
        img(src = ".", width = "300px")
      ),
      
 
      #__MULTIPLE FILES_________________________________________________________
      tabPanel("Multiple Files Analysis", 
               sidebarLayout(
                 sidebarPanel(
                   numericInput(inputId = "num_samples", label = "Number of samples", value = 1, min = 1, step = 1),
                   bsTooltip("num_samples", "Press each button for folder selection", placement = "right"),
                   selectInput("format_check", label = "Select the type of file", choices = c("RR","Ascii", "Polar", "Suunto", "EDFPlus", "Ambit"), selected = NULL),
                   uiOutput(outputId = "samples"),
                   textOutput("info"),
                   br(),
                   actionButton("settings_button", label = icon("cog")),
                   br(),

                 ),
                 

                 mainPanel(
                       br(),
                       numericInput("significance_level", "Significance level", value = 0.05),
                       
                       h3("Correction method"),
                       selectInput(inputId = "correction_method_selection", choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"), selected = "bonferroni",  label =  "Please select the correction method"),
                      
                       p("When the analysis finishes, do you want to save a copy of the indexes?"),
                       actionButton("save_yes", label = "Yes"),
                       actionButton("save_no", label = "No"),
                       #selectInput(inputId = "save_HRV_indexes", choices = c("Yes", "No"), selected = "No", label = "Want a copy of the analysis"),
                       textOutput("save"),
                       br(),
                       textOutput("info_other_panel"),
                       
                    conditionalPanel(
                       condition = ("input.save_yes > 0 || input.save_no > 0"),
                       actionButton("RHRV", "RHRV study", class = "blue-button"),
                       textOutput("console_info"),
                       br(),
                    ),
                    
                    conditionalPanel(
                       condition = "input.RHRV > 0",
                       p("Calculating.... Please wait, it could take some minutes"),
                       tableOutput("table_RHRV_analysis"),
                       textOutput("RHRV_results"),
                       actionButton("save_2", label = "Save", icon = icon("save")),
                       textOutput("save2"),
                    ),
                    
                   
                    textOutput("info_multiple_analysis")
                 )
               )
      ),
      
      #Settings
      tabPanel("Configuration",
               column(width = 6,
               h3("Time analysis configuration"),
               numericInput("window_size_button", "Window size (Value from 1 to 1000)", value = 300, min = 1, max = 1000, step = 1),
               bsTooltip("window_size_button", "The size of the window employed in time analysis. Default: 300 miliseconds", placement = "right"),
               #LIMITAMOS MINIMO Y MAXIMO??? ENTRE 1 y 10??
               numericInput("interval_size_button", "Interval", value = 7.8125),
               bsTooltip("interval_size_button", "Bin width of the histogram. Default 7.8125", placement = "right"),
               br(),
               ),


               column(width = 6,
               h3("Frequency analysis configuration"),
               numericInput("freqhr_button", "Frecuency size", value = 4, min = 2, max = 16, step = 1),
               bsTooltip("freqhr_button", "Frequency interpolation value. Default: 4 Hz", placement = "right"),
               selectInput(inputId = "frequency_method_selection", c("linear", "spline"), label = "Select the interpolation method", selected = "spline"),
               selectInput(inputId = "frequency_type_selection", c("fourier", "wavelet"), label = "Select the frequency type analysis", selected = "fourier"),
               actionButton("more_freq_options", "More options"),
               br(),
               
               conditionalPanel(
                 condition = "input.more_freq_options > 0 && input.frequency_type_selection == 'fourier'",
                 br(),
                 selectInput(inputId = "fourier_method_selection", choices = c("ar", "lomb", "pgram"), label = "Select the fourier method", selected = "lomb"),
                 
                 numericInput("ULFmin", "ULFmin", value = 0, step = 0.001, min = 0, max = 0.03),
                 bsTooltip("ULFmin", "Standard values<br/>from 0 to 0,03<br/>ULFmin < ULFmax", placement = "right"),
                
                 numericInput("ULFmax", "ULFmax", value = 0.03, step = 0.001),
                 bsTooltip("ULFmax", "Standard values<br/>from 0,03 to 0,05<br/>ULFmin < ULFmax < VLFmin", placement = "right"),
                
                 numericInput("VLFmin", "VLFmin", value = 0.03, step = 0.001),
                 bsTooltip("VLFmin", "Standard values<br/>from 0,03 to 0,05<br/>ULFmax < VLFmin < VLFmax", placement = "right"),
                
                 numericInput("VLFmax", "VLFmax", value = 0.05, step = 0.001),
                 bsTooltip("VLFmax", "Standard values<br/>from 0,05 to 0,15<br/>VLFmin < VLFmax < LFmin", placement = "right"),
                 
                 numericInput("LFmin", "LFmin", value = 0.05, step = 0.001),
                 bsTooltip("LFmin", "Standard values<br/>from 0,05 to 0,15<br/>VLFmax < LFmin < LFmax", placement = "right"),
                
                 numericInput("LFmax", "LFmax", value = 0.15, step = 0.001),
                 bsTooltip("LFmax", "Standard values<br/>from 0,15 to 0,4<br/>LFmin < LFmax < HFmin", placement = "right"),
                 
                 numericInput("HFmin", "HFmin", value = 0.15, step = 0.001),
                 bsTooltip("HFmin", "Standard values<br/>from 0,15 to 0,4<br/>LFmax < HFmin < HFmax", placement = "right"),
                
                 numericInput("HFmax", "HFmax", value = 0.4, step = 0.001, max = 1),
                 bsTooltip("HFmax", "Standard values<br/>from 0,4 to 1<br/>HFmin < HFmax", placement = "right"),

                 br()
               ),
               
               conditionalPanel(
                 condition = "input.more_freq_options > 0 && input.frequency_type_selection == 'wavelet'",
                 br(),
                 selectInput(inputId = "wavelet_method_selection", choices = c("la8","la16","la20","d4","d6","d8","d16","bl14","bl20","fk4","fk6","fk8","fk14","fk22","mb4","mb8","mb16","mb24", "bs3.1"), label = "Select the wavelet type", selected = "d4"),
                 #MIN Y MAXIMO???
                 numericInput("band_tolerance_button", "Band tolerance", value = 0.1, min = 0.001),
                 bsTooltip("band_tolerance_button", "Maximum acceptable error in the estimation of spectral bands", placement = "right"),
                 br()
               ),
               )
      ),
      
      
#__LINEAR ANALYSIS____________________________________________________________
      tabPanel("Single File Analysis",
        sidebarLayout(
          sidebarPanel(  
               selectInput(inputId = "linear_analysis_options", c("Time analysis", "Fourier analysis", "Wavelets analysis"), label = "Select the analysis", selected = "Time analysis"),
               selectInput(inputId = "type_of_file", c("RR","Ascii", "Polar", "Suunto", "EDFPlus", "Ambit"), label = "Select the type of file", selected = NULL),
               actionButton("file_selector", label = "Select file"),
               bsTooltip("file_selector", "Click to select file", placement = "right"),
               actionButton("settings_button2", label = icon("cog"), class ="right-btn"),
               bsTooltip("settings_button2", "Settings", placement = "right"),
               hr(),
               textOutput("single_file_info"),
               ),
           
              
          mainPanel(
#_____TIME ANALYSIS_____________________________________________________________
               conditionalPanel(
                 condition = "input.linear_analysis_options == 'Time analysis'",
                 br(),
                 textOutput("config_time_selection"),
                 br(),
                 actionButton("Analyze_Time_Button", "Time analysis", class = "blue-button"),
                 textOutput("info_file_selection"),
                 plotOutput("plot_time_analysis"),
                 tableOutput("table_time_analysis"),
               ),
               
#_____FREQUENCY ANALYSIS________________________________________________________
               conditionalPanel(
                 condition = "input.linear_analysis_options == 'Fourier analysis'",
                 br(),
                 textOutput("config_fourier_selection"),
                 br(),
                 textOutput("config_fourier_selection2"),
                 actionButton("Analyze_Fourier_Button", "Fourier analysis", class = "blue-button"),
                 textOutput("info_freq_selection"),
                 plotOutput("plot_freq_analysis"),
                 tableOutput("table_freq_analysis"),
               ),
               
#_____WAVELETS ANALYSIS_________________________________________________________
               conditionalPanel(
                 condition = "input.linear_analysis_options == 'Wavelets analysis'",
                 br(),
                 textOutput("config_wave_selection"),
                 br(),
                 actionButton("Analyze_Wave_Button", "Wavelet analysis", class = "blue-button"),
                 textOutput("info_wave_selection"),
                 plotOutput("plot_wave_analysis"),
                 tableOutput("table_wave_analysis"),
               )
      )
)
      ),
    )
  )
  
  
  
  
  
  
  
##SERVER########################################################################
  server <- function(input, output, session) {
    
#__MULTIPLE ANALYSIS__________________________________________________
        #Creates buttons for the folder selection
        observe({
          output$samples <- renderUI({
            num_samples <- input$num_samples
            br()
            samples <- lapply(seq_len(num_samples), function(i) {
             # actionButton(inputId = paste0("folder", i), label = paste0("Select folder ", i))
              div(
                actionButton(inputId = paste0("folder", i), label = paste0("Select folder ", i)),
                style = "margin-bottom: 10px;" # Ajusta el valor del margen según tus necesidades
              )
            })
            do.call(tagList, samples)
          })
        })
    
        
        # Initialize the path files list
         file_paths <- reactiveVal(list())
         save_path <- reactiveVal()
         single_file <- reactiveValues(file_name = NULL, path_file = NULL)
         console_messages <- reactiveVal(list(NULL))
         #resultados2 <- reactiveVal(data.frame())
    
        
        # Observes each button and updates the path list
        observe({
          num_samples <- input$num_samples
          for (i in num_samples) {
            btn_id <- paste0("folder", i)
            txt_id <- paste0("folder_text", i)
            observeEvent(input[[btn_id]], {
              path <- normalizePath(choose.dir(default = ".", caption = paste0("Select folder", i)))
              current_paths <- file_paths()
              current_paths[[i]] <- path
              file_paths(current_paths)
              toggleClass(btn_id, "boton-pulsado")
              output$info <- renderPrint({cat(paste0(file_paths() , " has been uploaded"))})
            })
          }
        })
    
        #Comprobar que valores de otro panel cambian en el mio    
        # output$info_other_panel <- renderPrint(
        # input$freqhr_button 
        # )
        
        #Save at the end in case user wants now regrets
        observeEvent(input$save_2, {
          save_path(normalizePath(choose.dir(caption = "Select the location where the results are going to be saved")))
          output$save2 <- renderPrint({cat(paste0("Results are available in: ", save_path()))})
          saveHRVindexes(Resultados_dataframe, save_path())
          #update_action_button(session, "save_2", disabled = TRUE)
        })
          
        observeEvent(input$save_yes, {
          save_path(normalizePath(choose.dir(caption = "Select the location where the results are going to be saved")))
          #save_path <- paste0("\"", save_path,"\"")
          output$save <- renderPrint({cat(paste0("Results will be available in: ", save_path()))})
        })
        
        observeEvent(input$save_no, {
          output$save <- renderPrint(cat("No copy will be done"))
          #output$save <- renderPrint({cat(paste0("Results will be available in: ", save_path))})
        })
        
        observeEvent(input$RHRV, {
         # output$info_multiple_analysis <-renderPrint({
          
            # resultados <- RHRVEasy(file_paths(), correction = TRUE, correctionMethod = input$correction_method_selection, verbose= TRUE,
            #                      format = input$type_of_file, typeAnalysis = input$frequency_type_selection,
            #                      significance_level = input$significance_level, nonLinear=FALSE, 
            #                      # saveHRVindexesInPath = save_path,
            #                      size = input$window_size_button,
            #                      #class = input$frequency_method_selection,
            #                      freqhr = input$freqhr_button )
            #, saveHRVindexesInPath = save_path

            #resultados <- RHRVEasy(file_paths(), input$significance_level, size = input$window_size_button, saveHRVindexesInPath = save_path)
            #file_paths <- gsub('\ ', "/", file_paths())
            
            #update_action_button(session, "save_2", disabled = TRUE)
            # update_action_button(session, "save_yes", disable = TRUE)
            # update_action_button(session, "save_no", disable = TRUE)
          
           # calculatePowerBand <-function (HRVData, indexFreqAnalysis = length(HRVData$FreqAnalysis), 
           #          size, shift, sizesp = NULL, scale = "linear", ULFmin = 0, 
           #          ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05, LFmin = 0.05, 
           #          LFmax = 0.15, HFmin = 0.15, HFmax = 0.4, 
           #          type = c("fourier","wavelet"), wavelet = "d4", bandtolerance = 0.01, relative = FALSE, 
           #          verbose = NULL) 
           #  
           #  lo llama desde fourier
           #  calculatePSD <-function (HRVData, indexFreqAnalysis = length(HRVData$FreqAnalysis), 
           #                           method = c("pgram", "ar", "lomb"), doPlot = T, ...) 
           #  
           #  Lo llama desde fourier  
           #  InterpolateNIHR <-function (HRVData, freqhr = 4, method = c("linear", "spline")
          
            path_save <- save_path()
            resultados_dataframe <- RHRVEasy(folders = file_paths(), 
                                             #correction = TRUE ??PONEMOS FALSE CUANDO COJA NONE??
                                             #correctionMethod = input$correction_method_selection,
                                             format = input$format_check,
                                             #typeAnalysis = input$frequency_type_selection,
                                             saveHRVindexesInPath = path_save, 
                                             #significance_level = input$significance_level,
                                             #Time analysis arguments
                                             #size = input$window_size_button,
                                             #interval = input$interval_size_button,
                                             
                                             
                                             # #Freq analysis arguments
                                             # freqhr = input$freqhr_button,
                                             # 
                                             # #Tengo dudas del metodo porque cuando llama desde fourier metodo es para fourier o wavelet
                                             # #method = input$frequency_method_selection,
                                             # #En la funcion de wavelet, se llama tipo
                                             # type = input$frequency_method_selection,
                                             # 
                                             # 
                                             # #CalculatePSD
                                             # #method = input$fourier_method_selection,
                                             # 
                                             # ULFmin = input$ULFmin, 
                                             # ULFmax = input$ULFmax, 
                                             # VLFmin = input$VLFmin, 
                                             # VLFmax = input$VLFmax,
                                             # LFmin = input$LFmin, 
                                             # LFmax = input$LFmax, 
                                             # HFmin = input$HFmin, 
                                             # HFmax = input$HFmax,
                                             # 
                                             # #Wavelet arguments
                                             # wavelet = input$wavelet_method_selection,
                                             #bandtolerance = input$band_tolerance_button
                                             )
            
            assign("Resultados_dataframe", resultados_dataframe, envir = .GlobalEnv)
            resultados_texto <- capture.output(resultados_dataframe)
            #console_messages(c(console_messages, capture.output({RHRVEasy(file_paths(), input$significance_level)})))
            assign("Resultados_texto", resultados_texto, envir = .GlobalEnv)
            # resultados[resultados != ""]
            output$table_RHRV_analysis <- renderTable(
             Resultados_texto[Resultados_texto != ""]
            # #resultados[resultados != ""]
            )
            
            # if(is.NULL(path_save)){
            #   update_action_button(session, "save_2", disable = FALSE)
            # }
            # update_action_button(session, "save_yes", disable = FALSE)
            # update_action_button(session, "save_no", disable = FALSE)
            
            # output$RHRV_results <- renderPrint(
            #   resultados,
            # )
          
            })
        
          
        #})
    

#__MOVE_BUTTONS_________________________________________________________________    
        observeEvent(input$settings_button, {
          updateTabsetPanel(session, "tabset", selected = "Configuration")
        })
        
        observeEvent(input$settings_button2, {
          updateTabsetPanel(session, "tabset", selected = "Configuration")
        })    
    

    
    
#__SINGLE_FILE_ANALYSIS_____________________________________________________
      observeEvent(input$file_selector, {
        path <- normalizePath(choose.files(caption = "Select the file to study"))
        single_file$file_name <- basename(path) 
        single_file$path_file <- dirname(path)
        output$single_file_info <- renderPrint(
          cat(paste0("The file ", single_file$file_name, " has been uploaded from ", single_file$path_file))
        )
      })
   
        
        
#__TIME ANALYSIS____________________________________________________________
    output$config_time_selection <- renderPrint(
      cat(paste0("Time analysis with a window size: ", input$window_size_button, " and bin interval size: ", input$interval_size_button))  
    )

     
    observeEvent(input$Analyze_Time_Button, {
      if(is.null(single_file$file_name)){
        output$info_file_selection <- renderPrint(cat("Please select a file to study"))
        observeEvent(input$file_selector, {output$info_file_selection <- renderPrint(cat(" "))})
      } else {
        hrv.data = preparing_analysis(file = single_file$file_name, rrs =  single_file$path_file, format = input$type_of_file)
        file_data = time_analysis(format = input$type_of_file , file = single_file$file_name, size = input$window_size_button, class = input$frequency_method_selection, rrs2 =  single_file$path_file) 
        #Plot the file in the load data file
        output$plot_time_analysis <- renderPlot({ PlotNIHR(hrv.data)  })
        #Shows the table with the time analysis
        output$table_time_analysis <-  renderTable({
          nombres <- names(file_data)
          table_results <- c(nombres)
          table_results = rbind(table_results, file_data)
          t(table_results)
          }, class = "mi-columna")
      }  
    })
    
    
    
#__FREQUENCY ANALYSIS______________________________________________________________
    output$config_fourier_selection <- renderPrint(
      cat(paste0("Fourier analysis with frequency size: ", input$freqhr_button, ", method: ", input$frequency_method_selection)),
      #cat(paste0("Values. ULFmin: ", input$ULFmin, ", ULFmax: ", input$ULFmax, ", VLFmin: ", input$VLFmin, ", VLFmax: ", input$VLFmax, ", LFmin: ", input$LFmin, ", LFmax: ", input$LFmax, ", HFmin: ", input$HFmin, ", HFmax: ", input$HFmax))  
    )
    
    observeEvent(input$Analyze_Fourier_Button, {
      if(is.null(single_file$file_name)){
        output$info_freq_selection <- renderPrint(cat("Please select a file to study"))
        observeEvent(input$file_selector, {output$info_freq_selection <- renderPrint(cat(" "))})
      } else {
        hrv.data = preparing_analysis(file = single_file$file_name, rrs =  single_file$path_file, format = input$type_of_file)
        # file_data = freq_analysis(format = input$type_of_file, files = single_file$file_name, 
        #                           class = input$frequency_method_selection, rrs2 = single_file$path_file, 
        #                           freqhr = input$freqhr_button, 
                                  
        
        file_data = freq_analysis(format = input$type_of_file, files = single_file$file_name, 
                                  class = input$frequency_method_selection, rrs2 = single_file$path_file, 
                                  freqhr = input$freqhr_button,
                                  ULFmin = input$ULFmin, ULFmax = input$ULFmax, 
                                  VLFmin = input$VLFmin, VLFmax = input$VLFmax,
                                  LFmin = input$LFmin, LFmax = input$LFmax, 
                                  HFmin = input$HFmin, HFmax = input$HFmax)
        
        #Plot the file in the load data file
        output$plot_freq_analysis <- renderPlot({PlotNIHR(hrv.data) })
        
        #Print the name of the file and its datapath
        output$info_freq_analysis <-  renderPrint({cat(paste0("The file ",  basename(file), " has been uploaded"))})
        
        #Shows the table with the time analysis
        output$table_freq_analysis <-  renderTable({ 
          nombres <- names(file_data)
          table_results <- c(nombres)
          table_results = rbind(table_results, file_data)
          t(table_results)
          })
     }
  })
    
    
#__WAVELETS ANALYSIS______________________________________________________________
    output$config_wave_selection <- renderPrint(
      cat(paste0("Wavelet analysis with frequency size: ", input$freqhr_button, ", method: ", input$frequency_method_selection)),
      #cat(paste0("Band tolerance: ", input$band_tolerance_button))
    )
    
    observeEvent(input$Analyze_Wave_Button, {
      if(is.null(single_file$file_name)){
        output$info_wave_selection <- renderPrint(cat("Please select a file to study"))
        observeEvent(input$file_selector, {output$info_wave_selection <- renderPrint(cat(" "))})
      } else {
        hrv.data = preparing_analysis(file = single_file$file_name, rrs =  single_file$path_file, format = input$type_of_file)
        file_data = freq_analysis(format = input$type_of_file, file = single_file$file_name, size = input$window_size_button, 
                                  class = input$frequency_method_selection, rrs2 =  single_file$path_file)
      
      #Plot the file in the load data file
      output$plot_wave_analysis <- renderPlot({PlotNIHR(hrv.data) })
      
      #Print the name of the file and its datapath
      output$info_wave_analysis <-  renderPrint({ cat(paste0("The file ",  basename(file), " has been uploaded")) })
      
      #Shows the table with the time analysis
      output$table_wave_analysis <-  renderTable({
        nombres <- names(file_data)
        table_results <- c(nombres)
        table_results = rbind(table_results, file_data)
        t(table_results)
      })
      }  
    })
    
    
#Observes buttons in ShinyAppRestrictions.R
    observeEvent(input$ULFmin, {settings_restrictions(session, input)})
    observeEvent(input$ULFmax, {settings_restrictions(session, input)})
    observeEvent(input$VLFmin, {settings_restrictions(session, input)})
    observeEvent(input$VLFmax, {settings_restrictions(session, input)})
    observeEvent(input$LFmin, {settings_restrictions(session, input)})
    observeEvent(input$LFmax, {settings_restrictions(session, input)})
    observeEvent(input$HFmin, {settings_restrictions(session, input)})
    observeEvent(input$HFmax, {settings_restrictions(session, input)})
    
  
  }
    shinyApp(ui = ui, server = server)
  
}

# resultaditos <- RHRVEasy(folders = c("../Desktop/RHRVEasy-master/rrs/normal/", "../Desktop/RHRVEasy-master/rrs/chf/"), size = 250, freqhr = 6, saveHRVindexesInPath = "../Desktop/RHRVEasy-master/rrs/")
# save_path <- "../Desktop/RHRVEasy-master/rrs/"
# saveHRVindexes(resultaditos, save_path)


'''
EXAMPLE OF STACK OVERFLOW
library(shiny) 
ui <- fluidPage(
  titlePanel("Stream the system output"),
  sidebarLayout(
    sidebarPanel(
      actionButton("btn_start",label = "Let's stream"),
      actionButton("btn_stop",label = "Stop")
    ),
    mainPanel(
      htmlOutput("textstream_output")
    )
  )
)
server <- function(input, output, session) {
  rv <- reactiveValues(textstream = c(""),
                       timer = reactiveTimer(1000),
                       started = FALSE)
  observeEvent(input$btn_start, { 
    rv$started <- TRUE
    system2("Rscript", "so_script.R", wait = FALSE)
  })
  observeEvent(input$btn_stop, { rv$started <- FALSE })
  observe({
    rv$timer()
    if (isolate(rv$started))
      rv$textstream <- paste(readLines("so_output.txt"), collapse = "<br/>")
  })
  output$textstream_output <- renderUI({
    HTML(rv$textstream)
  })
}
shinyApp(ui = ui, server = server)

cat('sink(file = "so_output.txt")
  for (i in 1:10) {
    cat(format(Sys.time(), format = "%H:%M:%S"), "\n")
    Sys.sleep(1)
  }
  cat("*** EOF ***\n")
  sink()
', file = "so_script.R")

