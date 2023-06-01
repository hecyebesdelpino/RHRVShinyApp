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
source("ShinyAppSingleFileAnalysis.R")

##USER##########################################################################
ui <- fluidPage(
    tabsetPanel( id = "tabset",
                 
     tags$style(HTML(code_style)),
     tags$script(HTML(code_script)),
     useShinyjs(),

      
      #_1_HOME___________________________________________________________________
      tabPanel(
        title = "Heart Rate Variability", 
        h1("Welcome to the HRV App"), 
        p("This app will allow you to obtain some graphical and statistical studies of your HRV samples"),
        p("Just go through the different tabs and try them all"),
        p("This app reads Ascii, RR, Ambit, Suunto and EDFPlus files"),
        p("If you have any problem, please contact us"),
      ),
      
     

      #_2_MULTIPLE FILES TAB_____________________________________________________
      tabPanel("Multiple Files Analysis", 
               sidebarLayout(
                 sidebarPanel( #width = 4
                   numericInput(inputId = "num_samples", label = "Number of samples", value = 2, min = 1, step = 1),
                   bsTooltip("num_samples", "Introduce the number of folders", placement = "right"),
                   selectInput("format_check", label = "Select the type of file", choices = c("RR","Ascii", "Polar", "Suunto", "EDFPlus", "Ambit"), selected = NULL),
                   actionButton("upload_files_button", "Upload files", class = "light-blue-button"),
                   uiOutput(outputId = "samples"),
                   textOutput("info"),
                   br(),
                   actionButton("settings_button", label = icon("cog")),
                   br(),
                 ),
                 

                 mainPanel(
                       br(),
                       numericInput("significance_level", "Significance level", value = 0.05, step = 0.001),
                       h3("Correction method"),
                       selectInput(inputId = "correction_method_selection", choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"), selected = "bonferroni",  label =  "Please select the correction method"),
                       textOutput("save_message"),
                       actionButton("save_yes", label = "Yes"),
                       actionButton("save_no", label = "No"),
                       textOutput("save"),
                       br(),
                       textOutput("info_other_panel"),
                       textOutput("loading"),
                       
                       conditionalPanel(
                         condition = ("input.save_yes > 0 || input.save_no > 0"),
                         actionButton("save_2", label =  icon("save")),
                         actionButton("RHRV", "RHRV study", class = "blue-button"),
                         textOutput("console_info"), #UNUSED
                         br(),
                       ),
                    
                      conditionalPanel(
                         condition = "input.RHRV > 0",
                         textOutput("RHRV_results"),
                         tableOutput("table_RHRV_analysis"),
                      ),
                      
                      textOutput("info_multiple_analysis")
                 )
             )       
      ),
     
     
     
     #_3_LINEAR ANALYSIS TAB_____________________________________________________
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
                  #TIME ANALYSIS________________________________________________
                  conditionalPanel(
                    condition = "input.linear_analysis_options == 'Time analysis'",
                    br(),
                    br(),
                    actionButton("Analyze_Time_Button", "Time analysis", class = "blue-button"),
                    textOutput("info_file_selection"),
                    br(),
                    plotOutput("plot_time_analysis"),
                    br(),
                    column(width = 5, offset = 3, tableOutput("table_time_analysis")),
                    br(),
                  ),
                  
                  #FREQUENCY ANALYSIS___________________________________________
                  conditionalPanel(
                    condition = "input.linear_analysis_options == 'Fourier analysis'",
                    br(),
                    br(),
                    actionButton("Analyze_Fourier_Button", "Fourier analysis", class = "blue-button"),
                    textOutput("info_freq_selection"),
                    br(),
                    plotOutput("plot_freq_analysis"),
                    br(),
                    column(width = 5, offset = 3, tableOutput("table_freq_analysis")),
                    br(),
                  ),
                  
                  #WAVELETS ANALYSIS____________________________________________
                  conditionalPanel(
                    condition = "input.linear_analysis_options == 'Wavelets analysis'",
                    br(),
                    br(),
                    actionButton("Analyze_Wave_Button", "Wavelet analysis", class = "blue-button"),
                    textOutput("info_wave_selection"),
                    br(),
                    plotOutput("plot_wave_analysis"),
                    br(),
                    column(width = 5, offset = 3, tableOutput("table_wave_analysis")),
                    br(),
                  )
                )
              )
     ), 
     
     
     
      #_4_Settings_________________________________________________________________
      tabPanel("Settings",
               column(width = 4,
               h3("Window configuration"),
                   column(width = 12,
                   numericInput("window_size_button", "Window size", value = 300, min = 1, max = 1000, step = 1),
                   bsTooltip("window_size_button", "The size of the window employed in time analysis. Default: 300 miliseconds", placement = "right"),
                   numericInput("interval_size_button", "Interval", value = 7.8125, step = 0.01),
                   bsTooltip("interval_size_button", "Bin width of the histogram. Default 7.8125", placement = "right"),
                   numericInput("window_shift_button", "Shift", value = 150, min = 1, max = 500, step = 1),
                   br(),
                   )
               ),

               column(width = 8,
               h3("Frequency configuration"),
                   column(width = 6,
                   numericInput("freqhr_button", "Frecuency size", value = 4, min = 1, step = 1),
                   bsTooltip("freqhr_button", "Frequency interpolation value. Default: 4 Hz", placement = "right"),
                   selectInput(inputId = "frequency_method_selection", c("linear", "spline"), label = "Select the interpolation method", selected = "spline"),
                   selectInput(inputId = "frequency_type_selection", c("fourier", "wavelet"), label = "Select the frequency type analysis", selected = "fourier"),
                   
                        conditionalPanel(
                             condition = "input.frequency_type_selection == 'fourier'",
                             br(),
                             selectInput(inputId = "fourier_method_selection", choices = c("ar", "lomb", "pgram"), label = "Select the fourier method", selected = "lomb"),
                         ),   
                        conditionalPanel(
                            condition = "input.frequency_type_selection == 'wavelet'",
                            br(),
                            selectInput(inputId = "wavelet_method_selection", choices = c("la8","la16","la20","d4","d6","d8","d16","bl14","bl20","fk4","fk6","fk8","fk14","fk22","mb4","mb8","mb16","mb24", "bs3.1"), label = "Select the wavelet type", selected = "d4"),
                            numericInput("band_tolerance_button", "Band tolerance", value = 0.100, min = 0.001, step = 0.001),
                            bsTooltip("band_tolerance_button", "Maximum acceptable error in the estimation of spectral bands", placement = "right"),
                            br()
                          ),
                   ),
               
                   column(width = 6,
                     #selectInput(inputId = "fourier_method_selection", choices = c("ar", "lomb", "pgram"), label = "Select the fourier method", selected = "lomb"),
                     numericInput("ULFmin", "ULFmin", value = 0.000, step = 0.001, min = 0),
                     bsTooltip("ULFmin", "Standard values<br/>from 0 to 0,03<br/>ULFmin < ULFmax", placement = "left"),
                     numericInput("ULFmax", "ULFmax", value = 0.030, step = 0.001),
                     bsTooltip("ULFmax", "Standard values<br/>from 0,03 to 0,05<br/>ULFmin < ULFmax < VLFmin", placement = "left"),
                     numericInput("VLFmin", "VLFmin", value = 0.030, step = 0.001),
                     bsTooltip("VLFmin", "Standard values<br/>from 0,03 to 0,05<br/>ULFmax < VLFmin < VLFmax", placement = "left"),
                     numericInput("VLFmax", "VLFmax", value = 0.050, step = 0.001),
                     bsTooltip("VLFmax", "Standard values<br/>from 0,05 to 0,15<br/>VLFmin < VLFmax < LFmin", placement = "left"),
                     numericInput("LFmin", "LFmin", value = 0.0500, step = 0.001),
                     bsTooltip("LFmin", "Standard values<br/>from 0,05 to 0,15<br/>VLFmax < LFmin < LFmax", placement = "left"),
                     numericInput("LFmax", "LFmax", value = 0.150, step = 0.001),
                     bsTooltip("LFmax", "Standard values<br/>from 0,15 to 0,4<br/>LFmin < LFmax < HFmin", placement = "left"),
                     numericInput("HFmin", "HFmin", value = 0.150, step = 0.001),
                     bsTooltip("HFmin", "Standard values<br/>from 0,15 to 0,4<br/>LFmax < HFmin < HFmax", placement = "left"),
                     numericInput("HFmax", "HFmax", value = 0.400, step = 0.001, max = 1.000),
                     bsTooltip("HFmax", "Standard values<br/>from 0,4 to 1<br/>HFmin < HFmax", placement = "left"),
                     textOutput("freq_intervals"),
                     br()
                   ),
              ),
              fixedRow(
                column(width = 3, offset = 1,
                       align = "left",
                       actionButton("restoreValues", "Restore", class = "green-button"),
                       bsTooltip("restoreValues", "Restore to default values in frequency intervals", placement = "right")
                )
              ),
        ),
  )
)
  
  
  
  
  
  
  
##SERVER########################################################################
server <- function(input, output, session) {
    
        # Initialize reactive Val
        file_paths <- reactiveVal(list())
        save_path <- reactiveVal()
        single_file <- reactiveValues(file_name = NULL, path_file = NULL)
        console_messages <- reactiveVal(list(NULL))
        num_samples <- reactiveVal(1)
        folder_info <- reactiveVal(list())
        restoreIntervalsClick <- reactiveVal(FALSE)
        hour_min <- reactiveVal(NULL)

  
  
  #_1_FILE UPLOAD_______________________________________________________________
        #Asks for folder/s location and stores in file_paths() list
        observeEvent(input$upload_files_button, {
          num_samples <- input$num_samples
            for (i in seq_len(num_samples)) {
              btn_id <- paste0("folder", i)
              path <- normalizePath(choose.dir(default = ".", caption = paste0("Select folder", i)))
              current_paths <- file_paths()
              current_paths[[i]] <- path
              file_paths(current_paths)
              toggleClass(btn_id, "boton-pulsado")
              output$info <- renderPrint({cat(paste0(file_paths() , " has been uploaded"))})
            }
          hide("upload_file_button")
          })
          
        #If the user changes the number of folders, file_paths() are removed and shows again
        #the upload button for uploading new files paths
        observeEvent(input$num_samples,{
          show(id="upload_file_button") 
          file_paths(list())
          output$info <- renderPrint({cat(paste0("No files selected"))})
        })
   
        
             
    
  #_2_SAVE EXCEL WITH HRV INDEXES_______________________________________________
        output$save_message <- renderText("When the analysis finishes, do you want to save a copy of the indexes?")
        
        #YES____________________________________________________________________
        observeEvent(input$save_yes, {
          hide(id= "save_2")
          save_path(normalizePath(choose.dir(caption = "Select the location where the results are going to be saved")))
          output$save <- renderPrint({cat(paste0("Results will be available in: ", save_path()))})
        })
        
        #NO_____________________________________________________________________
        observeEvent(input$save_no, {
          hide(id= "save_2")
          output$save <- renderPrint(cat("No copy will be done"))
        })
        
        #Save after computing the analysis (usefull for extra copies or in case user said no at first time)
        observeEvent(input$save_2, {
          save_path(normalizePath(choose.dir(caption = "Select the location where the results are going to be saved")))
          output$save <- renderPrint({cat(paste0("Results are available in: ", save_path()))})
          saveHRVindexes(Resultados_dataframe, save_path())
        })
        
        
        
        
  #_3__MOVEMENTS BUTTONS________________________________________________________
        observeEvent(input$settings_button, {
          updateTabsetPanel(session, "tabset", selected = "Settings")
        })
        
        observeEvent(input$settings_button2, {
          updateTabsetPanel(session, "tabset", selected = "Settings")
        })         
  
        
              
        
  #_4_RHRVEASY_ANALYSIS_________________________________________________________
        #Shows message when loading, it would be ideal to show the console real-time, When RHRV is working can´t show sys.time()
        output$loading <- renderPrint({
          cat("Calculating.... Please wait, it could take some minutes ")
          #Con esto va mostrando mensaje cada 5 segundos, el probema que cuando está en uso la consola este se bloquea
          #invalidateLater(1000)  # Actualizar cada segundo (1000 ms)
          #cat(paste0("Calculating.... Please wait, it could take some minutes. Updated: ", format(Sys.time(), "%H:%M:%S")))
          hide(id = "loading")
        })

        #The analysis___________________________________________________________
        observeEvent(input$RHRV, {  
          len <- length(file_paths())
          if(len == 0 ){
            showNotification("Please upload files", duration = 3, type = "error")
            
          } else if(len == 1){
            showNotification("Please select more than 1 file for multiple analysis. For performing single file analysis go to the next tab", duration = 4, type = "warning")
            
          } else {
            show(id = "loading")
            hide(id = "save_yes")
            hide(id= "save_no")
            hide(id = "RHRV")
            hide(id = "save")
            hide(id = "save_2")
            hide(id = "save_message")
            hide(id = "upload_files_button")
            path_save <- save_path()
            resultados_dataframe <- RHRVEasy(folders = file_paths(),
                                             correctionMethod = input$correction_method_selection,
                                             format = input$format_check,
                                             typeAnalysis = input$frequency_type_selection,
                                             saveHRVindexesInPath = path_save,
                                             significance_level = input$significance_level,
                                             size = input$window_size_button,
                                             interval = input$interval_size_button,
                                             freqhr = input$freqhr_button,
                                             type = input$frequency_method_selection,
                                             #method = input$frequency_method_selection,
                                             #method2 = input$fourier_method_selection,
                                             ULFmin = input$ULFmin,
                                             ULFmax = input$ULFmax,
                                             VLFmin = input$VLFmin,
                                             VLFmax = input$VLFmax,
                                             LFmin = input$LFmin,
                                             LFmax = input$LFmax,
                                             HFmin = input$HFmin,
                                             HFmax = input$HFmax,
                                             wavelet = input$wavelet_method_selection,
                                             bandtolerance = input$band_tolerance_button)
            #Saves results in global environment so they could be later used
            assign("Resultados_dataframe", resultados_dataframe, envir = .GlobalEnv)
            resultados_texto <- capture.output(resultados_dataframe)
            #Saves the console output when results are displayed
            assign("Resultados_texto", resultados_texto, envir = .GlobalEnv)
            #Calls a function in (SingleFileAnalysis.R) which treats the dataframe to show to the user in a more visual way
            results_matrix = multiple_results(resultados_dataframe)
            #Once analysis its finished, user can do another one
            show(id = "save_2")
            show(id = "RHRV")
            show(id = "save")
            show(id = "upload_files_button")
            hide(id = "loading")
            #Displays the multiple file table results
            output$table_RHRV_analysis <- renderTable(results_matrix[-1,])
          }
        })
        


  #_5_SINGLE_FILE_ANALYSIS________________________________________________________
        
        #Single file upload_____________________________________________________
        observeEvent(input$file_selector, {
            path <- normalizePath(choose.files(caption = "Select the file to study"))
            single_file$file_name <- basename(path) 
            single_file$path_file <- dirname(path)
            output$single_file_info <- renderPrint(
              cat(paste0("The file ", single_file$file_name, " has been uploaded from ", single_file$path_file))
            )
        })
   

        
        #TIME ANALYSIS__________________________________________________________
        observeEvent(input$Analyze_Time_Button, {
          if(is.null(single_file$file_name)){
            output$info_file_selection <- renderPrint(cat("Please select a file to study"))
            observeEvent(input$file_selector, {output$info_file_selection <- renderPrint(cat(" "))})
          } else {
            #Calls time_single_analysis function in "ShinyAppSingleFileAnalysis"
            hrv.data <- time_single_analysis(fileType = input$type_of_file, 
                                             Recordname = single_file$file_name, 
                                             RecordPath = single_file$path_file, 
                                             size = input$window_size_button, 
                                             interval = input$interval_size_button, 
                                             freqhr = input$freqhr_button, 
                                             method = input$frequency_method_selection)
            #Plot the file in the load data file
            output$plot_time_analysis <- renderPlot({PlotHR(hrv.data)})
            #Shows the table with the time analysis
            output$table_time_analysis <-  renderTable({
               TIME <- c("Size",	"SDNN",	"SDANN",	"SDNNIDX", "pNN50",	"SDSD",	"rMSSD", "IRRR",	"MADRR",	"TINN",	"HRVi")
               VALUES <- c(hrv.data$TimeAnalysis[[1]]$size,hrv.data$TimeAnalysis[[1]]$SDNN,hrv.data$TimeAnalysis[[1]]$SDANN,hrv.data$TimeAnalysis[[1]]$SDNNIDX,hrv.data$TimeAnalysis[[1]]$pNN50,hrv.data$TimeAnalysis[[1]]$SDSD,
                           hrv.data$TimeAnalysis[[1]]$rMSSD,hrv.data$TimeAnalysis[[1]]$IRRR,hrv.data$TimeAnalysis[[1]]$MADRR,hrv.data$TimeAnalysis[[1]]$TINN,hrv.data$TimeAnalysis[[1]]$HRVi)
               results <- cbind(TIME, VALUES)
               results
              })
          }  
        })
    
    
    
        #__FREQUENCY ANALYSIS___________________________________________________
        observeEvent(input$Analyze_Fourier_Button, {
          if(is.null(single_file$file_name)){
            output$info_freq_selection <- renderPrint(cat("Please select a file to study"))
            observeEvent(input$file_selector, {output$info_freq_selection <- renderPrint(cat(" "))})
          } else {
            #Calls fourier_single_analysis function in "ShinyAppSingleFileAnalysis"
            hrv.data = fourier_single_analysis(fileType = input$type_of_file, 
                                               Recordname = single_file$file_name, 
                                               RecordPath = single_file$path_file,
                                               freqhr = input$freqhr_button, 
                                               method = input$frequency_method_selection, 
                                               size = input$window_size_button, 
                                               shift = input$window_shift_button, 
                                               ULFmin = input$ULFmin, 
                                               ULFmax = input$ULFmax, 
                                               VLFmin = input$VLFmin, 
                                               VLFmax = input$VLFmax, 
                                               LFmin = input$LFmin, 
                                               LFmax = input$LFmax,
                                               HFmin = input$HFmin, 
                                               HFmax = input$HFmax, 
                                               type = "fourier")
            #Plot the file in the load data file
            output$plot_freq_analysis <- renderPlot({PlotPowerBand(hrv.data, ymax=200, ymaxratio = 1.7)})
            #Print the name of the file and its datapath
            output$info_freq_analysis <-  renderPrint({cat(paste0("The file ",  basename(file), " has been uploaded"))})
            #Shows the table with the time analysis
            output$table_freq_analysis <-  renderTable({ 
              FOURIER <- c("File", "Method", "ULFmin", "ULFmax", "VLFmin", "VLFmax", "LFmin", "LFmax", "HFmin", "HFmax", "Size", "Shift")
              VALUES <- c(single_file$file_name,input$frequency_method_selection, 
                          input$ULFmin, input$ULFmax, input$VLFmin, input$VLFmax,input$LFmin,input$LFmax,input$HFmin,input$HFmax,
                          input$window_size_button, input$window_shift_button)
              results <- cbind(FOURIER, VALUES)
              results
              })
         }
        })
    
        
    
        #__WAVELETS ANALYSIS______________________________________________________________
        observeEvent(input$Analyze_Wave_Button, {
          if(is.null(single_file$file_name)){
            output$info_wave_selection <- renderPrint(cat("Please select a file to study"))
            observeEvent(input$file_selector, {output$info_wave_selection <- renderPrint(cat(" "))})
          } else {
            #Calls wavelet_single_analysis function in "ShinyAppSingleFileAnalysis"
            hrv.data = wavelet_single_analysis(fileType = input$type_of_file, 
                                               Recordname = single_file$file_name, 
                                               RecordPath = single_file$path_file,
                                               freqhr = input$freqhr_button, 
                                               method = input$frequency_method_selection, 
                                               size = input$window_size_button,
                                               shift = input$window_shift_button, 
                                               ULFmin = input$ULFmin, 
                                               ULFmax = input$ULFmax, 
                                               VLFmin = input$VLFmin,
                                               VLFmax = input$VLFmax, 
                                               LFmin = input$LFmin, 
                                               LFmax = input$LFmax, 
                                               HFmin = input$HFmin, 
                                               HFmax = input$HFmax,
                                               type = "wavelet", 
                                               wavelet = input$wavelet_method_selection, 
                                               bandtolerance = input$band_tolerance_button)
          #Plot the file in the load data file
          output$plot_wave_analysis <- renderPlot({PlotPowerBand(hrv.data, ymax=700, ymaxratio = 50)})
          #Print the name of the file and its datapath
          output$info_wave_analysis <-  renderPrint({ cat(paste0("The file ",  basename(file), " has been uploaded")) })
          #Shows the table with the time analysis
          output$table_wave_analysis <-  renderTable({
            WAVELET <- c("File", "Bandtolerance", "Wavelet", "Method", "ULFmin", "ULFmax", "VLFmin", "VLFmax", "LFmin", "LFmax", "HFmin", "HFmax", "Size", "Shift")
            VALUES <- c(single_file$file_name, input$band_tolerance_button, input$wavelet_method_selection,input$frequency_method_selection, 
                             input$ULFmin, input$ULFmax, input$VLFmin, input$VLFmax,input$LFmin,input$LFmax,input$HFmin,input$HFmax,
                             input$window_size_button, input$window_shift_button)
            results <- cbind(WAVELET, VALUES)
            results
          })
          }  
        })
    
        
        

  #_6_VALUES_RESTRICTIONS_________________________________________________________
        #Observes buttons in ShinyAppRestrictions.R
        observeEvent(input$ULFmin, {settings_restrictions(session, input)})
        observeEvent(input$ULFmax, {settings_restrictions(session, input)})
        observeEvent(input$VLFmin, {settings_restrictions(session, input)})
        observeEvent(input$VLFmax, {settings_restrictions(session, input)})
        observeEvent(input$LFmin, {settings_restrictions(session, input)})
        observeEvent(input$LFmax, {settings_restrictions(session, input)})
        observeEvent(input$HFmin, {settings_restrictions(session, input)})
        observeEvent(input$HFmax, {settings_restrictions(session, input)})
        observeEvent(input$window_size_button, {settings_restrictions(session, input)})
        observeEvent(input$window_shift_button, {settings_restrictions(session, input)})
        observeEvent(input$interval_size_button, {settings_restrictions(session, input)})
        observeEvent(input$band_tolerance_button, {settings_restrictions(session, input)})
        observeEvent(input$freqhr_button, {settings_restrictions(session, input)})
        observeEvent(input$num_samples, {settings_restrictions(session, input)})
        observeEvent(input$significance_level, {settings_restrictions(session, input)})
        
        
        #Button for restoring default values    
        observeEvent(input$restoreValues, {
          if (!restoreIntervalsClick()) {
            restoreIntervalsClick(TRUE)
            updateNumericInput(session, inputId = "ULFmin", value = 0.000)
            updateNumericInput(session, inputId = "ULFmax", value = 0.030)
            updateNumericInput(session, inputId = "VLFmin", value = 0.030)
            updateNumericInput(session, inputId = "VLFmax", value = 0.050)
            updateNumericInput(session, inputId = "LFmin", value = 0.050)
            updateNumericInput(session, inputId = "LFmax", value = 0.150)
            updateNumericInput(session, inputId = "HFmin", value = 0.150)
            updateNumericInput(session, inputId = "HFmax", value = 0.400)
            updateNumericInput(session, inputId = "window_size_button", value = 300)
            updateNumericInput(session, inputId = "window_shift_button", value = 150)
            updateNumericInput(session, inputId = "interval_size_button", value = 7.8125)
            updateNumericInput(session, inputId = "band_tolerance_button", value = 0.1)
            updateNumericInput(session, inputId = "freqhr_button", value = 4)
          } else {
            restoreIntervalsClick(FALSE)
          }
         })
}


shinyApp(ui = ui, server = server)
  
}





