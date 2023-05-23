# Updates maximum and minimum values in frequencies so they can satify the following rule
# ULFmin < ULFmax < VLFmin < VLFmax < LFmin < LFmax < HFmin < HFmax

  
#__SETTINGS_____________________________________________________________________

settings_restrictions <- function(session, input) {
  observeEvent(input$ULFmin, {
    value_of_ULFmin <- input$ULFmin
    updateNumericInput(session, inputId = "ULFmax", min = value_of_ULFmin)
    # if(input$ULFmax <= input$ULFmin){
    #   updateNumericInput(session, inputId = "ULFmax", value = (value_of_ULFmin + 0.001))
    #   showNotification("ULFmin must be lower than ULFmax", duration = 3, closeButton = FALSE, type = "warning")
    # }
  })
  
  observeEvent(input$ULFmax, {
    value_of_ULFmax <- input$ULFmax
    updateNumericInput(session, inputId = "ULFmin", max = value_of_ULFmax)
    updateNumericInput(session, inputId = "VLFmin", min = value_of_ULFmax)
    
    # if(input$ULFmax <= input$ULFmin){
    #   updateNumericInput(session, inputId = "ULFmin", value = (value_of_ULFmax - 0.001))
    #   showNotification("ULFmax must be higher than ULFmin", duration = 3, closeButton = FALSE, type = "warning")
    # } else if(input$VLFmin < input$ULFmax){
    #   updateNumericInput(session, inputId = "VLFmin", value = (value_of_ULFmax))
    #   showNotification("ULFmax must be lower or equal than VLFmin", duration = 3, closeButton = FALSE, type = "warning")
    # }     
  })
  
  observeEvent(input$VLFmin, {
    value_of_VLFmin <- input$VLFmin
    updateNumericInput(session, inputId = "ULFmax", max = value_of_VLFmin)
    updateNumericInput(session, inputId = "VLFmax", min = value_of_VLFmin)
  })
  
  observeEvent(input$VLFmax, {
    value_of_VLFmax <- input$VLFmax
    updateNumericInput(session, inputId = "VLFmin", max = value_of_VLFmax)
    updateNumericInput(session, inputId = "LFmin", min = value_of_VLFmax)
  })
  
  observeEvent(input$LFmin, {
    value_of_LFmin <- input$LFmin
    updateNumericInput(session, inputId = "VLFmax", max = value_of_LFmin)
    updateNumericInput(session, inputId = "LFmax", min = value_of_LFmin)
  })
  
  observeEvent(input$LFmax, {
    value_of_LFmax <- input$LFmax
    updateNumericInput(session, inputId = "LFmin", max = value_of_LFmax)
    updateNumericInput(session, inputId = "HFmin", min = value_of_LFmax)
  })
  
  observeEvent(input$HFmin, {
    value_of_HFmin <- input$HFmin
    updateNumericInput(session, inputId = "LFmax", max = value_of_HFmin)
    updateNumericInput(session, inputId = "HFmax", min = value_of_HFmin)
  })
  
  observeEvent(input$HFmax, {
    value_of_HFmax <- input$HFmax
    updateNumericInput(session, inputId = "HFmin", max = value_of_HFmax)
  })
  
}