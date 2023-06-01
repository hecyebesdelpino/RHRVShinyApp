# Updates maximum and minimum values in frequencies so they can satify the following rule
# ULFmin < ULFmax < VLFmin < VLFmax < LFmin < LFmax < HFmin < HFmax

  
#__SETTINGS_____________________________________________________________________

settings_restrictions <- function(session, input) {
  
      observeEvent(input$ULFmin, {
        value_of_ULFmin <- input$ULFmin
        tryCatch({
        if(value_of_ULFmin < 0 || value_of_ULFmin > 0.396 ){
          showNotification("ULFmin must have values around 0", duration = 3, closeButton = FALSE, type = "warning")
          updateNumericInput(session, inputId = "ULFmin", value = 0)
    
    
        } else{
        if(input$ULFmax <= input$ULFmin){
          showNotification("ULFmin must be lower than ULFmax", duration = 3, closeButton = FALSE, type = "warning")
          updateNumericInput(session, inputId = "ULFmax", value = (value_of_ULFmin + 0.001))
    
        }}
        }, error = function(e) {
         # showNotification("Please use commas (,) instead of points (.)", type = "error")
        })
      })
    
    
      observeEvent(input$ULFmax, {
        value_of_ULFmax <- input$ULFmax
        tryCatch({
          if(value_of_ULFmax < 0.001 || value_of_ULFmax > 0.397 ){
            updateNumericInput(session, inputId = "ULFmax", value = 0.03)
            showNotification("ULFmax must have values around 0.03", duration = 3, closeButton = FALSE, type = "warning")
          } else{
              if(input$ULFmax == input$ULFmin){
                updateNumericInput(session, inputId = "ULFmin", value = (value_of_ULFmax - 0.001))
                showNotification("ULFmax must be higher than ULFmin", duration = 3, closeButton = FALSE, type = "warning")
              }else if(input$ULFmax > input$VLFmin){
                #updateNumericInput(session, inputId = "ULFmax", value = (value_of_ULFmax))
                updateNumericInput(session, inputId = "VLFmin", value = (value_of_ULFmax))
                showNotification("ULFmax must be lower or equal than VLFmin", duration = 3, closeButton = FALSE, type = "warning")
              }}
          #updateNumericInput(session, inputId = "ULFmax", value = (value_of_ULFmax))
          }, error = function(e) {
            # showNotification("Please use commas (,) instead of points (.)", type = "error")
          })
      })
    
      observeEvent(input$VLFmin, {
        value_of_VLFmin <- input$VLFmin
        tryCatch({
          if(value_of_VLFmin < 0.001 || value_of_VLFmin > 0.397 ){
            updateNumericInput(session, inputId = "VLFmin", value = 0.03)
            showNotification("VLFmin must have values around 0.03", duration = 3, closeButton = FALSE, type = "warning")
          } else{
            if(input$VLFmin < input$ULFmax){
              #updateNumericInput(session, inputId = "VLFmin", value = (value_of_VLFmin))
              updateNumericInput(session, inputId = "ULFmax", value = (value_of_VLFmin))
              showNotification("ULFmax must be lower or equal than VLFmin", duration = 3, closeButton = FALSE, type = "warning")
            }else if(input$VLFmin == input$VLFmax){
              updateNumericInput(session, inputId = "VLFmax", value = (value_of_VLFmin + 0.001))
              showNotification("VLFmax must be higher than VLFmin", duration = 3, closeButton = FALSE, type = "warning")
            }}
         # updateNumericInput(session, inputId = "VLFmin", value = (value_of_VLFmin))
        }, error = function(e) {
          # showNotification("Please use commas (,) instead of points (.)", type = "error")
        })
      })
    
      
      observeEvent(input$VLFmax, {
        value_of_VLFmax <- input$VLFmax
        tryCatch({
          if(value_of_VLFmax < 0.002 || value_of_VLFmax > 0.398 ){
            updateNumericInput(session, inputId = "VLFmax", value = 0.05)
            showNotification("VLFmax must have values around 0.05", duration = 3, closeButton = FALSE, type = "warning")
          } else{
            if(input$VLFmax == input$VLFmin){
              updateNumericInput(session, inputId = "VLFmin", value = (value_of_VLFmax - 0.001))
              showNotification("VLFmax must be higher than VLFmin", duration = 3, closeButton = FALSE, type = "warning")
            }else if(input$VLFmax > input$LFmin){
              updateNumericInput(session, inputId = "LFmin", value = (value_of_VLFmax))
              showNotification("VLFmax must be lower or equal than LFmin", duration = 3, closeButton = FALSE, type = "warning")
            }}
        }, error = function(e) {
          # showNotification("Please use commas (,) instead of points (.)", type = "error")
        })
      })
      
      
      
      observeEvent(input$LFmin, {
        value_of_LFmin <- input$LFmin
        tryCatch({
          if(value_of_LFmin < 0.002 || value_of_LFmin > 0.398 ){
            updateNumericInput(session, inputId = "LFmin", value = 0.05)
            showNotification("LFmin must have values around 0.05", duration = 3, closeButton = FALSE, type = "warning")
          } else{
            if(input$LFmin < input$VLFmax){
              updateNumericInput(session, inputId = "VLFmax", value = (value_of_LFmin))
              showNotification("VLFmax must be lower or equal than LFmin", duration = 3, closeButton = FALSE, type = "warning")
            }else if(input$LFmin == input$LFmax){
              updateNumericInput(session, inputId = "LFmax", value = (value_of_LFmin + 0.001))
              showNotification("LFmax must be higher than LFmin", duration = 3, closeButton = FALSE, type = "warning")
            }}
        }, error = function(e) {
          # showNotification("Please use commas (,) instead of points (.)", type = "error")
        })
      })
      
    
      
      observeEvent(input$LFmax, {
        value_of_LFmax <- input$LFmax
        tryCatch({
          if(value_of_LFmax < 0.003 || value_of_LFmax > 0.399 ){
            updateNumericInput(session, inputId = "LFmax", value = 0.15)
            showNotification("LFmax must have values around 0.15", duration = 3, closeButton = FALSE, type = "warning")
          } else{
            if(input$LFmax == input$LFmin){
              updateNumericInput(session, inputId = "LFmin", value = (value_of_LFmax - 0.001))
              showNotification("LFmax must be higher than LFmin", duration = 3, closeButton = FALSE, type = "warning")
            }else if(input$LFmax > input$HFmin){
              updateNumericInput(session, inputId = "HFmin", value = (value_of_LFmax))
              showNotification("LFmax must be lower or equal than HFmin", duration = 3, closeButton = FALSE, type = "warning")
            }}
        }, error = function(e) {
          # showNotification("Please use commas (,) instead of points (.)", type = "error")
        })
      })
      
    
      observeEvent(input$HFmin, {
        value_of_HFmin <- input$HFmin
        tryCatch({
          if(value_of_HFmin < 0.003 || value_of_HFmin > 0.399 ){
            updateNumericInput(session, inputId = "HFmin", value = 0.15)
            showNotification("HFmin must have values around 0.15", duration = 3, closeButton = FALSE, type = "warning")
          } else{
            if(input$HFmin < input$LFmax){
              updateNumericInput(session, inputId = "LFmax", value = (value_of_HFmin))
              showNotification("LFmax must be lower or equal than HFmin", duration = 3, closeButton = FALSE, type = "warning")
            }else if(input$HFmin == input$HFmax){
              updateNumericInput(session, inputId = "HFmax", value = (value_of_HFmin + 0.001))
              showNotification("HFmax must be higher than HFmin", duration = 3, closeButton = FALSE, type = "warning")
            }}
        }, error = function(e) {
          # showNotification("Please use commas (,) instead of points (.)", type = "error")
        })
      })
      
      observeEvent(input$HFmax, {
        value_of_HFmax <- input$HFmax
        updateNumericInput(session, inputId = "HFmin", max = value_of_HFmax)
      })
      
      observeEvent(input$HFmax, {
        value_of_HFmax <- input$HFmax
        tryCatch({
          if(value_of_HFmax < 0.004 || value_of_HFmax > 0.5 ){
            updateNumericInput(session, inputId = "HFmax", value = 0.4)
            showNotification("HFmax must have values around 0.4", duration = 3, closeButton = FALSE, type = "warning")
          } else{
            if(input$HFmax <= input$HFmin ){
              updateNumericInput(session, inputId = "HFmin", value = (value_of_HFmax -  0.001))
              showNotification("HFmin must be lower than HFmax", duration = 3, closeButton = FALSE, type = "warning")
            }}
        }, error = function(e) {
          # showNotification("Please use commas (,) instead of points (.)", type = "error")
        })
      })
      
      observeEvent(input$window_size_button, {
        value_of_window <- input$window_size_button
        value_integer <- as.integer(value_of_window)
        updateNumericInput(session, inputId = "window_size_button", value = value_integer)
        tryCatch({
          if(value_of_window < 1){
            updateNumericInput(session, inputId = "window_size_button", value = 300)
            showNotification("Please, window size must be higher than 0", duration = 3, closeButton = FALSE, type = "warning")
          }
        }, error = function(e) {
        })
      }
      )
    
      observeEvent(input$window_shift_button, {
      value_of_shift <- input$window_shift_button
      value_integer <- as.integer(value_of_shift)
      updateNumericInput(session, inputId = "window_shift_button", value = value_integer)
      value_of_window <- input$window_size_button
      tryCatch({
        if(value_of_shift < 1 || value_of_shift > value_of_window ){
          updateNumericInput(session, inputId = "window_shift_button", value = 150)
          showNotification("Please, shift must be higher than 0 and lower than window size", duration = 3, closeButton = FALSE, type = "warning")
        }
      }, error = function(e) {
      })
    }
    )
      
      observeEvent(input$interval_size_button, {
        value_of_interval <- input$interval_size_button
        tryCatch({
          if(value_of_interval < 0){
            updateNumericInput(session, inputId = "interval_size_button", value = 7.8125)
            showNotification("Please, interval must be higher than 0", duration = 3, closeButton = FALSE, type = "warning")
          } else{
            updateNumericInput(session, inputId = "interval_size_button", value = value_of_interval)
          }
        }, error = function(e) {
        })
      }
      )
  
      
      observeEvent(input$band_tolerance_button, {
        value_of_band <- input$band_tolerance_button
        tryCatch({
          if(value_of_band < 0){
            updateNumericInput(session, inputId = "band_tolerance_button", value = 0.1 )
            showNotification("Please, band tolerance must be higher than 0", duration = 3, closeButton = FALSE, type = "warning")
          } else{
            updateNumericInput(session, inputId = "band_tolerance_button", value = value_of_band)
          }
        }, error = function(e) {
        })
      }
      )
      
      observeEvent(input$freqhr_button, {
        value_of_freq <- input$freqhr_button
        tryCatch({
          if(value_of_freq < 0){
            updateNumericInput(session, inputId = "freqhr_button", value = 4 )
            showNotification("Please, frequency must be higher than 0", duration = 3, closeButton = FALSE, type = "warning")
          } else{
            updateNumericInput(session, inputId = "freqhr_button", value = value_of_freq)
          }
        }, error = function(e) {
        })
      }
      )
      
      
      observeEvent(input$num_samples, {
        num_samples <- input$num_samples
        tryCatch({
          if(num_samples < 1){
            updateNumericInput(session, inputId = "num_samples", value = 2 )
            showNotification("Please, number of samples must be higher than 0", duration = 3, closeButton = FALSE, type = "warning")
          } else{
            updateNumericInput(session, inputId = "num_samples", value = num_samples)
          }
        }, error = function(e) {
        })
      }
      )
      
      
      observeEvent(input$significance_level, {
        sig_level <- input$significance_level
        tryCatch({
          if(sig_level <= 0 || sig_level >= 1){
            updateNumericInput(session, inputId = "significance_level", value = 0.05 )
            showNotification("Please, significance level must be between 0 and 1", duration = 3, closeButton = FALSE, type = "warning")
          } else{
            updateNumericInput(session, inputId = "significance_level", value = sig_level)
          }
        }, error = function(e) {
        })
      }
      )
  
}




 
