#Single file analysis

#hrv.data preparatiom
prep_analysis <- function(fileType, Recordname, RecordPath){
  tryCatch({
    hrv.data = CreateHRVData()
    hrv.data = LoadBeat(fileType = fileType, HRVData = hrv.data, Recordname = Recordname, RecordPath = RecordPath)
    hrv.data = BuildNIHR(hrv.data)
    hrv.data = FilterNIHR(hrv.data)
    return(hrv.data)
  }, error = function(e) {
    showNotification("Error: Please choose another valid file", type = "error")
  })
  
}

#TIME
time_single_analysis<-function(hrv.data, size, interval, freqhr, method){
  tryCatch({
    hrv.data = InterpolateNIHR(HRVData = hrv.data, freqhr = freqhr, method = method)
    hrv.data = CreateTimeAnalysis(HRVData = hrv.data, size = size, interval = interval)
    
    return(hrv.data)
  }, error = function(e) {
    showNotification("Error: Please choose another valid file", type = "error")
  })
  
}





#Fourier
fourier_single_analysis <- function(hrv.data, freqhr, method, size,
                                    shift, ULFmin, ULFmax, VLFmin, VLFmax, LFmin, LFmax, HFmin, HFmax, type){
  tryCatch({
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
wavelet_single_analysis <- function(hrv.data, freqhr, method, size,
                                    shift, ULFmin, ULFmax, VLFmin, VLFmax, LFmin, LFmax, HFmin, HFmax, type, wavelet, bandtolerance) {
  tryCatch({
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



