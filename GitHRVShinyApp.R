library(shiny)

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
  
  # Crea una opción de navegación para la ventana 3
  tabPanel("Load Data", h1 = "Please load data",
           fileInput(inputId = "fileSelector",
                     label = "Load Data", 
                     multiple = FALSE,
                     placeholder = "No file selected",
                     accept = ".txt",
                     width = "100%")
  ),
  
  # Crea una opción de navegación para la ventana 2
  tabPanel("Time analysis", 
           h1("Do you want to perform a time analysis?"),
           numericInput(inputId = "primer_numero", "Write the first number"),
           numericInput(inputId = "segundo_numero", "Write the second number"),
           actionButton(inputId = "Sumar",  "Sumar"),
           textOutput("resultado")
  ),
  
  # Crea una opción de navegación para la ventana 3
  tabPanel("Frequency analysis", 
           h1("Do you want to perform a frequency analysis?")
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
}

shinyApp(ui = ui, server = server)


