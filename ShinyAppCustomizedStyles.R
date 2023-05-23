#Changes the button style. Will apply when the button has been selected it would change color
tags$head(
  tags$style(HTML("
      .boton-pulsado {
        background-color: green !important;
        color: white !important;
      }"
  )))

tags$head(
  tags$style(HTML("
      .blue-button {
        background-color: blue;
        color: white;
      }"
  )))

tags$head(
  tags$style(HTML("
      .left-btn {
        float: left;
      }
      
      .right-btn {
        float: right;
      }
    ")))

tags$head(
  tags$style(
    HTML("
      .table-style {
        /* Estilos para la tabla */
      }
      .mi-tabla th {
        /* Estilos para las celdas del encabezado */
      }
      .mi-tabla td {
        /* Estilos para las celdas de datos */
      }
    ")
  )
)

tags$head(
  tags$style(
    HTML("
      .mi-columna td:first-child {
        background-color: #cfe2f3; /* Fondo gris azulado */
        font-weight: bold; /* Letras en negrita */
      }
    ")
  )
)

tags$head(
  tags$script("
      $(document).on('change', '.decimal-input', function() {
        var value = $(this).val().replace('.', ',');
        $(this).val(value);
      });
    ")
)