#' Componente de UI para Barra Informativa Inferior
#'
#' Crea un elemento de interfaz de usuario que muestra texto informativo dinamico
#' en la parte inferior de la aplicacion Shiny.
#'
#' @param id Cadena de caracteres usada para el espacio de nombres de los elementos UI.
#'
#' @return Un elemento UI (tagList) que contiene un div con salida de texto estilizada.
#' @export
BarraInferior_UI <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::div(
      id = ns("info_text"),
      class = "info-text",
      style = "position: absolute; bottom: 10px; width: 100%; text-align: center; padding: 5px; background-color: rgba(0,0,0,0.6); color: white; font-size: 16px;",
      shiny::textOutput(ns("info_text_output"))
    )
  )
}

#' Logica del Servidor para la Barra Informativa Inferior
#'
#' Logica del lado servidor para manejar el texto informativo mostrado en la
#' barra inferior. Reacciona a la interaccion del usuario y cambios en la entrada
#' para actualizar el mensaje.
#'
#' @param id Cadena de caracteres usada para identificar el espacio de nombres del modulo.
#' @param boton Expresion reactiva para el boton de actualizacion (por ejemplo, `input$update_map`).
#' @param fecha Expresion reactiva que retorna la fecha seleccionada como cadena.
#' @param area Expresion reactiva que indica el area seleccionada (por ejemplo, "amba").
#' @param momento Expresion reactiva que indica el momento del dia (por ejemplo, "manana", "tarde").
#' @param porcentaje Expresion reactiva que indica el tipo de dato (por ejemplo, "pc" para prepandemia).
#'
#' @return No retorna valor. Se llama por efectos secundarios para actualizar dinamicamente el texto en la UI.
#' @export
BarraInferior_Server <- function(id,
                                 boton,
                                 fecha,
                                 area,
                                 momento,
                                 porcentaje) {
  moduleServer(
    id,
    function(input, output, session) {
      # Inicializa la variable reactive para el texto
      info_text <- reactiveVal("Imagen inicial para Buenos Aires, 14/04/2020 a la tarde, Prepandemia. Cliquea en un partido o haz zoom para visualizar. Cambia elecciones y actualiza el mapa para mostrar otra imagen.")

      # Observa el evento de clic en el boton de actualizar
      observeEvent(boton(), {
        # Si hay un raster seleccionado
        info_text(paste(
          "Ubicaci\u00f3n:", ifelse(area() == "amba", "\u00c1rea Metropolitana de Buenos Aires", "Buenos Aires"),
          "| Fecha:", fecha(),
          "| Momento del d\u00eda:", momento(),
          "| Tipo de raster: ", ifelse(porcentaje() == "pc", "Prepandemia", "Semanal")
        ))
      })

      # Actualiza el texto cuando cambia el area
      observeEvent(area(), ignoreInit = TRUE, {
        info_text("No hay imagen seleccionada. Elige fecha, cambio porcentual y momento del d\u00eda y cliquea actualizar el mapa.")
      })


      # Actualiza el texto en la franja de abajo usando renderText
      output$info_text_output <- renderText({
        info_text()
      })
    }
  )
}
