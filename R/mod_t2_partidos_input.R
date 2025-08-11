#' IU: Seleccion de departamentos de provincia de Buenos Aires con IU
#' dinámica.
#'
#' @param id Module name
#'
#' @return Elementos de la IU para que se seleccione partidos de AMBA o de
#' BsAs de forma condicional.
#' @export
Partidos_UI <- function(id) {
  ns <- NS(id)

  shiny::tagList(
    tags$div(
      fluidRow(
        shiny::column(
          6,
          shiny::radioButtons(ns("area"),
            label = "Selecciona el area",
            choices = c(
              "prov. de Buenos Aires" = "baires",
              "AMBA" = "amba"
            ),
            selected = "amba"
          )
        ),
        shiny::column(
          6,
          shiny::radioButtons(ns("porcentaje"),
            label = "Cambio porcentual",
            choices = c(
              "Prepandemia" = "pc",
              "Semanal" = "7dpc"
            ),
            selected = "7dpc"
          )
        ),
        shiny::selectInput(ns("partidos"),
          label = "Selecciona el partido",
          choices = geocovidapp::amba_reducido_names,
          selected = "Avellaneda",
          width = "100%"
        )
      )
    )
  )
}

#' Servidor: Seleccion de departamentos de provincia de Buenos Aires con IU
#' dinámica.
#'
#' @param id Module name
#'
#' @return El area y el partido seleccionado.
#' @export
Partidos_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      choices <- reactive({
        req(input$area)

        if (input$area == "amba") {
          sort(geocovidapp::amba_reducido_names) # tiene las comunas
        } else if (input$area == "baires") {
          sort(unique(geocovidapp::bsas$partido)) # sin las comunas
        }
      })


      # Actualiza opciones de partidos en base a la eleccion de area
      shiny::observe({
        shiny::updateSelectInput(session,
          inputId = "partidos",
          choices = choices(),
          selected = choices()[1]
        )
      })


      return(
        list(
          area = reactive({
            input$area
          }),
          partido = reactive({
            input$partidos
          }),
          porcentaje = reactive({
            input$porcentaje
          })
        )
      )
    }
  )
}
