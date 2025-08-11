#' IU: Selector de fecha
#'
#' @param id Module name
#' @param base_raster Dataframe que lista todos los rasters y desagrega en 
#' sus columnas características de interes, como si son rasters de 
#' AMBA o Buenos Aires, si el cambio porcentual es semanal o prepandemia 
#' o el momento del día que representan. 
#' 
#'
#' @return Selector de fecha
#' @export
MapaCovidElecciones_UI <- function(id, base_raster) {
  
  ns <- NS(id)
  shiny::tagList(
    
    shiny::dateInput(ns("fecha"),
                     label = "Fecha",
                     min = min(base::unique(base_raster$fecha)),
                     max = max(base::unique(base_raster$fecha)),
                     value = base::unique(base_raster$fecha)[1],
                     language = "es",
                     format = "yyyy-mm-dd")
    
  )
  
}


#' Servidor: Selector de fecha
#'
#' @param id Module name
#'
#' @return Fecha seleccionada por el usuario
#' @export
MapaCovidElecciones_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      return(
        list(
          fecha = reactive({ input$fecha })
        )
      )
      
    })}