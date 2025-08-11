#' Ejecuta la aplicación Shiny GeoCovidApp
#'
#' Esta función inicializa y ejecuta la aplicación principal de GeoCovidApp.
#' Se encarga de:
#' 
#' - Establecer la conexión a la base de datos mediante un pool.
#' - Cargar los datasets internos del paquete (e.g., `base_raster`, `bsas`, etc.).
#' - Llamar a `geocovidapp_init()` para configurar recursos adicionales.
#' - Ejecutar la aplicación Shiny con `ui()` y `server()`.
#'
#' @param demo Lógico. Si es `TRUE`, activa el modo demo (por defecto `FALSE`).
#'
#' @return Lanza una instancia de la app Shiny y no devuelve ningún valor.
#' @export
run_app <- function(demo = FALSE) {

  # Notar que la app NO esta leyendo el DESCRIPTION file del paquete
  # Durante el deployment con shinyapps.io solo se carga la carpeta inst/
  # Esto quiere decir que los paquetes que shinyapps.io no detecta de por si
  # mejor instalarlos explicitamente en el ambiente de deployment.
  # 
  geocovidapp::geocovidapp_init()
  
  # Run app
  shiny::shinyApp(ui = geocovidapp::ui(), server = function(input, output, session) {
    geocovidapp::server(input, output, session, demo = demo)
  })
}

