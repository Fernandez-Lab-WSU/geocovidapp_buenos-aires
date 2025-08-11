#' Inicializador de la aplicación GeoCovidApp
#'
#' Esta función se utiliza para inicializar la aplicación GeoCovidApp.
#' Es recomendable llamarla al comienzo de la función `run_app()` para realizar tareas 
#' de configuración necesarias antes de que se ejecute la interfaz de usuario y el servidor.
#'
#' Por ejemplo, esta función puede:
#' - Leer archivos de configuración con el paquete `config`.
#' - Establecer conexiones a bases de datos usando `pool`.
#' - Definir opciones globales o variables necesarias durante la ejecución de la app.
#'
#' @return Un objeto de tipo `list` con objetos inicializados, como por ejemplo `pool` o configuración.
#'
#' @export
geocovidapp_init <- function() {
  
  db <- config::get("database")
  
  pool <- pool::dbPool(
    drv = RPostgres::Postgres(),
    dbname = db$dbname,
    user = db$user,
    password = db$password,
    host = db$host,
    port = db$port
  )
  
  onStop(function() {
    pool::poolClose(pool)
  })
  
  # Carga datos desde data/ (si estan como datos internos)
  # Si los .rda estan en inst/data/, mejor moverlos a data/ y usar data()
  data(bsas, package = "geocovidapp")
  data(bsas_comunas, package = "geocovidapp")
  data(centroides_mapa, package = "geocovidapp")
  data(base_raster, package = "geocovidapp")
  data(data_sisa, package = "geocovidapp")
  
  # agrega ruta de archivo
  shiny::addResourcePath("www", system.file("www", package = "geocovidapp"))
}



