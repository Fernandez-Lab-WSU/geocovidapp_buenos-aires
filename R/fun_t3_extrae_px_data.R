#' Extraer datos de px desde la base de datos
#' 
#' Esta función obtiene datos espaciales desde la base de datos según los parámetros 
#' especificados, con la opción de cargar datos de prueba en modo demo.
#' 
#' @param partido Character. Nombre del partido (necesario si `mapa = FALSE`).
#' @param tipo_tab Character. Tipo de raster a consultar.
#' @param fecha Character. Fecha en formato 'YYYY-MM-DD' (necesaria si `mapa = TRUE`).
#' @param mapa Logical. Indica si se consulta la versión para mapa (`TRUE`) o tabla (`FALSE`).
#' @param con Conexión a la base de datos (objeto DBI).
#' @param demo Logical. Si es `TRUE`, carga datos ficticios en lugar de consultar la base.
#' 
#' @return Un objeto espacial (sf) con los datos consultados.
#' 
#' @details
#' - Si `mapa = FALSE`, se requiere el parámetro `partido` y se consulta la tabla 'px_tab3_promedio'.
#' - Si `mapa = TRUE`, se requiere el parámetro `fecha` y se consulta la tabla con datos promedio por fecha.
#' - En modo `demo = TRUE` se retornan datos ficticios para pruebas.
#' 
#' @importFrom sf st_read
#' 
#' @export
extrae_px_data <- function(partido, tipo_tab, fecha, mapa, con = NULL, demo = FALSE) {
  if (demo == TRUE) {
    return(load_fake_px_data(partido, tipo_tab))
  }
  
  if (mapa == FALSE) {
    if (is.null(partido)) stop("Falta reportar `partido`")
    
    query <- paste0(
      "SELECT * FROM px_tab3_promedio.px_tab3_promedio WHERE ",
      "partido = '", partido, "' AND ",
      "tipo_de_raster = '", tipo_tab, "'"
    )
    
  } else if (mapa == TRUE) {
    if (is.null(fecha)) stop("Falta reportar `fecha`")
    
    query <- paste0(
      "SELECT * FROM px_tab3_promedio.px_tab3_promedio WHERE ",
      "fecha = '", fecha, "' AND ",
      "tipo_de_raster = '", tipo_tab, "'"
    )
  }
  
  
  sf::st_read(con, query = query)
}
