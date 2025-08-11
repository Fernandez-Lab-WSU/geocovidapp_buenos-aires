#' Carga de raster desde la base de datos o raster falso en modo demo
#'
#' Esta funcion carga un raster desde la base de datos segun el area especificada
#' ("amba" o "baires"). Utiliza una consulta SQL para recuperar el raster como un archivo binario
#' y lo guarda temporalmente como un archivo `.tif`, que luego es cargado como un objeto raster
#' utilizando el paquete `terra`.  
#' Si el argumento `demo` es `TRUE`, devuelve un raster simulado sin necesidad de conexion a la base.
#'
#' @param pool Conexion al pool de base de datos (puede ser `NULL` si `demo = TRUE`).
#' @param raster_data Un dataframe que contiene la informacion del raster, incluyendo el nombre del archivo (`filename`).
#' @param area Cadena de texto que indica el área de interés. Puede ser `"amba"` o `"baires"`.
#' @param demo Logico, si es `TRUE` carga un raster falso para modo demo. Por defecto `FALSE`.
#'
#' @return Un objeto `rast` del paquete `terra`, que representa el raster cargado (real o simulado).
#' @export
rasterLoader <- function(pool,
                         raster_data,
                         area,
                         demo = FALSE) {
  # con <- pool::poolCheckout(pool)  # Obtienes la conexion
  # on.exit(pool::poolReturn(con))   # Aseguras que se devuelva al pool cuando la funcion termine

  # 1. Pregunto si es modo demo
  if (demo == TRUE) {
    # 2. Si demo, devuelvo raster falso (función que podés definir aparte)
    return(geocovidapp::load_fake_raster(area))
  } else {
  
  # Los rasters de amba y baires estan en diferentes tablas dentro de la base
  # porque tienen distintos tamanos
  if (area == "amba") {
    query <- paste0(
      "SELECT ST_AsGDALRaster(rast, 'GTiff') AS rast FROM raster_schema.rasters_geo WHERE filename='",
      raster_data$filename, "';"
    )
  } else {
    query <- paste0(
      "SELECT ST_AsGDALRaster(rast, 'GTiff') AS rast FROM raster_schema.raster_geo_baires WHERE filename='",
      raster_data$filename, "';"
    )
  }

  result <- pool::dbGetQuery(pool, query)

  if (nrow(result) == 0) {
    warning("Error: No raster found for the specified filename.")
  }

  # Guarda el binario a un archivo temporario
  temp_file <- tempfile(fileext = ".tif")
  writeBin(result$rast[[1]], temp_file)

  # Carga el raster leyendolo desde el archivo temporario
  return(terra::rast(temp_file)) }
}
