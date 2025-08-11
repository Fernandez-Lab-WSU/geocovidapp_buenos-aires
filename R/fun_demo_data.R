#' Genera un raster falso para pruebas en modo demo
#'
#' Esta función genera un objeto `SpatRaster` con valores aleatorios entre -50 y 50, 
#' útil para pruebas en modo demo sin conexión a bases de datos reales. 
#' La extensión y resolución del raster varían según el área especificada.
#'
#' @param area Área geográfica para la que se quiere simular el raster. 
#' Puede ser `"baires"` (provincia de Buenos Aires), `"amba"` (área metropolitana de Buenos Aires), 
#' o cualquier otro valor para una región genérica por defecto.
#'
#' @return Un objeto `terra::SpatRaster` con valores numéricos aleatorios entre -50 y 50,
#' sistema de referencia geográfico WGS84 (EPSG:4326), y extensión definida según el área.
#'
#' @import terra
#' @export
load_fake_raster <- function(area) {
  # Defino extensión y resolución según el área
  if (area == "baires") {
    # Provincia de Buenos Aires (aprox)
    ext <- terra::ext(-62, -56, -41, -33)  # lon_min, lon_max, lat_min, lat_max
    ncol <- 200
    nrow <- 150
  } else if (area == "amba") {
    # Área metropolitana de Buenos Aires (más chica)
    ext <- terra::ext(-58.7, -58.2, -35.0, -34.4)
    ncol <- 150
    nrow <- 100
  } else {
    # Default pequeño
    ext <- terra::ext(-60, -59, -35, -34)
    ncol <- 50
    nrow <- 50
  }
  
  # Creo el raster vacío con la extensión y resolución
  r <- terra::rast(ncol = ncol, nrow = nrow, ext = ext)
  
  # Genero valores aleatorios entre -50 y 50
  terra::values(r) <- runif(ncol * nrow, min = -50, max = 50)
  
  # Asigno CRS WGS84
  terra::crs(r) <- "EPSG:4326"
  
  return(r)
}

#' Genera un conjunto de datos espaciales falsos tipo `px_promedio`
#'
#' Esta función genera un objeto `sf` que simula la estructura de datos 
#' provenientes de una base de datos `px_promedio`, incluyendo variables horarias 
#' y criterios calculados a partir de los valores simulados. Útil para pruebas 
#' en modo demo.
#'
#' @param partido Nombre del partido (municipio) a simular. Se utiliza como valor 
#' de la columna `partido`.
#' @param tipo_tab Tipo de raster o categoría de análisis, que se guarda en la 
#' columna `tipo_de_raster`.
#'
#' @return Un objeto `sf::sf` con 4 polígonos simples y las siguientes columnas:
#' \itemize{
#'   \item `fecha`: Fechas consecutivas simuladas.
#'   \item `locacion`: Siempre `"baires"` en esta simulación.
#'   \item `tipo_de_raster`: Tipo de raster indicado como argumento.
#'   \item `partido`: Nombre del partido pasado como argumento.
#'   \item `manana_8`, `tarde_16`, `noche_0`: Valores aleatorios entre 0 y 7.
#'   \item `px_mean_dianoche`: Promedio entre `manana_8` y `tarde_16`.
#'   \item `criterio`: Clasificación basada en `px_mean_dianoche`.
#'   \item `criterio_noche`: Clasificación basada en `noche_0`.
#' }
#'
#' @details
#' Los criterios (`criterio`, `criterio_noche`) se asignan según los siguientes rangos:
#' \itemize{
#'   \item Menor a 2.5: `"5 - 1"`
#'   \item Entre 2.5 y 5: `"10 - 1"`
#'   \item Mayor o igual a 5: `"20 - 1"`
#' }
#'
#' @import sf
#' @export
load_fake_px_data <- function(partido, tipo_tab) {
  # Simulamos fechas
  fechas <- base::as.Date("2020-05-12") + 0:3
  
  # Polígonos simulados
  poligonos <- sf::st_sfc(
    sf::st_polygon(list(base::rbind(
      c(-58.28, -34.68), c(-58.27, -34.68), c(-58.27, -34.67), c(-58.28, -34.67), c(-58.28, -34.68)
    ))),
    sf::st_polygon(list(base::rbind(
      c(-58.27, -34.69), c(-58.26, -34.69), c(-58.26, -34.68), c(-58.27, -34.68), c(-58.27, -34.69)
    ))),
    sf::st_polygon(list(base::rbind(
      c(-58.29, -34.67), c(-58.28, -34.67), c(-58.28, -34.66), c(-58.29, -34.66), c(-58.29, -34.67)
    ))),
    sf::st_polygon(list(base::rbind(
      c(-58.30, -34.69), c(-58.29, -34.69), c(-58.29, -34.68), c(-58.30, -34.68), c(-58.30, -34.69)
    )))
  )
  
  # Generamos datos aleatorios
  manana_8 <- stats::runif(4, 0, 7)
  tarde_16 <- stats::runif(4, 0, 7)
  noche_0 <- stats::runif(4, 0, 7)
  
  # Cálculo del promedio
  px_mean_dianoche <- (manana_8 + tarde_16) / 2
  
  # Función para criterio
  get_criterio <- function(x) {
    if (x < 2.5) return("5 - 1")
    else if (x < 5) return("10 - 1")
    else return("20 - 1")
  }
  
  # Data frame
  df <- base::data.frame(
    fecha = fechas,
    locacion = "baires",
    tipo_de_raster = tipo_tab,
    partido = partido,
    manana_8 = manana_8,
    tarde_16 = tarde_16,
    noche_0 = noche_0,
    px_mean_dianoche = px_mean_dianoche,
    criterio = base::sapply(px_mean_dianoche, get_criterio),
    criterio_noche = base::sapply(noche_0, get_criterio)
  )
  
  # Combino con geometría
  sf_df <- sf::st_sf(df, geom = poligonos, crs = 4326)
  
  return(sf_df)
}
