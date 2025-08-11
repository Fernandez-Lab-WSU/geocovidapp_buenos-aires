#' Servidor de la aplicacion GeoCovid
#' 
#' Funcion servidor para la aplicacion GeoCovid, maneja la logica reactiva,
#' procesamiento de datos, y control de los modulos de UI.
#' Incluye manejo de mapas, seleccion de fechas, filtros por partidos,
#' actualizacion de graficos y reportes.
#' 
#' @param input Objeto shiny input.
#' @param output Objeto shiny output.
#' @param session Objeto shiny session.
#' @param r Lista reactiva para compartir valores (opcional).
#' @param demo Logico que indica si la aplicacion se ejecuta en modo demo (por defecto FALSE).
#' 
#' @importFrom rlang .data
#' @importFrom shiny renderText renderUI reactive eventReactive observe reactiveValues
#' @importFrom bslib value_box
#' @export
server <- function(input, output, session, r, demo = FALSE) {
  
  # Cargo mapa panel "Movilidad Buenos Aires"
  elecciones_mapa <- geocovidapp::MapaBaires_Server("tab1_mapa",
                                       fecha = imagen$fecha, # para la informacion de la franja de abajo 
                                       porcentaje = imagen$porcentaje, # para la informacion de la franja de abajo 
                                       momento = imagen$momento, # para la informacion de la franja de abajo 
                                       imagen = imagen$imagen,
                                       boton = imagen$boton,
                                       basemap = imagen$basemap,
                                       opacidad = imagen$opacity,
                                       area = imagen$area)
  
  # Tab movilidad Buenos Aires -----
  # Cargo imagenes raster para cada dia
  imagen <- geocovidapp::FechaMomento_Server("tab1_barraflotante",
                                pool = pool,
                                mapa_zoom = elecciones_mapa$mapa_zoom,
                                demo = demo
                            )
  
  

  # Tab2: Por partido -----
  
  # Simula clic inicial para que act_mapas se dispare al cargar
  observe({
    shinyjs::click("act_mapas")  # simula un clic real
  })

    # # el ususario selecciona opciones del mapa
  # elecciones_usuario_partidos <- geocovidapp::Partidos_Server('seleccion_partido',
  #                                                bsas = bsas,
  #                                                amba_reducido_names = amba_reducido_names)

  # Carga rasters tanto para los histogramas como los mapas
  # dentro de este modulo esta anidado partidos_input.R
  # la eleccion de la fecha depende de dygraph, por ende defini
  # valores
  

  elecciones_usuario <- geocovidapp::Partidos_Server(  # mod_t2_partidos_input.R
    "selector_dinamico"
  )
  
  # el usuario seleciona del grafico una fecha
  eleccion_fecha <- geocovidapp::Dygraph_Server('casos_covid',
                                                area = elecciones_usuario$area,
                                                partido = elecciones_usuario$partido)
  
  fecha_final <- reactive({  if (is.null(eleccion_fecha$casos_covid())) {
    as.Date("2020-05-03")
  } else {
    fecha_final <-as.Date(geocovidapp::formatted_date(eleccion_fecha$casos_covid()))
  }
  })
  
  datos_raster <- reactive({
 
    d <- geocovidapp::base_raster |>
      dplyr::filter(
        fecha == fecha_final(),
        tipo_de_raster == elecciones_usuario$porcentaje(),
        locacion == elecciones_usuario$area()
      )
    
    d
  })
  
  imagen_partido <- eventReactive(input$act_mapas, ignoreInit = FALSE, ignoreNULL = FALSE, {
    raster_data <- datos_raster()
    area_valor <- elecciones_usuario$area()

    # Chequear
    raster_outputs <- lapply(
      setNames(split(raster_data, raster_data$momento), 
               raster_data$momento),
      function(r) {
        geocovidapp::rasterLoader(
          pool = pool,
          raster_data = r,
          area = area_valor,
          demo = demo
        )
      }
    )
    raster_outputs
  })
  
  fecha_titulo <- eventReactive(input$act_mapas,{
    
  # si el valor de fecha es NULL, como al inicio  
  if(is.null(eleccion_fecha$casos_covid())){
    
    fecha <- '2020-05-03 00:00:00' # lubradate::ymd_hms() espera una string

  }else{
    
    fecha <- eleccion_fecha$casos_covid() }
    paste('Movilidad humana el', 
          format(lubridate::ymd_hms(fecha), format = "%d-%m-%Y"), 
          "para",
          elecciones_usuario$partido(),
          "- cambio porcentual ",
          ifelse(elecciones_usuario$porcentaje() == "pc", "prepandemia", "semanal"),
          "resoluci\u00F3n ",
          ifelse(elecciones_usuario$area() == "amba", "10 m/pixel", "40 m/pixel")) # CHEQUEAR
  })
  
  output$titulo <- renderText({
   fecha_titulo()
   })
  
  output$vb_fecha <- renderUI({ 
    bslib::value_box( # estilo en custom.css
      id = "vb_1",
      title = "Fecha seleccionada",
      value = format(as.Date(fecha_final()), "%d-%m-%Y") # notar que esto no depende del boton actualizar
      )
    })
  
  
  casos_dia_seleccionado <-reactive({
  if(stringr::str_starts(elecciones_usuario$partido(), "Comuna")){
    casos_comunas <-  geocovidapp::casos_partido_diarios(provincia = "CABA", 
                                      fecha = fecha_final()) 
    
    sum(casos_comunas$n_casos) # total de casos
    
   
  } else {  
    
    geocovidapp::casos_partido_diarios(provincia = "Buenos Aires", 
                                               fecha = fecha_final()) |> 
      dplyr::filter(
        partido == elecciones_usuario$partido()
      ) |> dplyr::pull(n_casos)  } })
  
  output$vb_casos <- renderUI({ 
    bslib::value_box(
    id = "vb_2",
    title = paste("Casos de COVID-19", elecciones_usuario$partido()),
    value = casos_dia_seleccionado()
  )
  })

  # Histogramas
  geocovidapp::HistogramaRaster_Server('hist',
                          pool = pool,
                          act_mapas = reactive({input$act_mapas}),
                          imagen = imagen_partido,
                          bsas_comunas = bsas_comunas, # Incluye comunas de CABA
                          tipo_de_raster = elecciones_usuario$porcentaje,
                          fecha = eleccion_fecha$casos_covid,
                          momento_dia = 'ma\u00F1ana',
                          partido = elecciones_usuario$partido)

  geocovidapp::HistogramaRaster_Server('hist2',
                          pool = pool,
                          act_mapas = reactive({input$act_mapas}),
                          imagen = imagen_partido,
                          bsas_comunas = bsas_comunas,
                          tipo_de_raster = elecciones_usuario$porcentaje,
                          fecha = eleccion_fecha$casos_covid,
                          momento_dia = 'tarde',
                          partido = elecciones_usuario$partido)

  geocovidapp::HistogramaRaster_Server('hist3',
                          pool = pool,
                          act_mapas = reactive({input$act_mapas}),
                          imagen = imagen_partido,
                          bsas_comunas = bsas_comunas,
                          tipo_de_raster = elecciones_usuario$porcentaje,
                          fecha = eleccion_fecha$casos_covid,
                          momento_dia = 'noche',
                          partido = elecciones_usuario$partido)

  # Mapas con detalle del partido
  mapa_manana <- geocovidapp::MapaPartido_Server("baires_partidos",
                     act_mapas = reactive({input$act_mapas}),
                     imagen = imagen_partido,
                     area = elecciones_usuario$area, # tengo que aclarar esto porque corresponde al subset de rasters
                     tipo_de_raster = elecciones_usuario$porcentaje,
                     fecha = eleccion_fecha$casos_covid,
                     momento_dia = 'ma\u00F1ana',
                     par_d_bsas = elecciones_usuario$partido, # Partidos_Input.R
                     opacidad = reactive({ input$opacity }))

  mapa_tarde <- geocovidapp::MapaPartido_Server("baires_partidos2",
                     act_mapas = reactive({input$act_mapas}),
                     imagen = imagen_partido,
                     area = elecciones_usuario$area,
                     tipo_de_raster = elecciones_usuario$porcentaje,
                     fecha = eleccion_fecha$casos_covid,
                     momento_dia = 'tarde',
                     par_d_bsas = elecciones_usuario$partido, # Partidos_Input.R #n era partido inicialmente
                     opacidad = reactive({ input$opacity }))

  mapa_noche <-geocovidapp::MapaPartido_Server("baires_partidos3",
                     act_mapas = reactive({input$act_mapas}),
                     imagen = imagen_partido,
                     area = elecciones_usuario$area,
                     tipo_de_raster = elecciones_usuario$porcentaje,
                     fecha = eleccion_fecha$casos_covid,
                     momento_dia = 'noche',
                     par_d_bsas = elecciones_usuario$partido, # Partidos_Input.R
                     opacidad = reactive({ input$opacity }))

  # Reporte
  
   geocovidapp::ReporteServer("desc_reporte",
                             area = elecciones_usuario$area,
                             act_mapas = reactive({input$act_mapas}),
                             partido = elecciones_usuario$partido, # Partidos_Input.R
                             fecha = eleccion_fecha$casos_covid,
                             mapa_partido_manana = mapa_manana$mapa_partido,
                             mapa_partido_tarde = mapa_tarde$mapa_partido,
                             mapa_partido_noche = mapa_noche$mapa_partido,
                             tipo_de_raster = elecciones_usuario$porcentaje,
                             opacidad = reactive({ input$opacity })
  )


  # Tab 3: casos covid -----

  elecciones_mapa_covid <- geocovidapp::MapaCovidElecciones_Server('mapa_covid')

  geocovidapp::MapaCovidDepartamentos_Server('casos_covid',
                                             pool = pool)

}