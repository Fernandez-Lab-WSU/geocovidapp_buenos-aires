#' IU: Mapa prov. de Buenos Aires por departamento
#'
#' @description
#' Este mapa se encuentra en el tab 3 de GeoCovid app.
#'
#' @param id Module name
#'
#' @return Elementos de interfaz de usuario del tab 3
#' @export
MapaCovidDepartamentos_UI <- function(id) {
  bsas_lista <- c(
    "25 de Mayo", "9 de Julio", "Adolfo Alsina", "Adolfo Gonzales Chaves",
    "Alberti", "Almirante Brown", "Arrecifes", "Avellaneda", "Ayacucho",
    "Azul", "Bah\u00eda Blanca", "Balcarce", "Baradero", "Benito Ju\u00e1rez",
    "Berazategui", "Berisso", "Bol\u00edvar", "Bragado", "Brandsen",
    "Campana", "Ca\u00f1uelas", "Capital Federal", "Capit\u00e1n Sarmiento",
    "Carlos Casares", "Carlos Tejedor", "Carmen de Areco", "Castelli",
    "Chacabuco", "Chascom\u00fas", "Chivilcoy", "Col\u00f3n", "Coronel de Marina Leonardo Rosales",
    "Coronel Dorrego", "Coronel Pringles", "Coronel Su\u00e1rez", "Daireaux",
    "Dolores", "Ensenada", "Escobar", "Esteban Echeverr\u00eda", "Exaltaci\u00f3n de la Cruz",
    "Ezeiza", "Florencio Varela", "Florentino Ameghino", "General Alvarado",
    "General Alvear", "General Arenales", "General Belgrano", "General Guido",
    "General Juan Madariaga", "General La Madrid", "General Las Heras",
    "General Lavalle", "General Paz", "General Pinto", "General Pueyrred\u00f3n",
    "General Rodr\u00edguez", "General San Mart\u00edn", "General Viamonte",
    "General Villegas", "Guamin\u00ed", "Hip\u00f3lito Yrigoyen", "Hurlingham",
    "Ituzaing\u00f3", "Jos\u00e9 C. Paz", "Jun\u00edn", "La Costa", "La Matanza",
    "La Plata", "Lan\u00fas", "Laprida", "Las Flores", "Leandro N. Alem",
    "Lezama", "Lincoln", "Lober\u00eda", "Lobos", "Lomas de Zamora",
    "Luj\u00e1n", "Magdalena", "Maip\u00fa", "Malvinas Argentinas", "Mar Chiquita",
    "Marcos Paz", "Mercedes", "Merlo", "Monte", "Monte Hermoso",
    "Moreno", "Mor\u00f3n", "Navarro", "Necochea", "Olavarr\u00eda", "Patagones",
    "Pehuaj\u00f3", "Pellegrini", "Pergamino", "Pila", "Pilar", "Pinamar",
    "Presidente Per\u00f3n", "Pu\u00e1n", "Punta Indio", "Quilmes", "Ramallo",
    "Rauch", "Rivadavia", "Rojas", "Roque P\u00e9rez", "Saavedra", "Saladillo",
    "Salliquel\u00f3", "Salto", "San Andr\u00e9s de Giles", "San Antonio de Areco",
    "San Cayetano", "San Fernando", "San Isidro", "San Miguel", "San Nicol\u00e1s",
    "San Pedro", "San Vicente", "Suipacha", "Tandil", "Tapalqu\u00e9",
    "Tigre", "Tordillo", "Tornquist", "Trenque Lauquen", "Tres Arroyos",
    "Tres de Febrero", "Tres Lomas", "Vicente L\u00f3pez", "Villa Gesell",
    "Villarino", "Z\u00e1rate"
  )

  ns <- NS(id)

  shiny::tagList(
    bslib::layout_sidebar(
      fluidRow(
        column(
          6,
          tags$h5(textOutput(ns("diaselec"))),
          tags$h6("provincia de Buenos Aires"),
          br(),
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("burbujas_map"),
              height = "600px"
            ),
            type = 2,
            color = "lightgrey",
            color.background = "white"
          )
        ),
        column(
          width = 6,
          tags$h5("Casos de COVID-19"),
          tags$h6("Click en el primer grafico para seleccionar la fecha del mapa"),
          Dygraph_UI(
            ns("casos_covid_interno")
          ),
          br(),
          tags$h5("Promedio de movilidad diurna y nocturna"),
          shinycssloaders::withSpinner(
            dygraphs::dygraphOutput(ns("raster_analisis"),
              height = 100
            ),
            type = 2,
            color = "lightgrey",
            color.background = "white"
          ),
          br(),
          tags$h5("Vision ampliada por departamento"),
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(
              ns("zoom_map"),
              height = 250
            ),
            type = 2,
            color = "lightgrey",
            color.background = "white"
          )
        )
      ),
      sidebar = bslib::sidebar(
        position = "right",
        open = "always",
        shiny::selectInput(ns("partidos"),
          label = "Selecciona el partido",
          choices = bsas_lista,
          selected = bsas_lista[1],
          width = "100%"
        ),
        shiny::radioButtons(ns("tipo_tab"),
          label = "Selecciona el raster",
          choices = c(
            "Prepandemia" = "pc",
            "Semanal" = "7dpc"
          ),
          selected = "pc"
        ),
        shiny::radioButtons(ns("momento"),
          label = "Selecciona el momento del dia",
          choices = c(
            "Ma\u00f1ana-Tarde" = "criterio",
            "Noche" = "criterio_noche"
          ),
          selected = "criterio"
        )
      )
    )
  )
}


#' Server: Modulo mapa casos COVID con promedio por departamento de Bs. As.
#'
#' @description
#' Logica del servidor para el modulo que muestra un mapa de Buenos Aires con un 
#' promedio de los valores de raster para ese dia y los casos de COVID-19 por
#' departamento como burbujas. 
#' Este modulo corresponde al tab 3, "Por partido", de la aplicacion GeoCovid.
#'
#' @param id `character`. Identificador del modulo Shiny.
#' @param pool Conexion a base de datos.
#'
#' @return Una lista de reactivos con el raster seleccionado (`mapa_partido`) y el nivel de zoom (`zoom_mapa_partido`).
#' @export
MapaCovidDepartamentos_Server <- function(id,
                                          pool) {
  moduleServer(
    id,
    function(input, output, session) {
      # Filtro los partidos de Buenos Aires
      fechas <- Dygraph_Server("casos_covid_interno",
        part = reactive({
          input$partidos
        }),
        area = reactiveVal({
          "baires"
        })
      )

      # Datos de movilidad por partido como media de los pixeles
      px_data <- shiny::reactive({
        partidos_input <- input$partidos
        tipo_tab_input <- input$tipo_tab
        
        geocovidapp::extrae_px_data(partido = partidos_input,
                    tipo_tab = tipo_tab_input, 
                    mapa = FALSE,
                    con = pool)
      })



      output$raster_analisis <- dygraphs::renderDygraph({
        pxdy <- px_data()

        a <- pxdy[, c("fecha", "px_mean_dianoche")]
        b <- pxdy[, c("fecha", "manana_8")]
        c <- pxdy[, c("fecha", "tarde_16")]
        d <- pxdy[, c("fecha", "noche_0")]

        px_baires_dianoche <- xts::xts(a$px_mean_dianoche,
          order.by = a$fecha
        )
        px_baires_8 <- xts::xts(b$manana_8,
          order.by = b$fecha
        )
        px_baires_16 <- xts::xts(c$tarde_16,
          order.by = c$fecha
        )
        px_baires_0 <- xts::xts(d$noche_0,
          order.by = d$fecha
        )

        px_baires <- cbind(
          px_baires_dianoche,
          px_baires_0,
          px_baires_8,
          px_baires_16
        )

        # quiero que empiece el mismo dia que el grafico de los casos de covid
        # por cuestiones visuales

        # Fecha que quieres que empiece el eje x
        start_date <- as.Date("2020-04-15")

        # Fecha del primer dato real en px_baires
        first_date <- zoo::index(px_baires)[1]

        # Si start_date es antes que first_date, agregamos filas con NA para esas fechas
        if (start_date < first_date) {
          missing_dates <- seq(start_date, first_date - 1, by = "days")

          # Crear xts con NA para esas fechas
          na_rows <- xts(matrix(NA, nrow = length(missing_dates), ncol = ncol(px_baires)), order.by = missing_dates)
          colnames(na_rows) <- colnames(px_baires)

          # Combinar datos NA con datos originales
          px_baires_extendido <- rbind(na_rows, px_baires)
        } else {
          px_baires_extendido <- px_baires
        }


        base::colnames(px_baires_extendido)[1] <- as.character("PromedioDiaYTarde")
        base::colnames(px_baires_extendido)[2] <- as.character("Noche")

        dygraphs::dygraph(data = px_baires_extendido) |>
          dygraphs::dySeries(c(
            "px_baires_8",
            "PromedioDiaYTarde",
            "px_baires_16"
          )) |>
          dygraphs::dySeries("Noche") |>
          dygraphs::dyOptions(
            labelsUTC = TRUE,
            includeZero = TRUE
          ) |>
          dygraphs::dyAxis("y",
            label = "% de casos por partido"
          ) |>
          dygraphs::dyHighlight(
            highlightCircleSize = 3,
            highlightSeriesBackgroundAlpha = 0.4,
            hideOnMouseOut = TRUE
          ) |>
          dygraphs::dyEvent("2020-05-10",
            "a",
            labelLoc = "bottom"
          ) |>
          dygraphs::dyEvent("2020-06-07",
            "b",
            labelLoc = "bottom"
          ) |>
          dygraphs::dyLegend(show = "follow", width = 400) |>
          dygraphs::dyCSS(system.file("geocovidapp/www/legend.css",
            package = "geocovidapp"
          ))
      })

      fecha_formato <- shiny::reactive({
        # Agrego un dia por default para que renderice si no hay otras fechas.
        if (is.null(fechas$casos_covid())) {
          format("2020-05-03", format = "%d-%m-%Y")
        } else {
          fecha <- fechas$casos_covid()
          formatted_date(fecha = fecha)
        }
      })

      # Texto que se ve en la panatalla por sobre el mapa
      output$diaselec <- renderText({
        paste(
          "Movilidad ciudadana",
          if (input$tipo_tab == "pc") {
            "prepandemia"
          } else {
            "semanal"
          },
          "para", if (input$momento == "criterio") {
            "el promedio ma\u00f1ana y tarde"
          } else {
            "la noche"
          },
          "de", format(as.Date(fecha_formato()), format = "%d-%m-%Y")
        )
      })

      # Datos de casos de COVID para el bubble map
      sisa <- reactive({
     
        comunas <- geocovidapp::data_sisa |>
          dplyr::filter(residencia_provincia_nombre == "CABA" &
            fecha_enfermo == fecha_formato()) |>
          dplyr::group_by(residencia_provincia_nombre) |>
          dplyr::summarize(n_casos = dplyr::n()) |>
          dplyr::rename("partido" = residencia_provincia_nombre)

        comunas[1, "partido"] <- "Capital Federal"

        casos_diarios <- geocovidapp::casos_partido_diarios(
          provincia = "Buenos Aires",
          fecha = fecha_formato()
        ) |>
          rbind(comunas)

        # 3. Grafico
        # uso un left_join porque ya casos_darios_partido no es un sf data.frame
        sisa <- casos_diarios |>
          dplyr::left_join(geocovidapp::centroides_mapa,
            by = c("partido")
          ) |>
          tidyr::drop_na(.data$n_casos) |>
          dplyr::arrange(dplyr::desc(.data$n_casos)) |>
          dplyr::mutate(crit_covid = dplyr::case_when(
            10 >= .data$n_casos &
              .data$n_casos >= 1 ~ "1 - 10",
            100 >= .data$n_casos &
              .data$n_casos > 10 ~ "10 - 100",
            .data$n_casos > 100 ~ "M\u00e1s de 100"
          )) |>
          dplyr::mutate(crit_covid = factor(
              crit_covid,
              levels = c("1 - 10", "10 - 100", "M\u00e1s de 100")
            )
          )
        sisa
      })

      # Extraigo la informacion de SQL
      px_data_mapa <- shiny::reactive({
        fecha_input <- fecha_formato() # Esto es output de dygraph
        tipo_tab_input <- input$tipo_tab

        # Bajar la base de datos completa tarda mucho tiempo.
   geocovidapp::extrae_px_data(tipo_tab = tipo_tab_input,
                            fecha = fecha_input, 
                            mapa = TRUE,
                            con = pool)
      })

      # Uso los datos de covid para estimar los valores
      burbujas_plot <- shiny::reactive({
        tam_burb <- sisa() |>
          dplyr::group_by(.data$crit_covid) |>
          dplyr::arrange(dplyr::desc(.data$n_casos))

        if (length(unique(tam_burb$n_casos)) == 1) {
          warning("Todos los valores de n_casos son iguales. No se pueden crear rangos \u00fatiles.")

          # Todo en uno
          tam_burb_small <- tam_burb
          tam_burb_medium <- tam_burb[0, ]
          tam_burb_large <- tam_burb[0, ]
        } else {
          q <- quantile(tam_burb$n_casos, probs = c(0.5, 0.9))

          size_small <- as.integer(q[[1]])
          size_medium <- as.integer(q[[2]])

          # Corrige si size_small es 1
          if (size_small == 1) {
            size_small <- 2
          }

          # Si size_small y size_medium terminan iguales, ajusta
          if (size_medium <= size_small) {
            warning("Los cortes small y medium se solapan. Se agrupara todo en dos rangos.")

            tam_burb_small <- subset(
              tam_burb,
              n_casos <= size_small
            )

            tam_burb_medium <- tam_burb[0, ]

            tam_burb_large <- subset(
              tam_burb,
              n_casos > size_small
            )
          } else {
            # Normal
            tam_burb_small <- subset(
              tam_burb,
              n_casos <= size_small
            )

            tam_burb_medium <- subset(
              tam_burb,
              n_casos > size_small & n_casos <= size_medium
            )

            tam_burb_large <- subset(
              tam_burb,
              n_casos > size_medium
            )
          }
        }
print(px_data_mapa())
        px_data_mapa <- px_data_mapa()
        
        # Me aseguro que los datos esten en el formato correcto
        px_data_mapa <- px_data_mapa |> 
          sf::st_as_sf(crs = 4326, sf_column_name = "geom")
        
        print(px_data_mapa)
        
        # fun_mapa_bsas_covid_burbujas.R
        geocovidapp::mapa_burbujas(
          data_sf = px_data_mapa,
          color_var = input$momento,
          data_burb_peq = tam_burb_small,
          data_burb_med = tam_burb_medium,
          data_burb_gra = tam_burb_large,
          cent_x = "X",
          cent_y = "Y",
          n_casos = "n_casos",
          partido = "partido",
          burb_peq = size_small,
          burb_med = size_medium
        )
      })

      # Mapa de burbujas.
      # Lo saque de esta seccion para poder reutilizarlo
      # En zoom_map
      output$burbujas_map <- plotly::renderPlotly({
        burbujas_plot()
      })

      # Detalle del mapa que se ve a un costado
      output$zoom_map <- plotly::renderPlotly({
        bsas_part <- bsas |>
          dplyr::filter(.data$partido == input$partidos)
        part_bbox <- sf::st_bbox(bsas_part)

        m <- list(
          l = 0,
          r = 0,
          b = 0,
          t = 0
        )

        # Reuso la base del mapa ya creado
        burbujas_plot() |>
          plotly::add_sf(
            stroke = I("#004643"),
            data = subset(
              px_data_mapa(),
              partido == input$partidos
            ),
            color = I("#ffffff00"),
            line = list(width = 4),
            hoverinfo = "skip"
          ) |>
          plotly::layout(
            showlegend = FALSE,
            margin = m,
            xaxis = list(range = c(
              part_bbox$xmin - 0.5,
              part_bbox$xmax + 0.5
            )),
            yaxis = list(range = c(
              part_bbox$ymin - 0.5,
              part_bbox$ymax + 0.5
            ))
          ) |>
          plotly::config(displayModeBar = FALSE)
      })
    }
  )
}
