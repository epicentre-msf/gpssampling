TabPanelLeafletFile <- R6::R6Class(
  'TabPanelLeafletFile',
  inherit = TabPanelLeaflet,
  portable = FALSE,
  public = list(
    getUIPanel = function() {
      shiny::fillCol(
        flex = c(65L, 0L, 280L), height = 350L, width = 600L,
        shiny::fileInput(
          inputId = ns('file'), label = NULL, buttonLabel = ..('Upload...'),
          placeholder = ..('Select geographic file...'), width = '100%',
          accept = c(
            '.gpx',
            '.json',
            '.kml',
            '.kmz',
            '.zip'
          )
        ),
        shinyjs::hidden(
          shiny::textInput(
            inputId = ns('file_path'), label = NULL
          )
          ),
        super$getUIPanel()
      )
    }
  ),
  private = list(
    getServer = function(input, output, session) {

      super$getServer(input, output, session)

      upload <- function(file) {

        if (!is.list(file)) {
          if (file == '') {
            return()
          } else {
            if (fs::file_exists(file)) {
              file <- list(
                datapath = file,
                type = ''
              )
            } else {
              return()
            }
          }
        }

        waiter <- waiter::Waiter$new(id = self$ns('map'), color = waiter::transparent(0.1), html = waiter::spin_1())
        waiter$show()
        on.exit({
          waiter$hide()
        })

        shinyFeedback::hideFeedback(inputId = 'file')

        shinyjs::disable(id = self$ns_parent('act_ok'), asis = TRUE)

        if (file$type == 'application/x-zip-compressed' || getFileExt(file$datapath) == 'zip') {

          withCallingHandlers(
            {
              dialog$out$sf <- readSpatialLayer(file = file$datapath)$layer
            },
            error = function(msg) {
              shinyFeedback::showFeedbackDanger(inputId = 'file', text = as.character(msg))
            },
            warning = function(msg) {
              shinyFeedback::showFeedbackWarning(inputId = 'file', text = as.character(msg))
            }
          )

        } else if (file$type == 'application/vnd.google-earth.kmz' | getFileExt(file$datapath) == 'kmz') {

          zipfiles <- unzip(zipfile = file$datapath, pattern = '.kml$')

          if (zipfiles$files_count == 0L) {

            shinyFeedback::showFeedbackWarning(inputId = 'file', text = ..("This file doesn't seem to contain a KML file."))

          } else if (zipfiles$files_count > 1L) {

            shinyFeedback::showFeedbackWarning(inputId = 'file', text = ..('This file contains several KML files. Please keep one KML by zip.'))

          } else {

            tryCatch(
              {
                kml_layers <- sf::st_layers(zipfiles$files[1L])
                sf_layers <- lapply(kml_layers$name, function(kml_layer) {
                  sf::read_sf(zipfiles$files[1L], layer = kml_layer)
                })
                dialog$out$sf <- Reduce('rbind', sf_layers)
              },
              error = function(msg) {
                shinyFeedback::showFeedbackDanger(inputId = 'file', text = as.character(msg))
              },
              warning = function(msg) {
                shinyFeedback::showFeedbackDanger(inputId = 'file', text = as.character(msg))
              }
            )

          }
        } else if (file$type == 'application/vnd.google-earth.kml+xml' | getFileExt(file$datapath) == 'kml') {

          kmlfile <- file$datapath

          tryCatch(
            {
              kml_layers <- sf::st_layers(kmlfile)
              sf_layers <- lapply(kml_layers$name, function(kml_layer) {
                sf::read_sf(kmlfile, layer = kml_layer)
              })
              dialog$out$sf <- Reduce('rbind', sf_layers)
            },
            error = function(msg) {
              shinyFeedback::showFeedbackDanger(inputId = 'file', text = as.character(msg))
            },
            warning = function(msg) {
              shinyFeedback::showFeedbackDanger(inputId = 'file', text = as.character(msg))
            }
          )

        } else if (file$type == 'application/gpx+xml' | getFileExt(file$datapath) == 'gpx') {

          gpxfile <- file$datapath

          tryCatch(
            {
              gpx_layers <- sf::st_layers(gpxfile)
              sf_layers <- lapply(gpx_layers$name, function(gpx_layer) {
                sf::read_sf(gpxfile, layer = gpx_layer)
              })
              sf_layers <- Reduce('rbind', sf_layers)
              names(sf_layers) <- gsub('^ogr_', '', names(sf_layers))
              dialog$out$sf <- sf_layers
            },
            error = function(msg) {
              shinyFeedback::showFeedbackDanger(inputId = 'file', text = as.character(msg))
            },
            warning = function(msg) {
              shinyFeedback::showFeedbackDanger(inputId = 'file', text = as.character(msg))
            }
          )

        }

        if (!is.null(dialog$out$sf)) {
          private$mutate()
        }

      }

      shiny::observeEvent(input$file_path, ignoreInit = TRUE, ignoreNULL = TRUE, {
        upload(input$file_path)
      })

      shiny::observeEvent(input$file, ignoreInit = TRUE, ignoreNULL = TRUE, {
        upload(input$file)
      })

    },
    mutate = function() {
    },
    renderMap = function() {
      data$displayPolygons(private$.map)
    }
  )
)

TabPanelLeafletFilePolygon <- R6::R6Class(
  'TabPanelLeafletFilePolygon',
  inherit = TabPanelLeafletFile,
  portable = FALSE,
  private = list(
    mutate = function() {

      if (is.na(sf::st_crs(dialog$out$sf))) {
        dialog$out$sf <- sf::st_set_crs(dialog$out$sf, 4326L)
      }

      dialog$out$sf <-
        dialog$out$sf[sf::st_geometry_type(dialog$out$sf) %in% c('POLYGON', 'MULTIPOLYGON'), ] |>
        sf::st_transform(4326L) |>
        sf::st_zm()

      if (nrow(dialog$out$sf) == 0L) {

        shinyFeedback::showFeedbackWarning(inputId = 'file', text = ..("This file doesn't seem to contain somes polygons."))

      } else if (nrow(dialog$out$sf) > 1000L) {

        shinyFeedback::showFeedbackWarning(inputId = 'file', text = ..('This file seem to contain lot of polygons. Try to reduce numbers of polygons'))

      } else {

        shinyFeedback::showFeedbackSuccess(inputId = 'file', text = NULL)

        dialog$out$sf <- dialog$out$sf |>
          dplyr::mutate(id = seq_len(dplyr::n()))

        private$.map |>
          leaflet::clearGroup('polygons') |>
          addPolygon(
            sf = dialog$out$sf,
            color = 'yellow',
            fill = TRUE,
            fill_opacity = 0.2,
            fit = FALSE
          ) |>
          fitToSpatialFeatureBounds(sf = dialog$out$sf)

        shinyjs::enable(id = self$ns_parent('act_ok'), asis = TRUE)

      }

    },
    renderMap = function() {
      data$displayPolygons(private$.map)
    }
  )
)

TabPanelLeafletFilePoint <- R6::R6Class(
  'TabPanelLeafletFilePoint',
  inherit = TabPanelLeafletFile,
  portable = FALSE,
  private = list(
    mutate = function() {

      dialog$out$sf$type <- sf::st_geometry_type(dialog$out$sf)

      pts_from_polygons <- dialog$out$sf |>
        dplyr::filter(type %in% c('POLYGON', 'MULTIPOLYGON')) |>
        sf::st_cast(to = 'POLYGON') |>
        sf::st_centroid() |>
        sf::st_transform(4326L) |>
        sf::st_zm()

      pts <- dialog$out$sf |>
        dplyr::filter(type == 'POINT') |>
        sf::st_cast(to = 'POINT') |>
        sf::st_transform(4326L) |>
        sf::st_zm()

      dialog$out$sf <- dplyr::bind_rows(
        pts_from_polygons,
        pts
      )

      private$.map <- leaflet::clearGroup(private$.map, 'roofs')

      if (nrow(dialog$out$sf) == 0L) {

        shinyFeedback::showFeedbackWarning(inputId = 'file', text = ..("This file doesn't seem to contain somes points."))

      } else if (nrow(dialog$out$sf) > 200000L) {

        shinyFeedback::showFeedbackWarning(inputId = 'file', text = ..('This file seem to contain lot of points (> 200000). Try to reduce numbers of points'))

      } else {

        shinyFeedback::showFeedbackSuccess(inputId = 'file', text = NULL)

        if (nrow(dialog$out$sf) <= 1000L) {

          private$.map <-
            leaflet::addCircles(
              private$.map,
              data = dialog$out$sf,
              group = 'roofs',
              fill = TRUE,
              fillColor = 'yellow',
              fillOpacity = 1L,
              radius = 2L,
              stroke = FALSE
            )

        } else {

          x <- terra::vect(dialog$out$sf)
          x_rst <- terra::rast(ext = terra::ext(x), ncols = 1000L, nrows = 1000L)
          x_rst <- terra::rasterize(x, x_rst)

          private$.map |>
            addStarsImage(
              stars::st_as_stars(x_rst),
              colors = 'yellow',
              group = 'roofs',
              project = FALSE,
              na.color = '#00000000'
            )
        }

        private$.map <- fitToSpatialFeatureBounds(private$.map, sf = dialog$out$sf)

        shinyjs::enable(id = self$ns_parent('act_ok'), asis = TRUE)

      }

    },
    renderMap = function() {
      data$displayPolygons(private$.map)
    }
  )
)

TabPanelLeafletOSMPolygon <- R6::R6Class(
  'TabPanelLeafletOSMPolygon',
  inherit = TabPanelLeaflet,
  portable = FALSE,
  public = list(
    getUIPanel = function() {
      shiny::fillCol(
        flex = c(35L, 25L, 280L), height = 350L, width = 600L,
        shiny::fillRow(
          flex = c(10L, 1L),
          shiny::textInput(
            inputId = ns('query'),
            label = NULL,
            placeholder = 'R3721234',
            width = '100%'
          ),
          shiny::actionButton(
            class = 'btn-xs',
            inputId = ns('act_search'),
            icon = icon('map-search-outline'),
            label = NULL,
            width = '100%'
          )
        ),
        tags$a(href = 'https://nominatim.openstreetmap.org', target = '_blank', ..('Search with OpenStreetMap and type the OSM ID...')),
        super$getUIPanel()
      )
    }
  ),
  private = list(
    getServer = function(input, output, session) {

      super$getServer(input, output, session)

      shiny::observeEvent(input$act_search, ignoreInit = TRUE, ignoreNULL = TRUE, {

        osm_id <- gsub('[A-Z]', '', input$query)
        osm_type <- gsub('[0-9]', '', input$query)

        out_sf <- nominatimlite::geo_address_lookup_sf(osm_ids = osm_id, type = osm_type, full_results = TRUE, points_only = FALSE)

        if (ncol(out_sf) > 1L) {

          out_sf <- sf::st_as_sf(out_sf$geometry[1L])

          if (sf::st_geometry_type(out_sf) %in% c('POLYGON', 'MULTIPOLYGON')) {

            private$.map |>
              leaflet::clearGroup('polygons') |>
              addPolygon(
                sf = out_sf,
                color = 'yellow',
                fill = TRUE,
                fill_opacity = 0.2,
                fit = FALSE
              ) |>
              fitToSpatialFeatureBounds(sf = out_sf)

            dialog$out$sf <- out_sf

            shinyjs::enable(id = self$ns_parent('act_ok'), asis = TRUE)
          }


        }
      })
    }
  )
)
