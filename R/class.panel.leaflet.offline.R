TabPanelLeafletOffline <- R6::R6Class(
  'TabPanelLeafletOffline',
  inherit = TabPanelLeaflet,
  portable = FALSE,
  public = list(
    getUIPanel = function() {
      shiny::fillCol(
        flex = c(40L, 25L, 280L, NA), height = 350L, width = 600L,
        shinyFeedback::loadingButton(inputId = self$ns('act_scan'), label = ..('Scan'), loadingLabel = ..('Scanning (please wait)...'), style = 'width: 100%', class = 'btn btn-default'),
        shinyjs::hidden(shinyWidgets::progressBar(id = self$ns('pb_scanning'), value = 0L, display_pct = TRUE)),
        super$getUIPanel(),
        shiny::absolutePanel(
          bottom = 8L, right = 162L, fixed = TRUE,
          shinyjs::hidden(button(inputId = self$ns('act_cancel'), label = ..('Cancel'), class = 'btn'))
        )
      )
    }
  ),
  private = list(
    getServer = function(input, output, session) {
      super$getServer(input, output, session)

      lf <- leafletProxy(map_id = 'map')

      async <- reactiveValues()
      async$job <- NULL
      async$queue <- ipc::queue()

      startJob <- function() {

        dialog$out$sf <- NULL
        # shinyjs::hide(id = 'pb_scanning')
        # shinyjs::show(selector = 'div.progress-group')

        shinyWidgets::updateProgressBar(session = application$session, id = self$ns('pb_scanning'), value = 0L)

        disableTabPanel(value = self$ns_parent('tab_google'))

        shinyjs::disable(id = self$ns_parent('act_ok'), asis = TRUE)
        shinyjs::disable(id = self$ns_parent('act_cancel'), asis = TRUE)
        shinyjs::hide(id = self$ns_parent('act_cancel'), asis = TRUE)
        shinyjs::show(id = 'act_cancel')
        shinyjs::show(selector = 'div.progress-group')

        lf <- leaflet::clearGroup(lf, group = 'tiles')

        async_source <- fs::path(getDirAppTemp(), 'job.R')

        dump(
          c(
            'createDir',
            'degrees',
            'getDirApp',
            'getDirApps',
            'getDirOrganization',
            'is_tile_grid',
            'isShinyApp',
            'isHostedApp',
            'add_tilenum',
            'readBasemapGoogle',
            'st_add_coordinates',
            'st_bbox_polygon',
            'st_centroid_ll',
            'tile_bbox_ll',
            'tile_to_sf',
            'tile_grid_to_sf',
            'tilenum_to_lon',
            'tilenum_to_lat'
          ),
          file = async_source
        )

        async$job <- callr::r_bg(
          args = list(
            polygons = sf::st_geometry(data$polygons_selected),
            async_source = async_source,
            async_queue = async$queue
          ),
          func = function(polygons, async_source, async_queue) {
            source(async_source)
            # out <- try(
            out <- readBasemapGoogle(polygons = polygons, async_queue = async_queue)
            # , silent = TRUE)
            out
          },
          stdout = '',
          stderr = '',
          supervise = TRUE
        )

        check$resume()
      }

      stopJob <- function() {

        shinyWidgets::updateProgressBar(session = application$session, id = self$ns('pb_scanning'), value = 100L)

        check$suspend()

        if (!is.null(async$job)) {
          async$job$kill()
        }

        enableTabPanel(value = self$ns_parent('tab_google'))

        if (is.null(dialog$out$tiles)) {
          shinyjs::disable(id = self$ns_parent('act_ok'), asis = TRUE)
        } else {
          shinyjs::enable(id = self$ns_parent('act_ok'), asis = TRUE)
        }

        shinyjs::enable(id = self$ns_parent('act_cancel'), asis = TRUE)
        shinyjs::show(id = self$ns_parent('act_cancel'), asis = TRUE)
        shinyjs::hide(id = 'act_cancel')
        shinyjs::hide(selector = 'div.progress-group')

        shinyFeedback::resetLoadingButton(inputId = 'act_scan')
      }

      shiny::observeEvent(input$act_scan, {
        startJob()
      })

      progress_polygon <- function(polygon) {
        fitToSpatialFeatureBounds(lf, data$polygons_selected[polygon, 1L])
      }

      progress_zoom <- function(zoom) {
        leaflet::clearGroup(lf, group = 'tiles')
      }

      progress_tile <- function(progress, tile) {
        shinyWidgets::updateProgressBar(session = application$session, id = self$ns('pb_scanning'), value = progress, title = 'rttt')

        lf |>
          leaflet::addPolygons(
            data = st_bbox_polygon(tile_bbox_ll(tile$x, tile$y, tile$zoom)),
            color = 'yellow',
            fill = TRUE,
            group = 'tiles',
            weight = 0.5
          )
      }

      check <- shiny::observe(suspended = TRUE, {
        shiny::invalidateLater(millis = 50L, session = application$session)

        if (!is.null(async$job)) {
          if (async$job$is_alive()) {
            async$queue$consumer$consume()
          } else {
            dialog$out$tiles <- async$job$get_result()
            stopJob()
          }
        }

      })

      shiny::observeEvent(input$act_cancel, {
        check$suspend()
        stopJob()
      })

    },
    renderMap = function() {
      application$data$displayPolygons(private$.map, only_selected = TRUE)
    }
  )
)
