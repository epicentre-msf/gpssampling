TabPanelLeaflet <- R6::R6Class(
  'TabPanelLeaflet',
  inherit = TabPanel,
  portable = FALSE,
  public = list(
    getUIPanel = function() {
      leaflet::leafletOutput(outputId = ns('map'), width = '100%', height = '100%')
    }
  ),
  private = list(
    .map = NULL,
    .map_proxy = NULL,
    getServer = function(input, output, session) {

      super$getServer(input, output, session)

      output$map <- leaflet::renderLeaflet({

        input$map_bound

        lf <- leaflet(session,
          inputId = ns('map'),
          attribution = FALSE,
          fullscreen = FALSE,
          prefer_canvas = FALSE,
          searchbar = FALSE,
          scalebar = FALSE,
          zoom = FALSE
        )

        lf <- setBasemap(lf, basemap = 'sat.google')

        lf <- lf |>
          leaflet::addMapPane('polygon_1', zIndex = 310L) |>
          leaflet::addMapPane('polygon_2', zIndex = 320L) |>
          leaflet::addMapPane('marker', zIndex = 510L)

        private$renderMap()

        return(lf)
      })

      private$.map_proxy <- leafletProxy(deferUntilFlush = FALSE)
      private$.map <- leafletProxy(deferUntilFlush = TRUE)
    },
    renderMap = function() {
    }
  )
)
