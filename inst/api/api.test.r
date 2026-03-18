library(shiny)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255L))
names(r_colors) <- colors()

ui <- fluidPage(
  leafletOutput('mymap', height = 800L),
  p(),
  actionButton('recalc', 'New points')
)

server <- function(input, output, session) {

  points <- eventReactive(input$recalc,
    {
      cbind(rnorm(40L) * 2L + 13L, rnorm(40L) + 48L)
    },
    ignoreNULL = FALSE
  )

  output$mymap <- renderLeaflet({
    lf <- leaflet()

    lf <- lf %>%
      addProviderTiles(providers$Stamen.TonerLite,
        options = providerTileOptions(noWrap = TRUE)
      )

    lf <- lf %>%
      leaflet::addTiles(
        urlTemplate = 'http://127.0.0.1:8000/tile?user=default@epicentre.msf.org&x={x}&y={y}&z={z}&rnd={rnd}',
        options = leaflet::tileOptions(
          minZoom = 0L,
          maxZoom = 21L,
          maxNativeZoom = 20L,
          zIndex = 201L,
          rnd = JS('function(foo) {return Math.random();}'),
          useCache = FALSE,
          crossOrigin = FALSE
        ),
        group = 'custom_basemap'
      )

    lf <- lf %>%
      addMarkers(data = points())


  })
}

shinyApp(ui, server)
