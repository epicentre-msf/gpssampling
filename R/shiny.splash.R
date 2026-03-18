# styler: block 

tabSplash <- function(id = 'mod_splash', vars, project) {

  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {

      ns <- session$ns

      shiny::observeEvent(input$act_start, {
        shiny::hideTab(inputId = 'navbar', target = 'mod_splash')
      })

    }
  )
} 

tabUISplash <- function(id = 'mod_splash') {

  ns <- shiny::NS(id)

  shiny::tabPanel(
    title = '', id = id, value = id,
    splash(
      img = sample(1:4, 1L),
      shiny::div(
        class = 'splash-caption',
        shiny::div(
          class = 'caption-content',
          shiny::img(src = 'assets/img/logo-epicentre-white.png', width = 250L, style = 'left: 20%;'),
          shiny::img(src = 'assets/img/logo-msf-white.png', width = 125L, style = 'right: 20%;'),
          shiny::div(class = 'font-alt title-size-1 mb-30', 'Rapid Population Estimation'),
          shiny::div(class = 'font-alt title-size-4 mb-40', 'Geo-Pop'),
          button(inputId = ns('act_start'), label = 'Start')
        )
      )
    )
  )
} 

