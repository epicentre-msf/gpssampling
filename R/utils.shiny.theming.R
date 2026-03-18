# Shiny theming and styling utilities

# styler: block Theme

#' shinyAppMinimal
#'
#' @param ui A shiny.tag.list object representing the UI elements of the Shiny app.
#' @param server A function representing the server logic of the Shiny app.
#' @param theme A character string specifying the theme of the app. Choices include 'bootstrap_3', 'bootstrap_5', and 'epicentre'.
#' @param port An integer specifying the port number for running the Shiny app.
#'
#' @return NULL
#'
shinyAppMinimal <- function(
  ui,
  server = NULL,
  theme = 'epicentre',
  port = 8000L
) {
  checkmate::assertClass(ui, classes = 'shiny.tag.list')
  checkmate::assertFunction(server, nargs = 0L, null.ok = TRUE)
  checkmate::assertChoice(
    theme,
    choices = c('bootstrap_3', 'bootstrap_5', 'epicentre')
  )

  ui <- switch(
    theme,
    bootstrap_3 = shiny::fillPage(
      useWaiter(),
      ui,
      theme = bslib::bs_theme(version = 3L)
    ),
    bootstrap_5 = bslib::page_fillable(
      useWaiter(),
      ui,
      theme = bslib::bs_theme(version = 5L)
    ),
    epicentre = bslib::page_fillable(useWaiter(), ui, theme = theme())
  )

  if (is.null(server)) {
    server <- function() {
    }
  }

  shiny::shinyApp(
    ui = ui,
    server = function(input, output, session) {
      session$onSessionEnded(
        function() {
          shiny::stopApp()
        }
      )

      bslib::bs_themer(gfonts = TRUE, gfonts_update = FALSE)

      server()

      waiter::waiter_hide()
    },
    options = list(port = port)
  )
}

#' Theme function for customizing Bootstrap themes
#'
#' This function allows customization of Bootstrap themes by adjusting various elements such as fonts, colors, buttons, and cards.
#'
#' @param preview Logical indicating whether to preview the theme changes (default is FALSE)
#'
#' @return A customized Bootstrap theme object
#'
#' @examples
#' theme()
#'
theme <- function(preview = FALSE) {
  #   <link rel="preconnect" href="https://fonts.googleapis.com">
  # <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
  # <link href="https://fonts.googleapis.com/css2?family=Fira+Sans:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap" rel="stylesheet">
  theme_font <- bslib::font_collection(
    bslib::font_google('Fira+Sans', wght = 1:9 * 100L, ital = 0:1),
    'Roboto',
    'Helvetica',
    'Arial',
    'sans-serif'
  )

  theme <- bslib::bs_theme(
    preset = 'bootstrap',
    spacer = '0.5rem',
    # 'modal-padding' = '0rem',
    # 'modal-header-padding' = '0rem',
    # 'modal-header-padding-y' = '0rem',
    # 'navbar-padding-y' = '0.25rem',

    # Typography
    base_font = theme_font,
    code_font = theme_font,
    heading_font = theme_font,
    'font-size-base' = '0.9rem',
    'font-weight-normal' = '400',

    # # Controls the default grayscale palette
    bg = '#FFFFFF',
    fg = '#474747',
    light = '#FFFFFF',

    # # Controls the accent (e.g., hyperlink, button, etc) colors
    primary = '#dbe5f0',
    # secondary = '#c03939',
    # Can also add lower-level customization
    # 'input-border-color' = '#EA80FC'
    # 'bs-body-color' = 'rgb(119, 119, 119)'

    # Button
    # `btn-color` = '#f2f2f2',
    # `btn-link-hover-color` = '#e7e7e7',
    `btn-white-space` = 'nowrap',

    # Options
    `enable-gradients` = TRUE,
    `enable-shadows` = TRUE,
    `link-color` = 'rgb(51, 122, 183)',
    `modal-sm` = '400px'
  )

  # Lower-level bs_add_*() functions allow you to work more
  # directly with the underlying Sass code
  # theme <- bslib::bs_add_variables(theme, 'spacer' = '0rem', .where = "declarations")
  # theme <- bslib::bs_add_variables(theme, 'navbar-padding-y' = '$spacer / 4', .where = "declarations") # $spacer / 2

  theme <- bslib::bs_add_rules(theme, 'body {background-color: #FAFAFA}')

  theme <- bslib::bs_add_rules(
    theme,
    '.form-label-sm {margin-bottom: 0.2rem !important;}'
  )

  # theme <- bslib::bs_add_rules(theme, '.vscomp-dropbox-container {z-index: 10000 !important;}')
  theme <- bslib::bs_add_rules(
    theme,
    '
    .vscomp-toggle-button {
      border: 1px solid var(--bs-border-color) !important;
      border-radius: var(--bs-border-radius);
    }'
  )
  theme <- bslib::bs_add_rules(
    theme,
    '
    .vscomp-wrapper.focused .vscomp-toggle-button, .vscomp-wrapper:focus .vscomp-toggle-button {
      border-color: #86b7fe !important;
      box-shadow: inset 0 1px 2px rgba(71, 71, 71, 0.075), 0 0 0 0.25rem rgba(13, 110, 253, 0.25) !important;
    }'
  )

  # Button rules
  theme <- bslib::bs_add_rules(
    theme,
    '
    .btn-default {
      --bs-btn-bg: #f2f2f2;
      --bs-btn-hover-bg: #e7e7e7;
      --bs-btn-active-bg: #dbe5f0;
      --bs-btn-color: #5b5b5b;
      --bs-btn-hover-color: #5b5b5b;
      --bs-btn-active-color: #1d3d63;
      border: none !important;
      box-shadow: none !important;
    }

    .btn-xs {
      --bs-btn-padding-y: .15rem;
      --bs-btn-padding-x: .25rem;
      --bs-btn-font-size: .675rem;
      --bs-btn-border-radius: var(--bs-border-radius-sm)
    }
  '
  )

  # Card rules
  theme <- bslib::bs_add_rules(
    theme,
    '
    div.card {
      border-radius: 0px;
    }

    div.card:has(div.card-header > ul.nav) {
      --bs-card-border-color: none;
    }

    div.card-header:has(ul.nav) {
      --bs-card-border-color: var(--bs-border-color);
    }

    div.card:has(div.card-header > ul.nav) > div.tab-content {
      border: solid var(--bs-border-width) var(--bs-border-color);
      border-top: none;
    }

    .nav {
      --bs-nav-link-padding-x: 0.5rem;
      --bs-nav-link-padding-y: 0.25rem;
    }
  '
  )

  if (preview) {
    bslib::bs_theme_preview(theme)
  }

  theme
}

themeGallery <- function() {
  tags <- shinyWidgets::pickerInput(
    inputId = 'groups',
    label = 'Select one from each group below:',
    choices = list(
      Group1 = c('1', '2', '3', '4'),
      Group2 = c('A', 'B', 'C', 'D')
    ),
    multiple = TRUE,
    options = list('max-options-group' = 1L)
  )
  themePreview(tags)
}

themePreview <- function(..., reset = TRUE) {
  if (reset) {
    path_cache <- sass::sass_cache_context_dir()
    if (fs::dir_exists(path_cache)) {
      fs::dir_delete(path_cache)
    }
  }
  dots <- rlang::list2(...)
  if (length(dots)) {
    shiny::shinyApp(
      ui = bslib::page_fillable(
        theme = theme(),
        shiny::wellPanel(
          ...
        )
      ),
      server = function(input, output) {
      }
    )
  } else {
    bslib::bs_theme_preview(theme(), with_themer = FALSE)
  }
}

# themePreview(actionButton("largeButton", "Large Primary Button", class = "btn-primary btn-lg"))
# themePreview(actionButton("largeButton", "Large Primary Button", class = "btn-primary btn-light"))
