# devtools::test(filter = 'test.utils.shiny', stop_on_failure = TRUE)

# cleanSessionSelenium()

theme <- bslib::bs_theme(
  # "font-size-base" = "0.9rem",
  'enable-rounded' = TRUE,
  'line-height-base' = 1.3,
  'line-height-sm' = 1.3,
  'line-height-lg' = 1.3,
  spacer = '0.2rem',
  version = 5L
)


app_bg <- startApp(
  app = shiny::shinyApp(
    ui =  shiny::tagList(
      shiny::wellPanel(
        imola::flexPanel(
          align_items = 'flex-start', grow = 0L,
          shiny::actionButton("goButton", "Go!", class = "btn-success"),
          button('btn', label = 'Default'),
          button('btn', label = 'Primary', semantic = 'primary'),
          button('btn', label = 'Secondary', semantic = 'secondary'),
          button('btn', label = 'Success', semantic = 'success'),
          button('btn', label = 'Danger', semantic = 'danger'),
          button('btn', label = 'Warning', semantic = 'warning'),
          button('btn', label = 'Info', semantic = 'info'),
          button('btn', label = 'Light', semantic = 'light'),
          button('btn', label = 'Dark', semantic = 'dark'),
          button('btn', label = 'Link', semantic = 'link')
        )
      )
    ),
    server = function(input, output, session) {}
  )
)

# startApp(
#   function() {
#     devtools::load_all()

#     shinyAppMinimal(
#       ui =
#         shiny::tagList(
#           shiny::wellPanel(
#             imola::flexPanel(
#               align_items = 'flex-start', grow = 0L,
#               button('btn', label = 'Default'),
#               button('btn', label = 'Primary', semantic = 'primary'),
#               button('btn', label = 'Secondary', semantic = 'secondary'),
#               button('btn', label = 'Success', semantic = 'success'),
#               button('btn', label = 'Danger', semantic = 'danger'),
#               button('btn', label = 'Warning', semantic = 'warning'),
#               button('btn', label = 'Info', semantic = 'info'),
#               button('btn', label = 'Light', semantic = 'light'),
#               button('btn', label = 'Dark', semantic = 'dark'),
#               button('btn', label = 'Link', semantic = 'link')
#             )
#           ),
#           shiny::wellPanel(
#             imola::flexPanel(
#               align_items = 'flex-start', grow = 0L,
#               button('btn', label = 'Default', outline = TRUE),
#               button('btn', label = 'Primary', semantic = 'primary', outline = TRUE),
#               button('btn', label = 'Secondary', semantic = 'secondary', outline = TRUE),
#               button('btn', label = 'Success', semantic = 'success', outline = TRUE),
#               button('btn', label = 'Danger', semantic = 'danger', outline = TRUE),
#               button('btn', label = 'Warning', semantic = 'warning', outline = TRUE),
#               button('btn', label = 'Info', semantic = 'info', outline = TRUE),
#               button('btn', label = 'Light', semantic = 'light', outline = TRUE),
#               button('btn', label = 'Dark', semantic = 'dark', outline = TRUE),
#               button('btn', label = 'Link', semantic = 'link', outline = TRUE)
#             )
#           ),
#           shiny::wellPanel(
#             imola::flexPanel(
#               align_items = 'baseline', grow = 0L,
#               button('btn', label = 'Large', semantic = 'primary', size = 'lg'),
#               button('btn', label = 'Normal', semantic = 'primary'),
#               button('btn', label = 'Small', semantic = 'primary', size = 'sm'),
#               button('btn', label = 'XSmall', semantic = 'primary', size = 'xs')
#             )
#           )
#         )
#     )
#   }
# )

session <- getSeleniumSession()

test_that('button', {
  selenider::open_url(url = 'https://ashbythorpe.github.io/selenider/articles/test-site.html')
})
