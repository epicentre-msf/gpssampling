pkg_infos <- epi.actions::runTargetInfo()

pkg_url <- sprintf('%s/%s', pkg_infos$git$organisation, pkg_infos$name)

uri_shiny_server <- 'https://apps.msf.net'

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = '#>',
  #   cache = FALSE,
  #   dpi = 96,
  #   fig.path = 'man/figures/',
  #   message = FALSE,
  out.width = '100%'
  #   warning = FALSE
)

options(
  tibble.print_min = 5L,
  tibble.print_max = 5L,
  width = 150L
)
