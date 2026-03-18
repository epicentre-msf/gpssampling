. <- function(keyword) {
  # print(sprintf('. %s', keyword))
  suppressWarnings(translator()$t(keyword, session = NULL))
}

.. <- function(keyword) {
  # print(sprintf('.. %s', keyword))
  suppressWarnings(translator()$t(keyword))
}

# styler: block

translator <- function(language = 'en') {
  if (is.null(.globals$translator)) {
    .globals$translator <- shiny.i18n::Translator$new(translation_json_path = getPackagePath('translation.json'))
    .globals$translator$set_translation_language(language)
  }
  .globals$translator
}
