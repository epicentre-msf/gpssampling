#' MDI HTML dependency
#'
#' Registers the Material Design Icons webfont via CDN.
#'
#' @return An `htmltools::htmlDependency` object.
#' @noRd
html_dependency_mdi <- function() {
  htmltools::htmlDependency(
    name = "mdi",
    version = "7.4.47",
    src = c(
      href = "https://cdn.jsdelivr.net/npm/@mdi/font@7.4.47"
    ),
    stylesheet = "css/materialdesignicons.min.css"
  )
}

#' Render an MDI icon
#'
#' @param name Character. MDI icon name (without the `mdi-` prefix).
#' @param class Character. Additional CSS classes.
#' @param size Character. Named size (`"xs"`, `"sm"`, `"md"`, `"lg"`) or
#'   a CSS font-size value (e.g. `"24px"`, `"1.5em"`).
#' @param color Character. CSS color value.
#'
#' @return An `htmltools::tags$i` element.
#' @noRd
icon_mdi <- function(name, class = NULL, size = NULL, color = NULL) {
  size_map <- c(xs = "18px", sm = "24px", md = "36px", lg = "48px")

  style_parts <- character(0)
  if (!is.null(size)) {
    css_size <- if (size %in% names(size_map)) size_map[[size]] else size
    style_parts <- c(style_parts, paste0("font-size:", css_size))
  }
  if (!is.null(color)) {
    style_parts <- c(style_parts, paste0("color:", color))
  }

  style <- if (length(style_parts) > 0) {
    paste(style_parts, collapse = ";")
  } else {
    NULL
  }

  css_class <- paste(c("mdi", paste0("mdi-", name), class), collapse = " ")
  htmltools::tags$i(class = css_class, style = style)
}
