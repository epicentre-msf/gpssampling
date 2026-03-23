# Page Navbar

Create a Bootstrap navbar for a page.

## Usage

``` r
page_navbar(
  ...,
  title = NULL,
  id = NULL,
  selected = NULL,
  position = c("static-top", "fixed-top", "fixed-bottom"),
  header = NULL,
  footer = NULL,
  bg = NULL,
  inverse = "auto",
  collapsible = TRUE,
  fluid = TRUE,
  theme = bslib::bs_theme(),
  window_title = NA,
  lang = NULL,
  h100 = getOption("gpssampling.h100", TRUE),
  shadow = TRUE
)
```

## Arguments

- ...:

  Arguments passed on to
  [`bslib::page_navbar`](https://rstudio.github.io/bslib/reference/page_navbar.html)

  `title`

  :   A (left-aligned) title to place in the card header/footer. If
      provided, other nav items are automatically right aligned.

  `id`

  :   a character string used for dynamically updating the container
      (see
      [`nav_select()`](https://rstudio.github.io/bslib/reference/nav_select.html)).

  `selected`

  :   a character string matching the `value` of a particular
      [`nav_panel()`](https://rstudio.github.io/bslib/reference/nav-items.html)
      item to selected by default.

  `sidebar`

  :   A
      [`sidebar()`](https://rstudio.github.io/bslib/reference/sidebar.html)
      component to display on every
      [`nav_panel()`](https://rstudio.github.io/bslib/reference/nav-items.html)
      page.

  `fillable`

  :   Whether or not to allow `fill` items to grow/shrink to fit the
      browser window. If `TRUE`, all
      [`nav_panel()`](https://rstudio.github.io/bslib/reference/nav-items.html)
      pages are `fillable`. A character vector, matching the `value` of
      [`nav_panel()`](https://rstudio.github.io/bslib/reference/nav-items.html)s
      to be filled, may also be provided. Note that, if a `sidebar` is
      provided, `fillable` makes the main content portion fillable.

  `fillable_mobile`

  :   Whether or not `fillable` pages should fill the viewport's height
      on mobile devices (i.e., narrow windows).

  `gap`

  :   A [CSS length
      unit](https://rstudio.github.io/htmltools/reference/validateCssUnit.html)
      defining the `gap` (i.e., spacing) between elements provided to
      `...`.

  `padding`

  :   Padding to use for the body. This can be a numeric vector (which
      will be interpreted as pixels) or a character vector with valid
      CSS lengths. The length can be between one and four. If one, then
      that value will be used for all four sides. If two, then the first
      value will be used for the top and bottom, while the second value
      will be used for left and right. If three, then the first will be
      used for top, the second will be left and right, and the third
      will be bottom. If four, then the values will be interpreted as
      top, right, bottom, and left respectively.

  `header`

  :   UI element(s)
      ([htmltools::tags](https://rstudio.github.io/htmltools/reference/builder.html))
      to display *above* the nav content. For `card`-based navsets,
      these elements are implicitly wrapped in a `card_body()`. To
      control things like `padding`, `fill`, etc., wrap the elements in
      an explicit
      [`card_body()`](https://rstudio.github.io/bslib/reference/card_body.html).

  `footer`

  :   UI element(s)
      ([htmltools::tags](https://rstudio.github.io/htmltools/reference/builder.html))
      to display *below* the nav content. For `card`-based navsets,
      these elements are implicitly wrapped in a `card_body()`. To
      control things like `padding`, `fill`, etc., wrap the elements in
      an explicit
      [`card_body()`](https://rstudio.github.io/bslib/reference/card_body.html).

  `navbar_options`

  :   Options to control the appearance and behavior of the navbar. Use
      [`navbar_options()`](https://rstudio.github.io/bslib/reference/navbar_options.html)
      to create the list of options.

  `fluid`

  :   `TRUE` to use fluid layout; `FALSE` to use fixed layout.

  `theme`

  :   A
      [`bs_theme()`](https://rstudio.github.io/bslib/reference/bs_theme.html)
      object.

  `window_title`

  :   the browser window title. The default value, `NA`, means to use
      any character strings that appear in `title` (if none are found,
      the host URL of the page is displayed by default).

  `lang`

  :   ISO 639-1 language code for the HTML page, such as "en" or "ko".
      This will be used as the lang in the `<html>` tag, as in
      `<html lang="en">`. The default (NULL) results in an empty string.

  `position`

  :   **\[deprecated\]** Please use
      [`navbar_options = navbar_options(position=)`](https://rstudio.github.io/bslib/reference/navbar_options.html)
      instead.

  `bg`

  :   **\[deprecated\]** Please use
      [`navbar_options = navbar_options(bg=)`](https://rstudio.github.io/bslib/reference/navbar_options.html)
      instead.

  `inverse`

  :   **\[deprecated\]** Please use
      [`navbar_options = navbar_options(inverse=)`](https://rstudio.github.io/bslib/reference/navbar_options.html)
      instead.

  `underline`

  :   **\[deprecated\]** Please use
      [`navbar_options = navbar_options(underline=)`](https://rstudio.github.io/bslib/reference/navbar_options.html)
      instead.

  `collapsible`

  :   **\[deprecated\]** Please use
      [`navbar_options = navbar_options(collapsible=)`](https://rstudio.github.io/bslib/reference/navbar_options.html)
      instead.

## Value

A `tag` object representing the page navbar.

## Examples

``` r
pageNavbar(title = 'My Page', bg = 'light')
#> Error in pageNavbar(title = "My Page", bg = "light"): could not find function "pageNavbar"
```
