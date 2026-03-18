# styler: block Leaflet

addIndicator <- function(lf, level, indicator, indicator.var) {
  tbl <- switch(level,
    adm.0 = dplyr::select(surveillance$lvl.0$data, week.n, dplyr::one_of(indicator.var)),
    adm.1 = dplyr::select(surveillance$lvl.1$data, adm.1, week.n, dplyr::one_of(indicator.var))
  )

  tbl <- tidyr::spread(tbl, 'week.n', indicator.var)

  spdf <- switch(level,
    adm.0 = surveillance$lvl.0$spdf,
    adm.1 = surveillance$lvl.1$spdf
  )

  spdf@data <- dplyr::left_join(spdf@data, tbl)
  spdf@data[, 8:52] <- round(stats::runif(1:1035) * 5L)
  # spdf@data <- dplyr::select(spdf@data, adm.1, week.n, mov_death)

  tpjs <- geojsonio::topojson_list(spdf)

  addGeoJSONv2(
    lf,
    tpjs,
    color = '#EEEEEE',
    fill = TRUE,
    fillColor = 'white',
    layerId = spdf$id,
    label = spdf$adm.1.name,
    weight = 1L,
    fillOpacity = 0.8,
    popupProperty = 'popup',
    highlightOptions = leaflet::highlightOptions(weight = 2L, color = 'yellow', fillOpacity = 1L, opacity = 1L, bringToFront = TRUE, sendToBack = TRUE),
    labelProperty = 'adm.1.name'
  )
}

addLegend <- function(map, title = NULL, subtitle = NULL, width = 120L, position = 'bottomleft', ...) {
  if (!is.null(width)) {
    width <- sprintf('%spx', width)
  } else {
    width <- 'auto'
  }

  if (!is.null(subtitle)) {
    title_html <- sprintf(paste0(
      '<p class="info-title" style="width: %s">%s</p>',
      '<p class="info-subtitle">%s</p>'
    ), width, title, subtitle)
  } else {
    title_html <- sprintf(
      '<p class="info-title" style="width: %s">%s</p>', width, title
    )
  }

  leaflet::addLegend(
    map = map,
    position = position,
    title = title_html,
    ...
  )
}

addLegendCircle <- function(map, colors, labels, sizes, title, position, opacity = 0.5) {
  sizes_margin <- round((max(sizes) - sizes) / 2L)

  css_colors <- sprintf('%s; width: %spx; height: %spx; margin: 0 %spx 0 %spx', colors, sizes, sizes, sizes_margin, sizes_margin)
  css_labels <- sprintf("<div style='display: inline-block; height: %spx; margin-bottom: 4px; line-height: %spx;'>%s</div>", sizes, sizes, labels)

  leaflet::addLegend(
    map = map,
    colors = css_colors,
    labels = css_labels,
    opacity = opacity,
    title = title,
    position = position
  )
}

addLegendCustom <- function(map, title, subtitle = NULL, colors = NULL, labels = NULL, sizes = NULL, ...) {
  if (!is.null(subtitle)) {
    title <- sprintf('%s<br><div style="font-weight: normal">%s</div>', title, subtitle)
  }
  title <- sprintf('%s', title)
  if (!is.null(colors)) {
    colors <- paste0(colors, '; width:', sizes, 'px; height:', sizes, 'px;')
  }
  if (!is.null(labels)) {
    labels <- paste0("<div style='display: inline-block;height: ", sizes, 'px;margin-top: 4px;line-height: ', sizes, "px;'>", labels, '</div>')
  }
  if (!is.null(colors)) {
    lf_legend <- leaflet::addLegend(map, title = title, colors = colors, labels = labels, ...)
  } else {
    lf_legend <- leaflet::addLegend(map, title = title, ...)
  }

  lf_legend
}

addLegendSubtitle <- function(map, title, subtitle = NULL, ...) {
  if (!is.null(subtitle)) {
    title <- sprintf('%s<br><div style="font-weight: normal">%s</div>', title, subtitle)
  }
  title <- sprintf('%s', title)
  legend <- leaflet::addLegend(map, title = title, ...)
  legend
}

addPolygonsAdm <- function(map, data, group, layer_ids,
                           fill = TRUE,
                           fill_color = 'white',
                           fill_opacity = 0.2,
                           stroke = TRUE,
                           stroke_colour = '#888888',
                           stroke_opacity = 0.2,
                           stroke_width = 1L,
                           highlight = TRUE,
                           ...) {
  leaflet::addPolygons(
    map = map,
    data = data,
    color = stroke_colour,
    fill = fill,
    fillColor = 'white',
    fillOpacity = fill_opacity,
    group = group,
    highlightOptions =
      if (highlight) {
        leaflet::highlightOptions(
          color = col_blue,
          weight = 3L,
          bringToFront = TRUE,
          sendToBack = TRUE
        )
      } else {
        highlight_options <- NULL
      },
    label = layer_ids,
    layerId = layer_ids,
    labelOptions = leaflet::labelOptions(
      textOnly = FALSE,
      style = list('font-size' = '12px')
    ),
    opacity = stroke_opacity,
    options = leaflet::pathOptions(pane = 'admin'),
    stroke = stroke,
    weight = stroke_width,
    ...
  )
}

#' Add stars layer to a leaflet map
#'
#' @param map a mapview or leaflet object.
#' @param x a stars layer.
#' @param band the band number to be plotted.
#' @param colors the color palette (see colorNumeric) or function to use to
#' color the raster values (hint: if providing a function, set na.color
#' to "#00000000" to make NA areas transparent)
#' @param opacity the base opacity of the raster, expressed from 0 to 1
#' @param attribution the HTML string to show as the attribution for this layer
#' @param layerId the layer id
#' @param group the name of the group this raster image should belong to
#' (see the same parameter under addTiles)
#' @param project if TRUE, automatically project x to the map projection
#' expected by Leaflet (EPSG:3857); if FALSE, it's the caller's responsibility
#' to ensure that x is already projected, and that extent(x) is
#' expressed in WGS84 latitude/longitude coordinates
#' @param method the method used for computing values of the new,
#' projected raster image. "bilinear" (the default) is appropriate for
#' continuous data, "ngb" - nearest neighbor - is appropriate for categorical data.
#' Ignored if project = FALSE. See projectRaster for details.
#' @param maxBytes the maximum number of bytes to allow for the projected image
#' (before base64 encoding); defaults to 4MB.
#' @param data the data object from which the argument values are derived; by
#'   default, it is the \code{data} object provided to \code{leaflet()}
#'   initially, but can be overridden.
#' @param ... currently not used.
#'
#' @details
#' This is an adaption of \code{\link{addRasterImage}}. See that documentation
#' for details.
#'
#' @examples
#' \donttest{
#' library(stars)
#' library(leaflet)
#'
#' tif <- system.file('tif/L7_ETMs.tif', package = 'stars')
#' x <- read_stars(tif)
#' leaflet() |>
#'   addProviderTiles('OpenStreetMap') |>
#'   addStarsImage(x, project = TRUE)
#' }
#'
addStarsImage <- function(
    map,
    x,
    band = 1L,
    colors = 'Spectral',
    opacity = 1L,
    attribution = NULL,
    layerId = NULL,
    group = NULL,
    project = FALSE,
    method = c('auto', 'bilinear', 'ngb'),
    maxBytes = 4L * 1024L * 1024L,
    data = leaflet::getMapData(map),
    ...) {
  # this allows using `addStarsImage` directly on a leaflet pipe, without
  # specifying `x` (e.g., leaflet(read_stars(tif)) |>
  # addProviderTiles("OpenStreetMap") |> addStarsImage())
  #
  if (inherits(map, c('leaflet', 'leaflet_proxy')) && missing(x)) {
    x <- attributes(map[['x']])[['leafletData']]
  }

  stopifnot(inherits(x, 'stars'))

  if (any(attr(attr(x, 'dimensions'), 'raster')$affine != 0L) ||
    attr(attr(x, 'dimensions'), 'raster')$curvilinear) {
    warning(
      'cannot handle curvilinear or sheared stars images. Rendering regular grid.',
      call. = FALSE
    )
  }

  raster_is_factor <- is.factor(x[[1L]])
  method <- match.arg(method)
  if (method == 'auto') {
    if (raster_is_factor) {
      method <- 'ngb'
    } else {
      method <- 'bilinear'
    }
  }

  if (inherits(map, 'mapview')) map <- mapview2leaflet(map)
  if (is.null(group)) group <- 'stars'
  if (is.null(layerId)) layerId <- group

  if (project) {
    # if we should project the data
    if (utils::packageVersion('stars') >= '0.4-1') {
      projected <- stars::st_warp(x, crs = 3857L)
    } else {
      projected <- sf::st_transform(x, crs = 3857L)
    }

    # if data is factor data, make the result factors as well.
    # if (raster_is_factor) {
    #  projected <- raster::as.factor(projected)
    # }
  } else {
    # do not project data
    projected <- x
  }

  bb <- sf::st_as_sfc(sf::st_bbox(projected))
  bounds <- as.numeric(sf::st_bbox(sf::st_transform(bb, 4326L)))

  if (length(dim(projected)) == 2L) {
    layer <- projected[[1L]]
  } else {
    layer <- projected[[1L]][, , band]
  }

  if (!is.function(colors)) {
    if (method == 'ngb') {
      # 'factors'
      colors <- leaflet::colorFactor(
        colors,
        domain = NULL, na.color = '#00000000', alpha = TRUE
      )
    } else {
      # 'numeric'
      colors <- leaflet::colorNumeric(
        colors,
        domain = NULL, na.color = '#00000000', alpha = TRUE
      )
    }
  }

  if (is.null(attr(layer, 'color'))) {
    clrs <- colors(as.numeric(layer))
  } else {
    clrs <- colors(layer)
  }
  clrs <- grDevices::col2rgb(clrs, alpha = TRUE)
  tileData <- as.raw(clrs)

  # tileData <- as.numeric(layer) |>
  #   colors() |> grDevices::col2rgb(alpha = TRUE) |> as.raw()
  dim(tileData) <- c(4L, nrow(projected), ncol(projected))
  pngData <- png::writePNG(tileData)
  if (length(pngData) > maxBytes) {
    stop(
      'Raster image too large; ', length(pngData),
      ' bytes is greater than maximum ', maxBytes, ' bytes'
    )
  }
  encoded <- base64enc::base64encode(pngData)
  uri <- paste0('data:image/png;base64,', encoded)

  latlng <- list(
    list(bounds[4L], bounds[1L]),
    list(bounds[2L], bounds[3L])
  )

  map <- leaflet::invokeMethod(
    map, data, 'addRasterImage', uri, latlng, layerId, group)

  leaflet::expandLimits(
    map,
    c(bounds[2L], bounds[4L]),
    c(bounds[1L], bounds[3L])
  )
}

calc_radius <- function(n, n_max, scale_factor = 20L) {
  sqrt(n) / sqrt(n_max) * scale_factor
}

colorBin <- function(scale, alpha = NA) {
  if (Inf %in% scale$bin) {
    scale_bin <- scale$bin
    scale_color <- rev(scale$color)
  } else {
    scale_bin <- c(scale$bin, 'Inf')
    scale_color <- scale$color
  }
  p <- leaflet::colorBin(
    palette = scales::alpha(scale_color, alpha),
    bins = scale_bin,
    na.color = scales::alpha('#dddddd', 0.4),
    alpha = !is.na(alpha)
  )
  p
}

easyButtonShiny <- function(inputId, ...) {
  leaflet::easyButton(
    id = paste0(inputId, '_btn'), ...,
    onClick = htmlwidgets::JS(sprintf('function(btn, map){ Shiny.onInputChange(\'%s\' , [1,Math.random()]); }', inputId))
  )
}

fitToSPDFBounds <- function(lf, spdf) {
  bb <- spdf@bbox
  leaflet::fitBounds(
    lf,
    bb[1L, 1L], # east
    bb[2L, 1L], # north
    bb[1L, 2L], # west
    bb[2L, 2L] # south
  )
}

fitToSpatialFeatureBounds <- function(map, sf) {
  bbox <- sf::st_bbox(sf) |> as.vector()
  leaflet::fitBounds(
    map,
    bbox[1L],
    bbox[2L],
    bbox[3L],
    bbox[4L]
  )
}

flyToSPDF <- function(lf, spdf, zoom) {
  pp <- gCentroid(spdf)
  leaflet::flyTo(
    lf,
    pp$x,
    pp$y,
    zoom
  )
}

flyToSPDFBounds <- function(lf, spdf) {
  bb <- spdf@bbox
  leaflet::flyToBounds(
    lf,
    bb[1L, 1L], # east
    bb[2L, 1L], # north
    bb[1L, 2L], # west
    bb[2L, 2L] # south
  )
}

flyToSpatialFeature <- function(map, sf, zoom) {
  if (is.defined(sf)) {
    leaflet::flyTo(map,
      lng = sf$centroid_lon,
      lat = sf$centroid_lat,
      zoom = zoom,
      options = list(
        duration = 0.1,
        easeLinearity = 0.75
      )
    )
  }
}

flyToSpatialFeatureBounds <- function(map, sf) {
  bbox <- sf::st_bbox(sf) |> as.vector()
  leaflet::flyToBounds(
    map,
    bbox[1L],
    bbox[2L],
    bbox[3L],
    bbox[4L]
  )
}

flyToSpatialLL <- function(map, lng, lat, zoom) {
  leaflet::flyTo(map,
    lng = lng,
    lat = lat,
    zoom = zoom,
    options = list(
      duration = 0.1,
      easeLinearity = 0.75
    )
  )
}

getMapBbox <- function(bounds) {
  c(
    xmin = bounds$west,
    ymin = bounds$south,
    xmax = bounds$east,
    ymax = bounds$north
  )
}

getMapBounds <- function(bbox) {
  list(
    west  = bbox$xmin,
    south = bbox$ymin,
    east  = bbox$xmax,
    north = bbox$ymax
  )
}

insertSelectBasemaps <- function(inputId, selected = NULL, position = 'topleft') {
  shiny::insertUI(
    selector = paste0('#', inputId, '_hollow-', position), where = 'beforeEnd', ui =
      pickerInputBasemaps(inputId = inputId, selected = selected)
  )
}

insertUIInMap <- function(inputId, position = 'topleft', ui) {
  shiny::insertUI(selector = paste0('#', inputId, '_hollow-', position), where = 'beforeEnd', ui = ui)
}

leafletProxy <- function(map_id = 'map', deferUntilFlush = FALSE) {
  leaflet::leafletProxy(mapId = map_id, deferUntilFlush = deferUntilFlush)
}

leaflet <- function(
    session, inputId,
    boxZoom = TRUE,
    dragging = TRUE,
    fullscreen = TRUE,
    fullscreen_pos = 'bottomleft',
    keyboard = TRUE,
    prefer_canvas = NULL,
    searchbar = TRUE,
    searchbar_pos = 'bottomright',
    scalebar = TRUE,
    scalebar_pos = 'bottomright',
    zoom = TRUE,
    zoom_min = NULL,
    zoom_max = NULL,
    zoom_position = 'bottomleft',
    zoom_snap = 1L,
    zoom_delta = zoom_snap,
    attribution = TRUE,
    rendering = TRUE,
    view = list(
      lng = -93.65,
      lat = 42.0285,
      zoom = 17L
    ),
    ...) {
  id <- session$ns(inputId)

  f <- getDirAssets('js', 'leaflet', 'shiny', 'render.js')

  s <- readLines(f)
  s <- paste(s, collapse = '\n')
  # s <- gsub('%id', id, s)
  # s <- gsub('%font.size', font.size, s)
  # s <- gsub('%margin.left', margin.left, s)
  # s <- gsub('%margin.right', margin.right, s)
  # s <- gsub('%margin.top', margin.top, s)
  # s <- gsub('%margin.bottom', margin.bottom, s)
  # s <- gsub('%margin.pad', margin.pad, s)

  lf <- leaflet::leaflet(
    options =
      leaflet::leafletOptions(
        attributionControl = attribution,
        boxZoom = boxZoom,
        doubleClickZoom = FALSE,
        dragging = dragging,
        keyboard = keyboard,
        minZoom = zoom_min,
        maxZoom = zoom_max,
        preferCanvas = prefer_canvas,
        zoomControl = FALSE,
        zoomSnap = zoom_snap,
        zoomDelta = zoom_delta,
        ...
      )
  )

  lf <- lf |>
    addCustomControl(id = paste0(id, '_hollow-topright'), position = 'topright') |>
    addCustomControl(id = paste0(id, '_hollow-topleft'), position = 'topleft') |>
    addCustomControl(id = paste0(id, '_hollow-bottomright'), position = 'bottomright')

  if (zoom) {
    lf <-
      leaflet::addEasyButtonBar(lf,
        position = zoom_position,
        easyButtonShiny(inputId = paste0(id, '_zoom_in'), icon = icon(name = 'plus'), title = 'Zoom In'),
        easyButtonShiny(inputId = paste0(id, '_zoom_extent'), icon = icon(name = 'home'), title = 'Zoom Extent'),
        easyButtonShiny(inputId = paste0(id, '_zoom_out'), icon = icon(name = 'minus'), title = 'Zoom Out')
      )
  }
  if (fullscreen) {
    lf <- leaflet.extras::addFullscreenControl(lf, position = fullscreen_pos)
  }
  if (scalebar) {

    lf <- leaflet::addScaleBar(lf, position = scalebar_pos)
  }
  if (searchbar) {
    lf <- leaflet.extras::addSearchOSM(lf,
      options =
        leaflet.extras::searchOptions(
          autoType = TRUE,
          # autoCollapse = TRUE,
          # firstTipSubmit = TRUE,
          hideMarkerOnCollapse = TRUE,
          minLength = 2L,
          tipAutoSubmit = FALSE,
          position = searchbar_pos
        )
    )
  }

  lf <- leaflet::setView(lf,
    lng = view$lng,
    lat = view$lat,
    zoom = view$zoom
  )

  if (rendering) {
    lf <- htmlwidgets::onRender(lf, s)
  }

  lf
}

makeInternalIcon <- function(icon, classname) {
  path <- fs::path_package(package = utils::packageName(), 'assets')
  leaflet::makeIcon(
    iconUrl = sprintf('%s/icons/ocha/%s.png', path, icon),
    iconRetinaUrl = sprintf('%s/icons/ocha/%s-2x.png', path, icon),
    iconWidth = 24L,
    iconHeight = 24L,
    className = classname
  )
}

observeEventBasemaps <- function(session, input, vars, inputId) {
  vars$map.tile.provider.road <- 'road.mapbox'
  vars$map.tile.provider.sat <- 'sat.mapbox'
  vars$map.tile.provider.terrain <- 'terrain.mapbox'
  vars$map.tile.provider.dark <- 'dark.mapbox'
  vars$map.tile.provider.gray <- 'gray.mapbox'
  vars$map.tile.provider.light <- 'light.mapbox'

  shiny::observeEvent(input[[paste0(inputId, '_basemap')]], {
    vars$map.tile.type <- input[[paste0(inputId, '_basemap')]]

    shiny::isolate(
      switch(vars$map.tile.type,
        road.mapbox = vars$map.tile.provider.road <- vars$map.tile.type,
        road.google = vars$map.tile.provider.road <- vars$map.tile.type,
        road.osm.hot = vars$map.tile.provider.road <- vars$map.tile.type,
        road.osm = vars$map.tile.provider.road <- vars$map.tile.type,
        sat.bing = vars$map.tile.provider.sat <- vars$map.tile.type,
        sat.esri = vars$map.tile.provider.sat <- vars$map.tile.type,
        sat.google = vars$map.tile.provider.sat <- vars$map.tile.type,
        sat.mapbox = vars$map.tile.provider.sat <- vars$map.tile.type,
        sat.maxar = vars$map.tile.provider.sat <- vars$map.tile.type,
        terrain.mapbox = vars$map.tile.provider.terrain <- vars$map.tile.type,
        terrain.google = vars$map.tile.provider.terrain <- vars$map.tile.type,
        terrain.mapbox = vars$map.tile.provider.terrain <- vars$map.tile.type,
        dark.mapbox = vars$map.tile.provider.dark <- vars$map.tile.type,
        gray.mapbox = vars$map.tile.provider.gray <- vars$map.tile.type,
        gray.osm = vars$map.tile.provider.gray <- vars$map.tile.type,
        light.mapbox = vars$map.tile.provider.light <- vars$map.tile.type,
        light.mapbox.blue = vars$map.tile.provider.light <- vars$map.tile.type
      )
    )
  })

  shiny::observeEvent(input[[paste0(inputId, '_basemap_toggle_type')]], {
    switch(vars$map.tile.type,
      road.google = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = vars$map.tile.provider.sat),
      road.mapbox = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = vars$map.tile.provider.sat),
      road.osm.hot = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = vars$map.tile.provider.sat),
      road.osm = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = vars$map.tile.provider.sat),
      sat.bing = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = vars$map.tile.provider.terrain),
      sat.esri = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = vars$map.tile.provider.terrain),
      sat.google = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = vars$map.tile.provider.terrain),
      sat.mapbox = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = vars$map.tile.provider.terrain),
      sat.maxar = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = vars$map.tile.provider.terrain),
      terrain.google = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = vars$map.tile.provider.dark),
      terrain.mapbox = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = vars$map.tile.provider.dark),
      dark.mapbox = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = vars$map.tile.provider.gray),
      gray.mapbox = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = vars$map.tile.provider.light),
      gray.osm = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = vars$map.tile.provider.light),
      light.mapbox = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = vars$map.tile.provider.road),
      light.mapbox.blue = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = vars$map.tile.provider.road)
    )
  })

  shiny::observeEvent(input[[paste0(inputId, '_basemap_toggle_provider')]], {
    switch(vars$map.tile.type,
      road.google = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = 'road.mapbox'),
      road.mapbox = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = 'road.osm'),
      road.osm = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = 'road.osm.hot'),
      road.osm.hot = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = 'road.google'),
      sat.bing = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = 'sat.esri'),
      sat.esri = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = 'sat.google'),
      sat.google = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = 'sat.mapbox'),
      sat.mapbox = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = 'sat.maxar'),
      sat.maxar = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = 'sat.bing'),
      terrain.google = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = 'terrain.mapbox'),
      terrain.mapbox = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = 'terrain.google'),
      dark.mapbox = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = 'dark.mapbox'),
      gray.mapbox = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = 'gray.osm'),
      gray.osm = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = 'gray.mapbox'),
      light.mapbox = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = 'light.mapbox.blue'),
      light.mapbox.blue = shinyWidgets::updatePickerInput(session, inputId = paste0(inputId, '_basemap'), selected = 'light.mapbox')
    )
  })

  shiny::observe({
    req(vars$map.tile.type)

    proxy <- leafletProxy(inputId)
    proxy <- setBasemap(proxy, vars$map.tile.type)
  })
}

observeEventMap <- function(session, input, vars, inputId) {
  shiny::observeEvent(input[[paste0(inputId, '_zoom_out')]], {
    map_bounds <- input[[paste0(inputId, '_bounds')]]
    map_zoom <- input[[paste0(inputId, '_zoom')]]

    proxy <- leafletProxy(inputId, deferUntilFlush = FALSE)

    leaflet::setView(proxy,
      lat  = (map_bounds$north + map_bounds$south) / 2L,
      lng  = (map_bounds$east + map_bounds$west) / 2L,
      zoom =  map_zoom - 1L
    )
  })

  shiny::observeEvent(input[[paste0(inputId, '_zoom_in')]], {
    map_bounds <- input[[paste0(inputId, '_bounds')]]
    map_zoom <- input[[paste0(inputId, '_zoom')]]

    proxy <- leafletProxy(inputId, deferUntilFlush = FALSE)

    leaflet::setView(proxy,
      lat  = (map_bounds$north + map_bounds$south) / 2L,
      lng  = (map_bounds$east + map_bounds$west) / 2L,
      zoom =  map_zoom + 1L
    )
  })

  shiny::observeEvent(input[[paste0(inputId, '_zoom')]], {
    map_zoom <- input[[paste0(inputId, '_zoom')]]

    if (map_zoom <= 0L) {
      shinyjs::disable('btn_zoom_out')
    } else {
      shinyjs::enable('btn_zoom_out')
    }

    if (map_zoom >= 20L) {
      shinyjs::disable('btn_zoom_in')
    } else {
      shinyjs::enable('btn_zoom_in')
    }
  })

  shiny::observeEvent(input[[paste0(inputId, '_zoom_extent')]], {
    if (is.null(vars$map_bounds)) {
      return()
    }

    proxy <- leafletProxy(inputId, deferUntilFlush = FALSE)

    leaflet::fitBounds(
      proxy,
      vars$map_bounds$east[[1L]],
      vars$map_bounds$north[[1L]],
      vars$map_bounds$west[[1L]],
      vars$map_bounds$south[[1L]]
    )
  })
}

pickerInputBasemaps <- function(inputId, selected = NULL) {
  pickerInputEx(
    inputId = paste0(inputId, '_basemap'),
    inline = FALSE,
    label = NULL,
    width = 200L,
    choices = list(
      Road = list(
        'Road: Google'        = 'road.google',
        'Road: Mapbox'        = 'road.mapbox',
        'Road: OSM'           = 'road.osm',
        'Road: OSM (HOT)'     = 'road.osm.hot'
      ),
      Satellite = list(
        'Sat.: Bing'          = 'sat.bing',
        'Sat.: ESRI'          = 'sat.esri',
        'Sat.: Google'        = 'sat.google',
        'Sat.: Mapbox'        = 'sat.mapbox',
        'Sat.: Maxar'         = 'sat.maxar'
      ),
      Terrain = list(
        'Terrain: Google'        = 'terrain.google',
        'Terrain: Mapbox'        = 'terrain.mapbox'
      ),
      Dark = list(
        'Dark: Mapbox' = 'dark.mapbox'
      ),
      Gray = list(
        'Gray: Mapbox'        = 'gray.mapbox',
        'Gray: OSM'           = 'gray.osm'
      ),
      Light = list(
        'Light: Mapbox'        = 'light.mapbox',
        'Light: Mapbox (Blue)' = 'light.mapbox.blue'
      )
    ),
    buttons = list(
      list(id = 'toggle_type', icon = 'map-o'),
      list(id = 'toggle_provider', icon = 'angle-double-right')
    ),
    options = shinyWidgets::pickerOptions(
      dropdownAlignRight = 'auto'
    ),
    size = 'sm',
    selected = selected
  )
}

setBasemap <- function(
    lf,
    basemap,
    api.key.bing = Sys.getenv('MAPS_API_KEY_BING'),
    api.key.mapbox = Sys.getenv('MAPS_API_KEY_MAPBOX')) {
  lf <- leaflet::removeTiles(lf, layerId = 'base')

  if (grepl('google', basemap, fixed = TRUE)) {
    shinyjs::hide(selector = '.leaflet-control-attribution')
  } else {
    shinyjs::show(selector = '.leaflet-control-attribution')
  }

  lf <- switch(basemap,
    road.osm = leaflet::addProviderTiles(lf, leaflet::providers$OpenStreetMap,
      layerId = 'base',
      options = leaflet::tileOptions(maxNativeZoom = 19L, maxZoom = 22L, zIndex = 200L)
    ),
    road.osm.hot = leaflet::addProviderTiles(lf, leaflet::providers$OpenStreetMap.HOT,
      layerId = 'base',
      options = leaflet::tileOptions(maxNativeZoom = 19L, maxZoom = 22L, zIndex = 200L)
    ),
    road.google = addGoogleTiles(lf,
      layerId = 'base',
      attribution = '',
      type = 'road',
      options = leaflet::tileOptions(maxZoom = 22L, zIndex = 200L, updateWhenZooming = TRUE)
    ),
    road.mapbox = leaflet::addTiles(lf,
      layerId = 'base',
      attribution = '<a href=\"https://www.mapbox.com/about/maps/\" target=\"_blank\">&copy; Mapbox</a> <a href=\"https://openstreetmap.org/about/\" target=\"_blank\">&copy; OpenStreetMap</a> <a class=\"mapbox-improve-map\" href=\"https://www.mapbox.com/map-feedback/\" target=\"_blank\">Improve this map</a>',
      urlTemplate = sprintf('https://api.mapbox.com/styles/v1/s-balandine/ckhuxxyag04pd19tem7udhdxe/tiles/256/{z}/{x}/{y}@2x?access_token=%s', api.key.mapbox),
      options = leaflet::tileOptions(maxNativeZoom = 18L, maxZoom = 22L, zIndex = 200L)
    ),
    sat.bing = leaflet.extras::addBingTiles(lf,
      layerId = 'base',
      apikey = api.key.bing,
      imagerySet = 'Aerial',
      options = leaflet::tileOptions(maxNativeZoom = 19L, maxZoom = 22L, zIndex = 200L)
    ),
    sat.maxar = leaflet::addTiles(lf,
      layerId = 'base',
      attribution = '<a href=\"https://wiki.openstreetmap.org/wiki/Maxar\" target=\"_blank\">&copy; Maxar</a>',
      urlTemplate = sprintf('https://services.digitalglobe.com/earthservice/tmsaccess/tms/1.0.0/DigitalGlobe:ImageryTileService@EPSG:3857@jpg/{z}/{x}/{-y}.jpg?connectId=552c824a-5d4b-4bea-969f-06c8b50b80bc&foo=premium', api.key.mapbox),
      options = leaflet::tileOptions(maxNativeZoom = 19L, maxZoom = 22L, zIndex = 200L)
    ),
    sat.mapbox = leaflet::addTiles(lf,
      layerId = 'base',
      attribution = '<a href=\"https://www.mapbox.com/about/maps/\" target=\"_blank\">&copy; Mapbox</a> <a href=\"http://www.openstreetmap.org/about/\" target=\"_blank\">&copy; OpenStreetMap</a> <a class=\"mapbox-improve-map\" href=\"https://www.mapbox.com/map-feedback/\" target=\"_blank\">Improve this map</a> <a href=\"https://www.digitalglobe.com/\" target=\"_blank\">&copy; DigitalGlobe</a>',
      urlTemplate = sprintf('https://{s}.tiles.mapbox.com/v4/mapbox.satellite/{z}/{x}/{y}.png?access_token=%s', api.key.mapbox),
      options = leaflet::tileOptions(maxNativeZoom = 18L, maxZoom = 22L, zIndex = 200L, subdomains = 'abcd')
    ),
    sat.esri = leaflet::addProviderTiles(lf, leaflet::providers$Esri.WorldImagery,
      layerId = 'base',
      options = leaflet::tileOptions(maxNativeZoom = 18L, maxZoom = 22L, zIndex = 200L)
    ),
    sat.google = addGoogleTiles(lf,
      layerId = 'base',
      attribution = '',
      type = 'satellite',
      options = leaflet::tileOptions(maxZoom = 22L, zIndex = 200L, updateWhenZooming = TRUE)
    ),
    terrain.google = addGoogleTiles(lf,
      layerId = 'base',
      attribution = '',
      type = 'terrain',
      options = leaflet::tileOptions(maxZoom = 22L, zIndex = 200L, updateWhenZooming = TRUE)
    ),
    terrain.mapbox = leaflet::addTiles(lf,
      layerId = 'base',
      attribution = '<a href=\"https://www.mapbox.com/about/maps/\" target=\"_blank\">&copy; Mapbox</a> <a href=\"https://openstreetmap.org/about/\" target=\"_blank\">&copy; OpenStreetMap</a> <a class=\"mapbox-improve-map\" href=\"https://www.mapbox.com/map-feedback/\" target=\"_blank\">Improve this map</a>',
      urlTemplate = sprintf('https://api.mapbox.com/styles/v1/s-balandine/ckhuy0eb402p41atdjafcgugv/tiles/256/{z}/{x}/{y}@2x?access_token=%s', api.key.mapbox),
      options = leaflet::tileOptions(maxNativeZoom = 18L, maxZoom = 22L, zIndex = 200L, subdomains = 'abcd')
    ),
    dark.mapbox = leaflet::addTiles(lf,
      layerId = 'base',
      attribution = '<a href=\"https://www.mapbox.com/about/maps/\" target=\"_blank\">&copy; Mapbox</a> <a href=\"https://openstreetmap.org/about/\" target=\"_blank\">&copy; OpenStreetMap</a> <a class=\"mapbox-improve-map\" href=\"https://www.mapbox.com/map-feedback/\" target=\"_blank\">Improve this map</a>',
      urlTemplate = sprintf('https://api.mapbox.com/styles/v1/s-balandine/ckhuyic56058j19miysj7yldz/tiles/256/{z}/{x}/{y}@2x?access_token=%s', api.key.mapbox),
      options = leaflet::tileOptions(maxNativeZoom = 18L, maxZoom = 22L, zIndex = 200L)
    ),
    light.mapbox = leaflet::addTiles(lf,
      layerId = 'base',
      attribution = '<a href=\"https://www.mapbox.com/about/maps/\" target=\"_blank\">&copy; Mapbox</a> <a href=\"https://openstreetmap.org/about/\" target=\"_blank\">&copy; OpenStreetMap</a> <a class=\"mapbox-improve-map\" href=\"https://www.mapbox.com/map-feedback/\" target=\"_blank\">Improve this map</a>',
      urlTemplate = sprintf('https://api.mapbox.com/styles/v1/s-balandine/ckhuyiros055n19poe6mj52s1/tiles/256/{z}/{x}/{y}@2x?access_token=%s', api.key.mapbox),
      options = leaflet::tileOptions(maxNativeZoom = 18L, maxZoom = 22L, zIndex = 200L, subdomains = 'abcd')
    ),
    light.mapbox.blue = leaflet::addTiles(lf,
      layerId = 'base',
      attribution = '<a href=\"https://www.mapbox.com/about/maps/\" target=\"_blank\">&copy; Mapbox</a> <a href=\"https://openstreetmap.org/about/\" target=\"_blank\">&copy; OpenStreetMap</a> <a class=\"mapbox-improve-map\" href=\"https://www.mapbox.com/map-feedback/\" target=\"_blank\">Improve this map</a>',
      urlTemplate = sprintf('https://api.mapbox.com/styles/v1/s-balandine/ckhuyjqcw059p19o4wqqaw46z/tiles/256/{z}/{x}/{y}@2x?access_token=%s', api.key.mapbox),
      options = leaflet::tileOptions(maxNativeZoom = 18L, maxZoom = 22L, zIndex = 200L, subdomains = 'abcd')
    ),
    gray.osm = leaflet::addProviderTiles(lf, leaflet::providers$OpenStreetMap.BlackAndWhite,
      layerId = 'base',
      options = leaflet::tileOptions(maxNativeZoom = 19L, maxZoom = 22L, zIndex = 200L)
    ),
    gray.mapbox = leaflet::addTiles(lf,
      layerId = 'base',
      attribution = '<a href=\"https://www.mapbox.com/about/maps/\" target=\"_blank\">&copy; Mapbox</a> <a href=\"https://openstreetmap.org/about/\" target=\"_blank\">&copy; OpenStreetMap</a> <a class=\"mapbox-improve-map\" href=\"https://www.mapbox.com/map-feedback/\" target=\"_blank\">Improve this map</a>',
      urlTemplate = sprintf('https://api.mapbox.com/styles/v1/s-balandine/ckhuxxyag04pd19tem7udhdxe/tiles/256/{z}/{x}/{y}@2x?access_token=%s', api.key.mapbox),
      options = leaflet::tileOptions(maxNativeZoom = 18L, maxZoom = 22L, zIndex = , subdomains = 'abcd')
    ),
  )
}

setMaxBounds <- function(map, sf) {
  bbox <- sf::st_bbox(sf) |> as.vector()
  leaflet::setMaxBounds(
    map,
    bbox[1L],
    bbox[2L],
    bbox[3L],
    bbox[4L]
  )
}

# styler: block Leaflet Custom Control

# Source https://github.com/yigityuce/Leaflet.Control.Custom

#' Add a custom control to a Leaflet map.
#'
#' This function adds a custom control to a Leaflet map allowing users to customize the control with specified parameters.
#'
#' @param map A Leaflet map object.
#' @param position A character string specifying the position of the control on the map (default is 'bottomright').
#' @param id A character string providing an identifier for the custom control (default is 'hollow').
#' @param title A character string representing the title of the custom control.
#' @param classes A character string containing additional CSS classes to be applied to the custom control.
#' @param content A character string defining the content of the custom control.
#'
#' @return Updated Leaflet map with the custom control added.
#'
#' @examples
#' \dontrun{
#' map <- leaflet() |>
#'   addTiles() |>
#'   setView(lng = 0L, lat = 0L, zoom = 2L)
#' addCustomControl(map, position = 'topright', id = 'custom', title = 'Custom Control', content = 'This is a custom control.')
#' }
#'
addCustomControl <- function(
    map,
    position = 'bottomright',
    id = 'hollow',
    title = '',
    classes = '',
    content = '') {
  map$dependencies <- c(map$dependencies, customControlDependency())

  leaflet::invokeMethod(
    map, leaflet::getMapData(map), 'addCustomControl',
    leaflet::filterNULL(list(
      position = position,
      id = id,
      title = title,
      classes = classes,
      content = content
    ))
  )
}

customControlDependency <- function() {
  list(
    htmltools::htmlDependency(
      name = 'customControl',
      version = '1.0.1',
      src = getDirAssets('js', 'leaflet', 'custom-control'),
      script = c(
        'Leaflet.Control.Custom.js',
        'Leaflet.Control.Custom-Bindings.js'
      )
    )
  )
}

# styler: block Leaflet Draw Toolbar

addDrawToolbar <- function(map) {
  map <- leafpm::addPmToolbar(map,
    toolbarOptions = leafpm::pmToolbarOptions(
      drawMarker = FALSE,
      drawPolygon = TRUE,
      drawPolyline = FALSE,
      drawCircle = FALSE,
      drawRectangle = FALSE,
      editMode = TRUE,
      cutPolygon = TRUE,
      removalMode = TRUE,
      position = 'topleft'
    ),
    drawOptions = leafpm::pmDrawOptions(
      snappable = TRUE,
      snapDistance = 20L,
      snapMiddle = TRUE,
      tooltips = TRUE,
      cursorMarker = TRUE,
      finishOn = NULL,
      allowSelfIntersection = FALSE,
      templineStyle = list(),
      hintlineStyle = list(color = '#3388ff', dashArray = '1,1'),
      markerStyle = list(draggable = TRUE)
    ),
    editOptions = leafpm::pmEditOptions(
      snappable = TRUE,
      snapDistance = 20L,
      allowSelfIntersection = FALSE,
      draggable = TRUE,
      preventMarkerRemoval = FALSE,
      preventVertexEdit = FALSE
    ),
    cutOptions = leafpm::pmCutOptions(
      snappable = TRUE,
      allowSelfIntersection = FALSE
    )
  )
}

#' Adds Google Tiles Layer
#'
#' @param map The Map widget
#' @param type String. Type of Tiles to display
#' @param layerId String. An optional unique ID for the layer
#' @param group String. An optional group name for the layer
#' @param ... Optional Parameters required by the Google API described at \url{https://msdn.microsoft.com/en-us/library/ff701716.aspx}
#'
#' @seealso Get a Google Maps API Key: \url{https://msdn.microsoft.com/en-us/library/ff428642.aspx}
#'
#' @examples
#' \dontrun{
#' library(leaflet)
#' map <- leaflet() |>
#'   addTiles() |>
#'   setView(lng = -71.06, lat = 42.36, zoom = 12L)
#' addGoogleTiles(map)
#' }
#'
addGoogleTiles <- function(
    map,
    type = c('roadmap', 'satellite', 'terrain'),
    layerId = NULL,
    group = NULL,
    ...) {
  type <- match.arg(type)

  map$dependencies <- c(map$dependencies, googleLayerDependencies())
  leaflet::invokeMethod(map, leaflet::getMapData(map), 'addGoogleTiles', layerId, group, list(type = type, ...))
}

googleLayerDependencies <- function() {
  list(
    htmltools::htmlDependency(
      name = 'Leaflet.GridLayer.GoogleMutant',
      version = '0.6.4',
      src = getDirAssets('js', 'leaflet', 'leaflet.googlemutant'),
      script = c(
        'leaflet.googlemutant.js',
        'leaflet.googlemutant-bindings.js'
      )
    )
  )
}

#' User functions :
#'
#' Make the package more user friendly (you need to use this function into the ui part of your shinny Application)
#' @param apikey the api-key you want to use for your application (you need to use a valid key)
#' @return a tags$head that will be usefull to load the references js/css
#'
useGoogle <- function(
    apikey = Sys.getenv('MAPS_API_KEY_GOOGLE')) {
  if (apikey == '') {
    stop('Google Map API need a key.')
  }

  tags$head(
    tags$script(src = sprintf('https://maps.googleapis.com/maps/api/js?key=%s', as.character(apikey)))
  )
}

# styler: block Leaflet Easy Toolbar (correctif)

easyDependencies <- function() {
  list(
    htmltools::htmlDependency(
      name = 'Leaflet.Easy',
      version = '1.0',
      src = getDirAssets('js', 'leaflet', 'easy'),
      script = 'easy.js'
    )
  )
}

addEasyToolbar <- function(map) {
  map$dependencies <- c(map$dependencies, easyDependencies())
  map
}
# styler: block Leaflet PaintPolygon

#' Add a PaintPolygon Control
#'
#' Add a PaintPolygon control to a leaflet map widget.
#'
#' @param map The leaflet map widget.
#' @param position The position of the control (default is 'bottomright').
#' @param radius The radius of the brush (default is 30).
#' @param minRadius The minimum radius of the brush (default is 10).
#' @param maxRadius The maximum radius of the brush (default is 50).
#' @param layerOptions Additional options for the painted layer (default is an empty list).
#' @param drawOptions Options for drawing with the brush (default is a list with weight = 1).
#' @param eraseOptions Options for erasing with the brush (default is a list with color = '#ff324a' and weight = 1).
#' @param menu Logical, whether to display the menu options (default is TRUE).
#'
addPaintPolygonControl <- function(
    map,
    position = 'bottomright',
    radius = 30L,
    minRadius = 10L,
    maxRadius = 50L,
    layerOptions = list(),
    drawOptions = list(weight = 1L),
    eraseOptions = list(color = '#ff324a', weight = 1L),
    menu = TRUE) {
  map$dependencies <- c(map$dependencies, paintPolygonDependencies())

  if (menu) {
    menu <- list(
      drawErase = TRUE,
      size = TRUE,
      eraseAll = TRUE
    )
  }

  leaflet::invokeMethod(
    map, leaflet::getMapData(map), 'addPaintPolygonControl',
    leaflet::filterNULL(
      list(
        position = position,
        radius = radius,
        minRadius = minRadius,
        maxRadius = maxRadius,
        layerOptions = layerOptions,
        drawOptions = drawOptions,
        eraseOptions = eraseOptions,
        menu = menu
      )
    )
  )
}

eraseAllPolygon <- function(map) {
  leaflet::invokeMethod(map, leaflet::getMapData(map), 'eraseAll')
}

paintPolygonDependencies <- function() {
  list(
    htmltools::htmlDependency(
      name = 'Leaflet.PaintPolygon',
      version = '1.2.1',
      src = getDirAssets('js', 'leaflet', 'leaflet.paintpolygon'),
      script = c(
        'Leaflet.PaintPolygon.js',
        'Leaflet.PaintPolygon-Bindings.js'
      )
    )
  )
}

setDataPolygon <- function(map, data) {
  leaflet::invokeMethod(map, leaflet::getMapData(map), 'setData', data)
}

setRadiusPolygon <- function(map, radius) {
  leaflet::invokeMethod(map, leaflet::getMapData(map), 'setRadius', radius)
}

startDrawPolygon <- function(map, color) {
  leaflet::invokeMethod(map, leaflet::getMapData(map), 'startDraw', list(color = color))
}

startErasePolygon <- function(map) {
  leaflet::invokeMethod(map, leaflet::getMapData(map), 'startErase')
}

stopDrawPolygon <- function(map) {
  leaflet::invokeMethod(map, leaflet::getMapData(map), 'stopDraw')
}

# styler: block Leaflet Sidebar

#' Add a Sidebar Leaflet Control
#'
#' The sidebar plugin only works in a reactive environment (e.g Shiny), as the
#' HTML must be created by using \code{\link{sidebar_tabs}} and
#' \code{\link{sidebar_pane}} and it must be created before
#' \code{\link[leaflet]{leafletOutput}}.
#'
#' @param map A leaflet map widget
#' @param id Id of the sidebar-div. Must match with the \code{id} of
#'   \code{\link{sidebar_tabs}}
#' @param options A named list with \code{position} and \code{fit} elements.
#' @family Sidebar Functions
#' @references \url{https://github.com/Turbo87/sidebar-v2}
#' @inherit leaflet::addControl return
#' @examples \dontrun{
#' library(shiny)
#' runApp(paste0(
#'   system.file('examples', package = 'leaflet.extras2'),
#'   '/sidebar_app.R'
#' ))
#' }
addSidebar <- function(map, options = list(
                         container = 'sidebar',
                         position = 'left',
                         fit = TRUE
                       )) {
  map$dependencies <- c(map$dependencies, sidebar_deps())
  leaflet::invokeMethod(map, NULL, 'addSidebar', options)
}

#' Close the Sidebar
#'
#'
#' @param map A leaflet map widget
#'
#' @family Sidebar Functions
#'
#' @inherit leaflet::addControl return
#'
closeSidebar <- function(map) {
  leaflet::invokeMethod(map, NULL, 'closeSidebar')
}

#' Open the Sidebar by ID
#' @param map A leaflet map widget
#' @param id The id of the \code{\link{sidebar_pane}} to open
#' @family Sidebar Functions
#' @inherit leaflet::addControl return
openSidebar <- function(map, id) {
  leaflet::invokeMethod(map, NULL, 'openSidebar', id)
}

#' Remove the Sidebar
#' @param map A leaflet map widget
#' @family Sidebar Functions
#' @inherit leaflet::addControl return
removeSidebar <- function(map) {
  leaflet::invokeMethod(map, NULL, 'removeSidebar')
}

sidebar_deps <- function(mini = FALSE) {
  list(
    htmltools::htmlDependency(
      name = 'lfx-sidebar',
      version = '1.0.0',
      src = getDirAssets('js', 'leaflet', 'sidebar'),
      script = c(
        'leaflet-sidebar.js',
        'leaflet-sidebar-binding.js'
      ),
      stylesheet = 'leaflet-sidebar.css'
    )
  )
}

#' Create a Sidebar Pane
#' @param title A title for the sidebar panel
#' @param id An id for the sidebar panel
#' @param icon An icon for the sidebar panel
#' @param ... List of elements to include in the panel
#' @family Sidebar Functions
#' @references \url{https://github.com/Turbo87/sidebar-v2},
#'          \url{https://github.com/Turbo87/sidebar-v2/blob/master/doc/usage.md}
#' @return A \code{shiny.tag} with sidebar-specific HTML classes
#' @examples \dontrun{
#' library(shiny)
#' sidebar_pane(id = 'id', icon = icon('cars'), tags$div())
#' }
sidebar_pane <- function(title = 'Sidebar Title',
                         id = NULL,
                         icon = icon('caret-right'), ...) {
  if (is.null(id)) {
    stop('The sidebar pane needs an id.')
  }
  tags$div(
    class = 'sidebar-pane', id = id,
    tags$h3(
      class = 'sidebar-header', title,
      tags$span(class = 'sidebar-close', icon)
    ),
    ...
  )
}

#' Create a Sidebar
#' @param id The id of the sidebar, which must match the \code{id} of
#'   \code{\link{addSidebar}}. Default is \code{"sidebar"}
#' @param iconList A list of icons to be shown, when the sidebar is collapsed.
#'   The list is required and must match the amount of \code{\link{sidebar_pane}}.
#' @param ... The individual \code{\link{sidebar_pane}}.
#' @family Sidebar Functions
#' @references \url{https://github.com/Turbo87/sidebar-v2},
#'          \url{https://github.com/Turbo87/sidebar-v2/blob/master/doc/usage.md}
#' @return A \code{shiny.tag} with individual sidebar panes
#' @examples \dontrun{
#' library(shiny)
#' runApp(paste0(
#'   system.file('examples', package = 'leaflet.extras2'),
#'   '/sidebar_app.R'
#' ))
#' }
sidebar_tabs <- function(id = 'sidebar', iconList = NULL, ...) {
  arg <- list(...)
  ids <- lapply(arg, function(x) tagGetAttribute(x, 'id'))
  if (length(ids) != length(iconList)) {
    stop('The number of icons needs to match the number of sidebar panes.')
  }
  tags$div(
    id = id, class = 'sidebar collapsed',
    tags$div(
      class = 'sidebar-tabs',
      tags$ul(
        role = 'tablist',
        tagList(lapply(seq_along(ids), function(x) {
          tags$li(tags$a(href = paste0('#', ids[[x]]), role = 'tab', iconList[[x]]))
        }))
      )
    ),
    tags$div(
      class = 'sidebar-content',
      tagList(arg)
    )
  )
}

# styler: block Leaflet Style Fast

styleDependencies <- function() {
  list(
    htmltools::htmlDependency(
      name = 'Leaflet.Style',
      version = '1.0',
      src = getDirAssets('js', 'leaflet', 'style'),
      script = 'style.js'
    )
  )
}

addStyleFast <- function(map) {
  map$dependencies <- c(map$dependencies, styleDependencies())
  map
}

#' Efficiently style a group that has already been added to the map
#'
#' Call with a group and a vector of style lists of length N. The first N
#' features of the group will be restyled.
#'
#' @examples
#' \donttest{
#' renderLeaflet('map', {
#'   leaflet() |> addPolygons(data = zones, group = 'zones', color = 'red')
#' })
#' colour <- 'blue'
#' styles <- lapply(pal(values), function(colour) {
#'   list(fillColor = colour, color = colour)
#' })
#' leafletProxy('map') |>
#'   setStyle('zones', styles)
#' }
#'
setStyle <- function(map, group, styles, label = NULL, offset = 0L) {
  leaflet::invokeMethod(map, NULL, 'setStyle', group, styles, label, offset - 1L)
}

setStyleFast <- function(map, group, colors = NULL, weights = NULL, labels = NULL, popups = NULL, strokes = NULL, fills = NULL, fill_opacities = NULL, radiuses = NULL, visibilities = NULL) {
  leaflet::invokeMethod(map, NULL, 'setStyleFast', group, colors, weights, labels, popups, strokes, fills, fill_opacities, radiuses, visibilities)
}
