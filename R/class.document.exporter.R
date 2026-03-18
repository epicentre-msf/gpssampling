#' @title DocumentExporter
#' @description R6 class for exporting sample data to Word documents.
#'
#' Extracts document export responsibilities from UserData.
#' Receives a reference to the UserData instance (`data`) for read-only
#' access to polygons and samples.
#'
#' @noRd
#'
DocumentExporter <- R6::R6Class(
  classname = 'DocumentExporter',
  portable = FALSE,
  public = list(
    #' @field data Reference to the parent UserData instance.
    data = NULL,

    #' @description Create a new DocumentExporter.
    #' @param data The UserData instance owning polygon/sample state.
    initialize = function(data) {
      self$data <- data
    },

    #' @description Generate an export filename based on polygon type.
    #' @param extension File extension (e.g., "docx", "gpx").
    #' @return Character string with the generated filename.
    sampleExportFilename = function(extension) {
      polygon <- data$polygons[1L, ]
      if (is.defined(polygon)) {
        file <- df_sampling_method[polygon$type, 'label']
      } else {
        file <- df_sampling_method['SP_SMP', 'label']
      }
      file <- sprintf(
        '%s %s - %s.%s',
        file,
        tolower(polygon$id),
        Sys.Date(),
        extension
      )
      file
    },

    #' @description Export samples to a Word document with overview map.
    #' @param file Output file path.
    #' @param open Whether to open the document after creation.
    #' @param polygon_idx Optional polygon index to export.
    sampleExportToDoc = function(file, open = TRUE, polygon_idx = NULL) {
      if (is.null(polygon_idx)) {
        sf_pol <- data$polygon_focused
        sf_pts <- data$samples
      } else {
        sf_pol <- data$polygons[polygon_idx, ]
        sf_pts <- sf_pol$samples_sf[[1L]]
      }

      if (nrow(sf_pts) == 0L) {
        return()
      }

      sf_pts_cc <- sf::st_coordinates(sf_pts)

      bb <- sf::st_bbox(sf_pts)

      # Calculate A4 bbox
      bbw <- abs(bb[['xmin']] - bb[['xmax']])
      bbh <- abs(bb[['ymin']] - bb[['ymax']])

      if (bbw > bbh) {
        # Landscape
        bbh_a4 <- (bbw / 297L) * 210L
        bb[['ymin']] <- bb[['ymin']] - (bbh_a4 - bbh) / 2L
        bb[['ymax']] <- bb[['ymax']] + (bbh_a4 - bbh) / 2L
      }

      grDevices::png(
        'my_plot.png',
        width = 297L,
        height = 210L,
        units = 'mm',
        res = 96L
      )

      plotMap(bb)
      plot(
        sf::st_geometry(sf_pol),
        border = scales::alpha('yellow', 0.5),
        lwd = 5L,
        add = TRUE
      )
      plot(sf::st_geometry(sf_pol), border = 'yellow', lwd = 2L, add = TRUE)
      plot(sf_pts, add = TRUE, pch = 16L, col = 'yellow')

      for (i in seq_len(nrow(sf_pts_cc))) {
        text(
          x = sf_pts_cc[i, 1L],
          y = sf_pts_cc[i, 2L],
          labels = i,
          pos = 3L,
          cex = 0.75,
          col = 'white'
        )
      }

      grDevices::dev.off()

      doc <- createDocument(template = 'samples')
      doc <- doc |>
        officer::body_replace_img_at_bkm(
          bookmark = 'MAP',
          value = officer::external_img(
            src = fs::path(getwd(), 'my_plot.jpg'),
            width = units::set_units(units::set_units(145L, mm), inches),
            height = units::set_units(units::set_units(145L, mm), inches)
          )
        ) |>
        generateReport(filename = 'doc.docx', open = TRUE)

      doc <- createDocument(template = 'samples')
      doc <- doc |>
        officer::body_replace_img_at_bkm(
          bookmark = 'MAP',
          value = officer::external_img(
            src = fs::path(getwd(), 'my_plot.jpg'),
            width = units::set_units(units::set_units(145L, mm), inches),
            height = units::set_units(units::set_units(145L, mm), inches)
          )
        ) |>
        generateReport(filename = 'doc.docx', open = TRUE)

      generateReport(doc, filename = fs::file_temp(), open = open)
    },

    #' @description Export individual sample pages with maps and QR codes.
    #' @param file Output file path.
    #' @param open Whether to open the document after creation.
    #' @param polygon_idx Optional polygon index to export.
    #' @param samples_idxs Optional indices of specific samples to export.
    sampleExportToDocEach = function(
      file,
      open = TRUE,
      polygon_idx = NULL,
      samples_idxs = NULL
    ) {
      if (is.null(polygon_idx)) {
        sf_pol <- data$polygon_focused
        sf_pts <- data$samples
      } else {
        sf_pol <- data$polygons[polygon_idx, ]
        sf_pts <- sf_pol$samples_sf[[1L]]
      }

      if (nrow(sf_pts) == 0L) {
        return()
      }

      if (!is.null(samples_idxs)) {
        sf_pts <- sf_pts[samples_idxs, ]
      }

      sf_pts_cc <- sf::st_coordinates(sf_pts)

      for (i in seq_len(nrow(sf_pts_cc))) {
        bb <- sf::st_bbox(sf_pts[i, ], crs = sf::st_crs(4326L))
        bb <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(bb), 3857L))

        # Calculate bbox around 200m
        bb_200 <- bb
        bb_200[['xmin']] <- bb_200[['xmin']] - 200L
        bb_200[['ymin']] <- bb_200[['ymin']] - 200L
        bb_200[['xmax']] <- bb_200[['xmax']] + 200L
        bb_200[['ymax']] <- bb_200[['ymax']] + 200L

        # Calculate bbox around 400m
        bb_400 <- bb
        bb_400[['xmin']] <- bb_400[['xmin']] - 400L
        bb_400[['ymin']] <- bb_400[['ymin']] - 400L
        bb_400[['xmax']] <- bb_400[['xmax']] + 400L
        bb_400[['ymax']] <- bb_400[['ymax']] + 400L

        bb_200 <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(bb_200), 4326L))
        bb_400 <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(bb_400), 4326L))

        file_map_200 <- sprintf('%s/pts_%s_200.png', fs::path_temp(), i)
        file_map_400 <- sprintf('%s/pts_%s_400.png', fs::path_temp(), i)
        file_map_400_road <- sprintf(
          '%s/pts_%s_400_road.png',
          fs::path_temp(),
          i
        )
        file_map_qr <- sprintf('%s/pts_%s_qr.png', fs::path_temp(), i)

        private$render_sample_map(
          file_map_200,
          bb_200,
          sf_pts,
          sf_pts_cc,
          i,
          width = 140L,
          height = 140L
        )
        private$render_sample_map(
          file_map_400,
          bb_400,
          sf_pts,
          sf_pts_cc,
          i,
          width = 70L,
          height = 70L
        )
        private$render_sample_map(
          file_map_400_road,
          bb_400,
          sf_pts,
          sf_pts_cc,
          i,
          width = 70L,
          height = 70L,
          road = TRUE
        )

        qr <- qrcode::qr_code(sprintf(
          'geo:%s,%s',
          sf_pts_cc[i, 1L],
          sf_pts_cc[i, 2L]
        ))
        grDevices::png(
          file_map_qr,
          width = 70L,
          height = 70L,
          units = 'mm',
          res = 96L
        )
        plot(qr)
        grDevices::dev.off()
      }

      tbl <- private$build_sample_table(
        sf_pol,
        sf_pts,
        sf_pts_cc,
        file_map_200,
        file_map_400,
        file_map_400_road,
        file_map_qr,
        i = nrow(sf_pts_cc)
      )

      doc <- createDocument(template = 'default.portrait')
      doc |>
        flextable::body_add_flextable(value = tbl, align = 'left') |>
        generateReport(filename = 'doc.docx', open = TRUE)
    },

    #' @description Stub for export functionality.
    export = function() {
    },

    #' @description Stub for import functionality.
    import = function() {
    }
  ),
  private = list(
    render_sample_map = function(
      filepath,
      bbox,
      sf_pts,
      sf_pts_cc,
      idx,
      width,
      height,
      road = FALSE
    ) {
      grDevices::png(
        filepath,
        width = width,
        height = height,
        units = 'mm',
        res = 96L
      )
      plotMap(bbox, road = road)
      plot(sf_pts[idx, ], add = TRUE, pch = 16L, col = 'white', cex = 2L)
      plot(sf_pts[idx, ], add = TRUE, pch = 16L, col = 'yellow')
      text(
        x = sf_pts_cc[idx, 1L],
        y = sf_pts_cc[idx, 2L],
        labels = idx,
        pos = 3L,
        cex = 0.75,
        col = 'white'
      )
      grDevices::dev.off()
    },
    build_sample_table = function(
      sf_pol,
      sf_pts,
      sf_pts_cc,
      file_map_200,
      file_map_400,
      file_map_400_road,
      file_map_qr,
      i
    ) {
      tbl <-
        flextable::flextable(data.frame(
          a = rep('', 3L),
          b = rep('', 3L),
          stringsAsFactors = FALSE
        )) |>
        flextable::width(width = 130L, j = 'a', unit = 'mm') |>
        flextable::width(width = 70L, j = 'b', unit = 'mm') |>
        flextable::height(height = 130L, i = 1L, unit = 'mm') |>
        flextable::delete_part(part = 'header') |>
        flextable::delete_part(part = 'footer') |>
        flextable::border_remove() |>
        flextable::valign(i = 1L, j = 1L, valign = 'top')

      if (sf_pol$type == 'SP_QDR') {
        tbl <- tbl |>
          flextable::compose(
            i = 1L,
            j = 1L,
            value = flextable::as_paragraph(
              flextable::as_chunk(
                sprintf(
                  '%s (%s)\n\n',
                  sf_pts[i, ]$id_user,
                  sf_pts[i, ]$id_key_calc
                ),
                props = flextable::fp_text_default(font.size = 18L)
              ),
              flextable::as_chunk(
                'Population:',
                props = flextable::fp_text_default(font.size = 12L)
              ),
              flextable::as_chunk('\t\t< 5 years: ..........'),
              flextable::as_chunk('\t\t>= 5 years: ..........')
            ),
            part = 'body'
          )
      }

      mm_to_inches <- function(mm_val) {
        units::set_units(units::set_units(mm_val, mm), inches)
      }

      tbl <- tbl |>
        flextable::compose(
          i = 2L,
          j = 1L,
          value = flextable::as_paragraph(
            flextable::as_image(
              src = file_map_200,
              width = mm_to_inches(125L),
              height = mm_to_inches(125L)
            )
          ),
          part = 'body'
        ) |>
        flextable::compose(
          i = 2L,
          j = 2L,
          value = flextable::as_paragraph(
            flextable::as_image(
              src = file_map_400,
              width = mm_to_inches(65L),
              height = mm_to_inches(65L)
            )
          ),
          part = 'body'
        ) |>
        flextable::compose(
          i = 2L,
          j = 3L,
          value = flextable::as_paragraph(
            flextable::as_image(
              src = file_map_400_road,
              width = mm_to_inches(65L),
              height = mm_to_inches(65L)
            )
          ),
          part = 'body'
        ) |>
        flextable::compose(
          i = 1L,
          j = 2L,
          value = flextable::as_paragraph(
            flextable::as_image(
              src = file_map_qr,
              width = mm_to_inches(65L),
              height = mm_to_inches(65L)
            )
          ),
          part = 'body'
        )

      tbl
    }
  )
)
