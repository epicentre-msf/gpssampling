# styler: block

#' @title Add a break to a rdocx element
#' @description Add a bread to a rdocx element
#' @param doc a rdocx element
#' @param landscape_continuous a boolean. Must be TRUE in case of landscape disposition to be continued after the break.
#' Default to FALSE
#' @return a rdocx element with a break at the end
#' @examples
#' doc <- read_docx()
#' doc <- addTextElement(doc, 'Titre 1 : description of the dataset', style = 'heading 1')
#' doc <- addBreak(doc)
#' doc <- addTextElement(doc, 'Titre 2 : comparaison of the dataset', style = 'heading 1')
#' doc <- setLandscape(doc, add_break = TRUE)
#' doc <- addBreak(doc, landscape_continuous = TRUE)
#' doc <- addTextElement(doc, 'Titre 3 : plot', style = 'heading 1')
#' doc <- setLandscape(doc, add_break = FALSE)
#' print(doc, target = 'output.docx')
#'
addBreak <- function(doc, landscape_continuous = FALSE) {
  if (methods::is(doc, 'rdocx')) {
    if (landscape_continuous) {
      doc |>
        officer::body_add_break()
    } else {
      doc |>
        officer::body_add_break() |>
        officer::body_end_section_continuous()
    }
  }
  doc
}

addFlextable <- function(doc, tbl, width = 100L) {
  # doc_dims    <- officer::docx_dim(doc)
  # doc.width   <-
  #   doc_dims$page['width']  -
  #   doc_dims$margins['left'] -
  #   doc_dims$margins['right']
  # ncol <- length(dim(tbl)$widths)
  # tbl <- flextable::width(tbl, j = 1:ncol, width = doc.width/ncol)
  doc <- flextable::body_add_flextable(doc, value = tbl, align = 'left')
  doc <- officer::body_add_par(doc, value = ' ', style = 'Normal')
}

addHyperRef <- function(
  x,
  target = 'http://www.google.de',
  style = NULL,
  pos = 'after'
) {
  if (is.null(style)) {
    style <- x$default_styles$table
  }
  style_id <- x$doc_obj$get_style_id(style = style, type = 'character')

  refID <- sprintf('rId%d', x$doc_obj$relationship()$get_next_id())

  x$doc_obj$relationship()$add(
    refID,
    type = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink',
    target = target,
    target_mode = 'External'
  )

  xml_elt <- sprintf(
    "<w:hyperlink r:id='%s' w:history='1'><w:r w:rsidRPr='00CD112F'><w:rPr><w:rStyle w:val='Hyperlink'/></w:rPr><w:t>LINK</w:t></w:r></w:hyperlink>",
    refID
  )
  xml_elt <- paste0(
    '<w:p>',
    '<w:pPr><w:pStyle w:val="',
    style_id,
    '"/></w:pPr>',
    xml_elt,
    '</w:p>'
  )

  officer::body_add_xml(x = x, str = xml_elt, pos = pos)
}

addImage <- function(
  doc,
  image,
  width = 100L,
  height = 100L,
  add_para = FALSE,
  landscape = FALSE
) {
  doc_dims <- getDocDims(doc)
  if (landscape) {
    plot_width <- width / 100L * doc_dims$height
    plot_height <- height / 100L * doc_dims$width
  } else {
    plot_width <- width / 100L * doc_dims$width
    plot_height <- height / 100L * doc_dims$height
  }
  doc <- doc |>
    officer::body_add_img(
      src = image,
      width = plot_width,
      height = plot_height
    ) |>
    officer::body_add_par(value = '', style = 'Normal')
  doc
}

addMap <- function(
  doc,
  plot,
  width = 100L,
  height = 100L,
  add_para = FALSE,
  landscape = FALSE
) {
  doc_dims <- getDocDims(doc)
  if (landscape) {
    plot_width <- width / 100L * doc_dims$height
    plot_height <- height / 100L * doc_dims$width
  } else {
    plot_width <- width / 100L * doc_dims$width
    plot_height <- height / 100L * doc_dims$height
  }
  # plot <- plot + theme_map(width = plot_width, height = plot_height)
  doc <- addPlotDefault(
    doc,
    plot = plot,
    width = plot_width,
    height = plot_height,
    add_para = add_para
  )
  doc
}

#' @title Add new line to rdocx element
#' @description Add a new line to rdocx element
#' @param doc a rdocx element
#' @return a rdocx element with a new line at the end
addNewLine <- function(doc) {
  if (methods::is(doc, 'rdocx')) {
    doc |>
      officer::body_add_par(value = '', style = 'Normal')
  }
  doc
}

addParagraphe <- function(doc, text, style = 'Paragraphe') {
  if (methods::is(doc, 'rdocx')) {
    doc <- officer::body_add_par(doc, text, style = style)
  } else {
    doc <- paste0(doc, sprintf('%s %s\n', strrep('#', level), text))
  }
  doc
}

addPlot <- function(
  doc,
  plot,
  width = 100L,
  height = 100L,
  add_para = FALSE,
  landscape = FALSE
) {
  doc_dims <- getDocDims(doc)
  if (landscape) {
    plot_width <- width / 100L * doc_dims$height
    plot_height <- height / 100L * doc_dims$width
  } else {
    plot_width <- width / 100L * doc_dims$width
    plot_height <- height / 100L * doc_dims$height
  }
  # plot <- plot + theme_plot(width = plot_width, height = plot_height)
  doc <- addPlotDefault(
    doc,
    plot = plot,
    width = plot_width,
    height = plot_height,
    add_para = add_para
  )
  doc
}

addPlotDefault <- function(
  doc,
  plot,
  width,
  height,
  title = NULL,
  add_para = FALSE
) {
  if (methods::is(doc, 'rdocx')) {
    if (!is.null(title)) {
      doc |>
        officer::body_add_par(value = title, style = 'graphic title') |>
        officer::slip_in_text(
          ' : ',
          style = 'Default Paragraph Font',
          pos = 'before'
        ) |>
        officer::slip_in_seqfield(
          str = 'SEQ graph \\* Arabic \\s 1 \\* MERGEFORMAT',
          style = 'Default Paragraph Font',
          pos = 'before'
        ) |>
        officer::slip_in_text(
          '.',
          style = 'Default Paragraph Font',
          pos = 'before'
        ) |>
        officer::slip_in_seqfield(
          str = sprintf('STYLEREF %.0f \\s', 1L),
          style = 'Default Paragraph Font',
          pos = 'before'
        ) |>
        officer::slip_in_text(
          'Figure ',
          style = 'Default Paragraph Font',
          pos = 'before'
        )
    }

    if (add_para) {
      doc <- body_add_gg(doc, plot = plot, width = width, height = height)
    } else {
      doc <- body_add_gg(doc, plot = plot, width = width, height = height)
    }
  } else {
    doc <- paste0(doc, '```{r, layout="l-body"}\n')
    doc <- paste0(doc, 'tb1\n')
    doc <- paste0(doc, '```\n')
  }
  doc
}

addTable <- function(doc, table) {
  if (methods::is(doc, 'rdocx')) {
    doc <- addFlextable(doc, table)
  } else {
    doc <- paste0(doc, '```{r, layout="l-body"}\n')
    doc <- paste0(doc, 'tb1\n')
    doc <- paste0(doc, '```\n')
  }
  doc
}

addText <- function(doc, text) {
  if (methods::is(doc, 'rdocx')) {
    doc <- officer::slip_in_text(doc, text)
  }
  doc
}

addTitle <- function(doc, title, level = 1L) {
  if (methods::is(doc, 'rdocx')) {
    doc <- officer::body_add_par(
      doc,
      title,
      style = sprintf('heading %d', level)
    )
  } else {
    doc <- paste0(doc, sprintf('%s %s\n', strrep('#', level), title))
  }
  doc
}

body_add_gg <- function(
  doc,
  plot,
  width = 6L,
  height = 5L,
  style = NULL,
  res = 600L,
  emf = TRUE
) {
  stopifnot(inherits(plot, 'gg'))
  if (emf && requireNamespace("devEMF", quietly = TRUE)) {
    file <- tempfile(fileext = '.emf')
    devEMF::emf(file = file, width = width, height = height, coordDPI = res)
  } else {
    file <- fs::file_temp(ext = 'png')
    grDevices::png(
      filename = file,
      width = width,
      height = height,
      units = 'in',
      res = res,
      type = 'cairo'
    )
  }
  print(plot)
  grDevices::dev.off()
  doc |>
    officer::body_add_img(
      src = file,
      style = style,
      width = width,
      height = height
    ) |>
    officer::body_add_par(value = '', style = 'Normal')
  fs::file_delete(file)
  doc
}

createDocument <- function(
  template = 'report (template)',
  type = 'doc',
  title = NULL,
  subtitle = NULL,
  authors = NULL
) {
  if (type == 'doc') {
    doc <- officer::read_docx(
      path = getPackagePath(sprintf('res/%s.docx', template))
    )
    doc <- officer::cursor_begin(doc)
    if (!is.null(title)) {
      doc <- officer::body_add_par(doc, value = title, style = 'Title')
    }
    if (!is.null(subtitle)) {
      doc <- officer::body_add_par(doc, value = subtitle, style = 'Subtitle')
    }
    if (!is.null(authors)) {
      doc <- officer::body_add_par(
        doc,
        value = sprintf('%s. Edited. %s', authors, Sys.Date())
      )
    }
  } else {
    doc <- '---\n'
    doc <- paste0(doc, sprintf('title: "%s"\n', title))
    doc <- paste0(doc, sprintf('description: "%s"\n', subtitle))
    doc <- paste0(doc, 'author:\n')
    doc <- paste0(doc, '  - name: Serge Balandine\n')
    doc <- paste0(doc, '    affiliation: MSF/Epicentre\n')
    doc <- paste0(doc, '    affiliation_url: https://epicentre.msf.org\n')
    doc <- paste0(doc, 'date: "`r Sys.Date()`"\n')
    doc <- paste0(doc, 'output: distill::distill_article\n')
    doc <- paste0(doc, '---\n')
    doc <- paste0(doc, '```{r setup, include=FALSE}\n')
    doc <- paste0(doc, 'knitr::opts_chunk$set(echo = FALSE)\n')
    doc <- paste0(doc, '```\n')
  }
  doc
}

generateReport <- function(doc, filename = NULL, open = TRUE) {
  if (methods::is(doc, 'rdocx')) {
    f_out <- fs::file_temp(pattern = 'report', ext = 'docx')

    invisible(print(doc, target = f_out))

    if (!is.null(filename)) {
      fs::file_copy(path = f_out, new_path = filename, overwrite = TRUE)
    } else {
      filename <- f_out
    }

    if (open) {
      shell.exec(filename)
    }
  } else {
    f_in <- fs::file_temp(
      pattern = 'report',
      tmpdir = getDirAppTemp(),
      ext = 'Rmd'
    )

    f_con <- file(f_in, 'w', encoding = 'UTF-8')
    cat(doc, file = f_con)
    close(f_con)

    # Process the Arguments
    args <- list()
    args$input <- f_in
    args$output_dir <- getDirAppTemp()

    # Run the render
    f_out <- do.call('render', args = args)

    if (open) {
      fs::file_show(path = tools::file_path_as_absolute(f_out))
    }
  }

  invisible(f_out)
}

getDocDims <- function(doc) {
  doc_dims <- officer::docx_dim(doc)
  doc_width <-
    doc_dims$page['width'] -
    doc_dims$margins['left'] -
    doc_dims$margins['right']
  doc_height <-
    doc_dims$page['height'] -
    doc_dims$margins['top'] -
    doc_dims$margins['bottom'] -
    doc_dims$margins['header'] -
    doc_dims$margins['footer']
  list(
    width = doc_width,
    height = doc_height
  )
}

reportMap <- function(gg, width = 100L, height = 100L) {
  doc <- createDocument(type = 'doc')
  doc <- addMap(doc, plot = gg, width = width, height = height, add_para = TRUE)
  doc_file <- generateReport(doc)
  doc_file
}

reportPlot <- function(gg, width = 100L, height = 100L) {
  doc <- createDocument(type = 'doc')
  doc <- addPlot(
    doc,
    plot = gg,
    width = width,
    height = height,
    add_para = TRUE
  )
  doc_file <- generateReport(doc)
  doc_file
}

reportTable <- function(tbl) {
  doc <- createDocument(type = 'doc')
  doc <- addTable(doc, table = tbl)
  doc_file <- generateReport(doc)
  doc_file
}

saveDocument <- function(doc, target = NULL, open = TRUE) {
  if (is.null(target)) {
    target <- fs::file_temp(pattern = 'report', ext = 'docx')
  }

  invisible(print(doc, target = target))

  if (open) {
    shell.exec(target)
  }

  target
}

slip_in_gg <- function(
  doc,
  plot,
  width = 6L,
  height = 5L,
  style = NULL,
  res = 600L,
  emf = TRUE
) {
  stopifnot(inherits(plot, 'gg'))
  if (emf && requireNamespace("devEMF", quietly = TRUE)) {
    file <- tempfile(fileext = '.emf')
    devEMF::emf(file = file, width = width, height = height, coordDPI = res)
  } else {
    file <- fs::file_temp(ext = 'png')
    grDevices::png(
      filename = file,
      width = width,
      height = height,
      units = 'in',
      res = res,
      type = 'cairo'
    )
  }
  print(plot)
  grDevices::dev.off()
  doc |>
    officer::slip_in_img(
      src = file,
      style = style,
      width = width,
      height = height
    )
  if (file_exists(file)) {
    fs::file_delete(file)
  }
  doc
}
