# Data operations and code/label lookup utilities

# styler: block Code & Label

#' Get choices for labelling
#'
#' This function is used to generate choices for labelling purposes. It takes in a data frame of values and outputs a list of choices with corresponding labels.
#'
#' @param df_values A data frame of values.
#' @param var_name The variable name for the labels. Default is 'label'.
#' @param var_code The variable code for the choices. If not provided, row names are used.
#' @param overall The overall label for all choices. If provided, it is added to the list of choices with a code of 'T'.
#'
#' @return A list of choices with corresponding labels.
#' @seealso \code{\link{label}} for labelling.
#' @examples
#' df <- data.frame(var1 = c('A', 'B', 'C'), var2 = c(1L, 2L, 3L))
#' choices <- getChoices(df, 'var1', 'var2', overall = 'Overall')
#' print(choices)
getChoices <- function(
  df_values,
  var_name = 'label',
  var_code = NULL,
  overall = NULL
) {
  if (is.null(var_code)) {
    choices <- row.names(df_values)
  } else {
    choices <- df_values[[var_code]]
  }
  names(choices) <- df_values[[var_name]]
  if (!is.null(overall)) {
    choices <- list(list(Total = 'T'), choices)
    names(choices) <- c('Total', overall)
  }
  choices
}

#' Get coded factors from codes and values.
#'
#' This function takes two input vectors, \code{codes} and \code{values}, and returns a factor variable based on the values and codes. The function can also take an additional argument \code{short} which, when set to \code{TRUE}, returns factors with short labels. By default, the function returns factors with full labels.
#'
#' @param codes A data frame or tibble containing code and label information.
#' @param values A vector of values to be converted into factors.
#' @param short Logical value indicating whether to use short labels (default is \code{FALSE}).
#' @return A factor variable created from the values and codes.
#' @keywords internal
#' @examples
#' codes <- data.frame(cde_code = c(1L, 2L, 3L), cde_label = c('Low', 'Medium', 'High'), cde_label_short = c('L', 'M', 'H'))
#' values <- c(1L, 3L)
#' getCodeFactors(codes, values)
#' getCodeFactors(codes, values, short = TRUE)
getCodeFactors <- function(codes, values, short = FALSE) {
  if (short) {
    values <- factor(
      values,
      levels = codes$cde_code,
      labels = codes$cde_label_short
    )
  } else {
    values <- factor(values, levels = codes$cde_code, labels = codes$cde_label)
  }
  values
}

getCodeKeys <- function(codes, values) {
  codes$cde_key[match(values, codes$cde_code)]
}

#' Get code labels
#'
#' This function returns the labels corresponding to a set of codes from a given data frame.
#' If a code does not have a corresponding label, it will be replaced with a dot.
#'
#' @param codes A data frame containing the code labels.
#' @param values A vector of codes for which labels are sought.
#' @param short If TRUE, the short labels will be returned instead of the full labels. Default is FALSE.
#'
#' @return A vector of labels corresponding to the given codes.
#'
#' @keywords internal
#'
#' @examples
#' codes <- data.frame(
#'   cde_code = c(1L, 2L, 3L),
#'   cde_label = c('Label 1', 'Label 2', 'Label 3'),
#'   cde_label_short = c('L1', 'L2', 'L3')
#' )
#' values <- c(1L, NA, 2L)
#' getCodeLabels(codes, values) # returns: 'Label 1'  '.'  'Label 2'
#' getCodeLabels(codes, values, short = TRUE) # returns: 'L1'  '.'  'L2'
getCodeLabels <- function(codes, values, short = FALSE) {
  labels <- values
  labels[is.na(labels)] <- '.'
  if (short) {
    labels <- codes$cde_label_short[match(values, codes$cde_code)]
  } else {
    labels <- codes$cde_label[match(values, codes$cde_code)]
  }
  labels
}

#' Get code path
#'
#' This function takes in two arguments, \code{codes} and \code{values}, and returns the code path for the given values.
#'
#' @param codes A data frame containing code information.
#' @param values A vector of values for which code paths are needed.
#'
#' @details This function replaces all \code{NA} values in \code{paths} with '.' and then retrieves the code path for each value in \code{values} using the matching \code{cde_code} in \code{codes}.
#'
#' @return A vector of code paths for each value in \code{values}.
#'
#' @keywords internal
#'
#' @examples
#' codes <- data.frame(cde_code = c(1L, 2L, 3L), cde_path = c('path1', 'path2', 'path3'))
#' values <- c(2L, 1L)
#' getCodePath(codes, values)
getCodePath <- function(codes, values) {
  paths <- values
  paths[is.na(paths)] <- '.'
  paths <- codes$cde_path[match(values, codes$cde_code)]
  paths
}

#' Get code values
#'
#' This function retrieves the code values for a given list of labels from a codes data frame.
#'
#' @param codes A data frame containing the codes and labels.
#' @param labels A vector of labels for which the code values should be retrieved.
#' @param short A logical value indicating whether the short labels should be used. Default is \code{FALSE}.
#' @return A vector of corresponding code values for the given labels.
#' @keywords internal
#'
#' @examples
#' codes <- data.frame(
#'   cde_code = c(1L, 2L, 3L), cde_label = c('Label 1', 'Label 2', 'Label 3'),
#'   cde_label_short = c('L1', 'L2', 'L3')
#' )
#' labels <- c('Label 1', 'Label 2', NA, 'Label 3')
#' getCodeValues(codes, labels) # returns: 1, 2, NA, 3
#' getCodeValues(codes, labels, short = TRUE) # returns: L1, L2, NA, L3
getCodeValues <- function(codes, labels, short = FALSE) {
  values <- labels
  values[is.na(values)] <- '.'
  if (short) {
    values <- codes$cde_code[match(values, codes$cde_label_short)]
  } else {
    values <- codes$cde_code[match(values, codes$cde_label)]
  }
  values
}

#' @title Get Grouped Choices
#'
#' @description This function takes in a dataframe containing values, names, and groups,
#' and returns choices grouped by their respective groups. It also allows for an overall
#' group to be specified and included in the output.
#'
#' @param df_values A dataframe containing values, names, and groups.
#' @param var_name The name of the column containing choice names.
#' @param var_code The name of the column containing choice values. If NULL, row names are used.
#' @param var_group The name of the column containing choice groups.
#' @param overall The name of the overall group. If specified, an overall group will be included in the output.
#'
#' @return A named list of choices, grouped by their respective groups.
#'
#' @examples
#' df <- data.frame(
#'   var_code = c(1L, 2L, 3L, 4L),
#'   var_name = c('Choice A', 'Choice B', 'Choice C', 'Choice D'),
#'   var_group = c('Group 1', 'Group 1', 'Group 2', 'Group 2')
#' )
#' getGroupedChoices(df)
#'
#' @keywords internal
#'
getGroupedChoices <- function(
  df_values,
  var_name = 'label',
  var_code = NULL,
  var_group = NULL,
  overall = NULL
) {
  if (is.null(var_code)) {
    choice_values <- row.names(df_values)
  } else {
    choice_values <- df_values$var_code
  }
  choice_names <- df_values$var_name
  choice_groups <- df_values$var_group
  choice_groups_u <- unique(choice_groups)
  choices <- lapply(
    choice_groups_u,
    function(choice_group) {
      group_choice_values <- choice_values[choice_groups == choice_group]
      group_choice_names <- choice_names[choice_groups == choice_group]
      group_choices <- group_choice_values
      names(group_choices) <- group_choice_names
      group_choices
    }
  )
  names(choices) <- choice_groups_u
  if (!is.null(overall)) {
    choices <- c(Overall = NA, choices)
    choices[[1L]] <- c(Total = 'T')
    names(choices[[1L]]) <- overall
  }
  choices
}

#'
#' Get Labels
#'
#' This function extracts labels from a data frame based on a code and variable name.
#'
#' @keywords internal
#'
#' @param df_values A data frame containing the values and labels.
#' @param code The code used to identify the label.
#' @param var_name The name of the variable which contains the labels. Default is 'label'.
#'
#' @return A vector of labels corresponding to the given code.
#'
#' @examples
#' df <- data.frame(code = c('A', 'B', 'C'), label = c('Label A', 'Label B', 'Label C'))
#' getLabels(df, 'B')
#'
getLabels <- function(df_values, code, var_name = 'label') {
  lbls <- df_values[code, var_name]
  lbls
}

#' Function: matchValue
#'
#' Description: Function that matches values in a data frame based on given criteria.
#'
#' @param tbl Data frame containing the data.
#' @param x Vector of values to match in the data frame.
#' @param var_x Name of the variable in the data frame to match against x.
#' @param var_y Name of the variable in the data frame where the matched values are retrieved from.
#'
#' @return A vector of values from var_y that match the values in x with respect to var_x.
#'
#' @examples
#' tbl <- data.frame(a = 1:10, b = rev(1:10))
#' matchValue(tbl, c(5L, 1L), 'a', 'b')
#'
matchValue <- function(tbl, x, var_x, var_y) {
  x_vec <- tbl[[var_x]]
  y_vec <- tbl[[var_y]]
  y_vec_idx <- unlist(lapply(x, function(xv) which(x_vec == xv)))
  y_vec[y_vec_idx]
}

compareDataframe <- function(base, compare, keys = NULL, preview = TRUE) {
  base <- base |>
    dplyr::select(names(base)[names(base) %in% names(compare)]) |>
    dplyr::mutate_if(
      .predicate = ~ methods::is(.x, 'glue'),
      .funs = as.character
    )

  compare <- compare |>
    dplyr::select(names(base)) |>
    dplyr::mutate_if(
      .predicate = ~ methods::is(.x, 'glue'),
      .funs = as.character
    )

  rlang::check_installed('diffdf', reason = 'to compare data frames')
  diffs <- diffdf::diffdf(
    base = base,
    compare = compare,
    keys = keys,
    suppress_warnings = TRUE
  )

  if (preview && length(diffs) > 0L) {
    base_path <- fs::file_temp(ext = 'csv')
    compare_path <- fs::file_temp(ext = 'csv')

    readr::write_csv(base, file = base_path, na = '')
    readr::write_csv(compare, file = compare_path, na = '')

    system2(
      command = sysWhich('bcompare'),
      args = glue::glue('"{base_path}" "{compare_path}"'),
      wait = FALSE
    )
  }

  diffs
}

#' Get Filters
#'
#' A function to retrieve the filters used for data filtering.
#'
#' @return A list of filters used for data filtering.
#' @keywords internal
#'
getFilters <- function() {
  mt_filters
}
