# styler: block

#' Functions usefull
#'
#' MyPackage has different families of functions described below
#'
#' @section Some functions:
#' * [getPackageDescription()]
#' * [getPackageField()]
#'
#' @section Other functions:
#' * [getPackageField()]
#' * [getPackageField()]
#'
#' @name utils
#'
NULL

#' Functions Package
#'
#' MyPackage has different families of functions described below
#'
#' @name utils_package
#'
NULL

#' Functions Package Dir
#'
#' @inheritDotParams fs::path
#'
#' @return A character string representing the full path to the user's roaming data directory.
#'
#' @name utils_dir
#'
NULL

# styler: block Alias

#' bash alias for \code{\link[base]{as.data.frame}}
#'
#' @inheritParams base::as.data.frame
#'
adf <- base::as.data.frame

#' bash alias for \code{\link[base]{setwd}}
#'
#' @inheritParams base::setwd
#'
cd <- base::setwd

#' bash alias for \code{\link[base]{getwd}}
#'
pwd <- base::getwd

#' bash alias for \code{\link[utils]{sessionInfo}}
#'
#' @inheritParams utils::sessionInfo
#'
info <- utils::sessionInfo

# styler: block Path

#' @describeIn utils_dir Get Directory Data
#'
#' This function returns the path to the user data directory for the package.
#'
#' @param ... Other parameters to be passed to [fs::path].
#'
#' @examples
#' getDirData()
#' getDirData('subdir')
#'
#' @keywords internal
#'
getDirApp <- function(...) {
  fs::path(getDirApps(), utils::packageName(), ...)
}

#' @describeIn utils_dir Get the directory path for all application directories.
#'
#' This function creates a directory with the name 'all' inside the main application directory
#' and returns the full path to this new directory.
#'
#' @param ... Other parameters to be passed to [fs::path].
#'
#' @examples
#' getDirApps()
#' getDirApps('app1')
#'
#' @keywords internal
#'
getDirAppAll <- function(...) {
  fs::path(getDirApps(), 'all', ...)
}

#' @describeIn utils_dir Get the directory path for all data files in the application.
#'
#' @description This function returns the directory path where all the data files of the application are stored.
#'
#' @param ... Other parameters to be passed to [fs::path].
#'
#' @examples
#' getDirAppAllData()
#'
#' @keywords internal
#'
getDirAppAllData <- function(...) {
  fs::path(getDirAppAll(), 'data', ...)
}

#' @describeIn utils_dir Get the directory path in the SharePoint application
#'
#' This function returns the directory path in the SharePoint application folder.
#'
#' @param ... Other parameters to be passed to [fs::path].
#'
#' @examples
#' getDirAppSharepoint()
#'
#' @keywords internal
#'
getDirAppSharepoint <- function(path = 'MSF', ...) {
  fs::path(getDirSharepoint(path), getPackageDescription()$Sharepoint, ...)
}

#' @describeIn utils_dir Get temporary directory under the app directory
#'
#' This function creates a temporary directory under the application directory and returns the path to that directory.
#'
#' @param ... Other parameters to be passed to [fs::path].
#'
#' @seealso \code{\link{getDirApp}}, \code{\link{createDir}}
#'
#' @examples
#' getDirAppTemp()
#' getDirAppTemp('subdir')
#'
#' @keywords internal
#'
getDirAppTemp <- function(...) {
  fs::path(getDirApp(), 'temp', ...)
}

#' @describeIn utils_dir Get the directory path for application users
#'
#' This function returns the absolute path of the directory for application users.
#' The function utilizes the \code{\link{createDir}} function to create the users directory if it doesn't exist.
#'
#' @param ... Other parameters to be passed to [fs::path].
#'
#' @seealso \code{\link{createDir}}, \code{\link{getDirApp}}
#'
#' @keywords internal
#'
getDirAppUsers <- function(...) {
  fs::path(getDirApp(), 'users', ...)
}

#' @describeIn utils_dir Get directory for storing applications
#'
#' This function creates a directory for storing applications within the organization directory.
#'
#' @param ... Other parameters to be passed to [fs::path].
#'
#' @examples
#' getDirApps() # creates a directory called 'apps' within the organization directory
#' getDirApps('app1') # creates a directory called 'app1' within the organization directory
#'
#' @keywords internal
#'
getDirApps <- function(...) {
  fs::path(getDirOrganization(), 'apps', ...)
}

#' @describeIn utils_dir Chemin des ressource du package (image, icon, fonts, css, js, ...)
#'
#' Cette fonction renvoie le chemin du r<U+FFFD>pertoire contenant les ressources du package,
#' telles que les images, les ic<U+FFFD>nes, les polices, les fichiers CSS, les fichiers JS, etc.
#'
#' @keywords internal
#'
getDirAssets <- function(...) {
  getPackagePath('assets', ...)
}

#' @describeIn utils_dir Get the directory path of the database
#'
#' This function retrieves the directory path where the database is located.
#'
#' @keywords internal
#'
getDirDatabase <- function() {
  path <- fs::path(getDirApp(), '.db')
  path
}

#' @describeIn utils_dir Get the path to the development directory.
#'
#' This function retrieves the path to the development directory, based on the operating system.
#' For Windows, it first obtains the user library directory using the `Sys.getenv` function,
#' and then applies the `fs::path_dir` function multiple times to navigate to the development directory.
#' For other operating systems, it also retrieves the user library directory using `Sys.getenv',
#' and applies the `fs::path_dir` function to navigate to the development directory.
#' Finally, it constructs the full path by appending any additional subdirectories specified in the arguments.
#'
#' @param ... Other parameters to be passed to [fs::path].
#'
#' @keywords internal
#'
getDirDevelopment <- function(...) {
  path <- fs::path_wd()
  if (!grepl('development', path, fixed = TRUE)) {
    stop('Can\'t find path devlopment')
  }
  while (fs::path_file(path) != 'development') {
    path <- fs::path_dir(path)
  }
  fs::path(path, ...)
}

#' @describeIn utils_dir Get Directory Organization
#'
#' This function creates a directory with the specified organization name and returns its path.
#'
#' @param org The name of the organization for which the directory is to be created. Default is 'epicentre'.
#' @param ... Other parameters to be passed to [fs::path].
#'
#' @examples
#' getDirOrganization()
#' getDirOrganization(org = 'myOrg')
#'
#' @keywords internal
#'
getDirOrganization <- function(org = 'epicentre', ...) {
  checkmate::assert_string(org)
  path <- rappdirs::user_data_dir()
  path <- fs::path(path, org, ...)
  path
}

#' @describeIn utils_dir Get directory path within the package
#'
#' This function returns the path of a directory within the package.
#'
#' @param ... Other parameters to be passed to [fs::path].
#'
#' @examples
#' getDirPackage('data')
#' getDirPackage('inst', 'templates')
#'
#' @keywords internal
#'
getDirPackage <- function(...) {
  path <- fs::path(getDirDevelopment(), 'development', ...)
  path
}

#' @describeIn utils_dir Get the SharePoint directory path
#'
#' This function returns the path of the SharePoint directory.
#'
#' @param path Character string indicating the directory inside the SharePoint folder (default is 'MSF')
#'
#' @examples
#' getDirSharepoint()
#' # Returns: 'C:/Users/<Username>/OneDrive/Sharepoint/MSF'
#'
#' getDirSharepoint('Documents')
#' # Returns: 'C:/Users/<Username>/OneDrive/Sharepoint/Documents'
#'
#' @keywords internal
#'
getDirSharepoint <- function(path = 'MSF') {
  path <- fs::path(fs::path_dir(path = Sys.getenv('ONEDRIVE')), path)
  path
}

#' @describeIn utils_dir Get Shiny directory
#'
#' This function returns the directory where the Shiny application is hosted.
#'
#' @param ... Other parameters to be passed to [fs::path].
#'
#' @examples
#' getDirShiny()
#'
#' @keywords internal
#'
getDirShiny <- function(...) {
  if (isHostedApp()) {
    path <- fs::path_wd('site')
  } else {
    path <- fs::path_wd('pkgdown', 'site')
  }
  fs::path(path, ...)
}

#' @describeIn utils_dir Get Temporary Directory Path
#'
#' This function returns the absolute path to the temporary directory.
#'
#' @param ... Other parameters to be passed to [fs::path].
#'
#' @keywords internal
#'
getDirTemp <- function(...) {
  path <- fs::path(getDirApp(), 'temp', ...)
  path
}

#' @describeIn utils_dir Directory of the current user
#'
#' This function returns the directory of the current user.
#'
#' @param ... Other parameters to be passed to [fs::path].
#'
#' @examples
#' getDirUser()
#'
#' @keywords internal
#'
getDirUser <- function(...) {
  if (is.win()) {
    path <- fs::path_dir(getDirUserData(), ...)
  }
  path
}

#' @describeIn utils_dir Get the directory for user data
#'
#' This function returns the directory path for user data. It first checks the operating system. If it is Windows, it calls the function \code{getDirUserDataLocal()} from the \code{fs} package to get the local user data directory. Otherwise, it returns \code{NULL}.
#'
#' @param ... Other parameters to be passed to [fs::path].
#'
#' @keywords internal
#'
getDirUserData <- function(...) {
  checkmate::assert(is.win(), 'This function is only applicable for Windows.')

  path <- fs::path_dir(getDirUserDataLocal())
  path <- fs::path(path, ...)

  path
}

#' @describeIn utils_dir Get local user data directory
#'
#' This function returns the path to the local user data directory. It uses the
#' `rappdirs::user_data_dir` function to get the user data directory and appends
#' any additional path provided.
#'
#' @param ... Other parameters to be passed to [fs::path].
#'
#' @examples
#' getDirUserDataLocal()
#' getDirUserDataLocal('myapp')
#'
#' @keywords internal
#'
getDirUserDataLocal <- function(...) {
  path <- fs::path(rappdirs::user_data_dir(), ...)
  path
}

#' @describeIn utils_dir Get the path to the user's roaming data directory.
#'
#' @param ... Other parameters to be passed to [fs::path].
#'
#' @examples
#' getDirUserDataRoaming('subdir1', 'subdir2')
#' # Returns 'C:/Users/User/AppData/Roaming/subdir1/subdir2'
#'
#' getDirUserDataRoaming('filename.txt')
#' # Returns 'C:/Users/User/AppData/Roaming/filename.txt'
#'
#' @keywords internal
#'
getDirUserDataRoaming <- function(...) {
  path <- fs::path(rappdirs::user_data_dir(roaming = TRUE), ...)
  path
}

#' @describeIn utils_dir Get user's home directory
#'
#' This function returns the path of the user's home directory.
#'
#' @keywords internal
#'
getDirUserHome <- function() {
  path <- fs::path_home()
  if (Sys.info()['sysname'] == 'Windows') {
    path <- fs::path_dir(path = path)
  }
  path
}

#' Get File Cache
#'
#' This function returns the path of the specified file cache.
#'
#' @param name The name of the file cache.
#' @param ext The extension of the file cache. Default value is 'feather'.
#' @keywords internal
#' @return The path of the specified file cache.
#'
getFileCache <- function(name, ext = 'feather') {
  fs::path(getDirApp(), sprintf('%s.%s', name, ext))
}

#' Return the file extension.
#'
#' This function takes a file path as input and returns the file extension. The file extension is defined as the characters after the last period (.) in the file name.
#'
#' @param path The file path for which the extension needs to be determined.
#' @param tolower Logical, indicating whether the returned extension should be converted to lowercase. Default is \code{TRUE}.
#' @return The file extension as a character string.
#' @keywords internal
#'
#' @examples
#' getFileExt('path/to/file.txt') # Returns 'txt'
#' getFileExt('path/to/file.TXT', tolower = FALSE) # Returns 'TXT'
#'
getFileExt <- function(path, tolower = TRUE) {
  fname_parts <- strsplit(path, '.', fixed = TRUE)[[1L]]
  fname_ext <- fname_parts[length(fname_parts)]
  if (tolower) {
    fname_ext <- tolower(fname_ext)
  }
  fname_ext
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

sysWhich <- function(name) {
  checkmate::assertString(x = name)

  path <- Sys.which(names = name)

  if (fs::file_exists(path)) {
    return(path)
  }

  if (.Platform$OS.type == 'windows') {

    r <- try(utils::readRegistry(key = glue::glue('SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\{name}.exe'), hive = 'HLM'), silent = TRUE)

    if (!is.error(r) && names(r)[1L] == '(Default)') {

      path <- r[[1L]]

      if (fs::file_exists(path)) {
        return(path)
      }
    }

  }

  NULL
}

# styler: block Logging

#' Logging
#'
#' This function prints formatted text to the console.
#'
#' @param fmt A character string containing format specifications.
#' @param ... Optional arguments to be formatted.
#'
#' @examples
#' catf('Hello, %s', 'world!')
#' # Output: Hello, world!
#'
catf <- function(fmt, ...) {
  cat(sprintf(fmt, ...))
  utils::flush.console()
}

#' Clean directory cache
#'
#' This function deletes the cache folder in the current working directory.
#'
#' @examples
#' cleanDirCache()
#'
#' @keywords internal
#'
cleanDirCache <- function() {
  fs::dir_delete(getDirApp('*'))
}

compareDataframe <- function(base, compare, keys = NULL, preview = TRUE) {

  base <- base |>
    dplyr::select(names(base)[names(base) %in% names(compare)]) |>
    dplyr::mutate_if(.predicate = ~ methods::is(.x, 'glue'), .funs = as.character)

  compare <- compare |>
    dplyr::select(names(base)) |>
    dplyr::mutate_if(.predicate = ~ methods::is(.x, 'glue'), .funs = as.character)

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

#' Console output function
#'
#' This function prints messages to the console with a timestamp and optional formatting.
#' The function also records the time elapsed since the last call to `console.out`.
#'
#' @param msg The message to print. Can include formatting code.
#' @param ... Additional arguments for formatting the message.
#'
#' @keywords internal
#'
console.out <- function(msg, ...) {
  toc <- tictoc::toc(quiet = TRUE)

  # Uncomment the following line to print time elapsed
  # cat(sprintf(' (%s seconds elapsed)\n', round(toc$toc - toc$tic, 4)))

  # Uncomment the following line to log messages with log4r
  # log4r::debug(.logger, sprintf(msg, ...))

  tictoc::tic()
}

#' @describeIn utils_dir Create Directory
#'
#' This function creates a directory at the specified path. If the directory already exists, it does nothing.
#'
#' @param ... Other parameters to be passed to [fs::path].
#'
#' @examples
#' createDir('path/to/directory')
#'
#' @keywords internal
#'
createDir <- function(...) {
  path <- fs::path(...)
  path <- stringr::str_to_lower(path)
  fs::dir_create(path, recurse = TRUE)
  path
}

#' Delete the directory cache
#'
#' This internal function deletes the directory cache using the \code{fs::dir_delete}
#' function and the \code{getDirApp} function to get the directory path.
#'
#' @keywords internal
#'
deleteDirCache <- function() {
  fs::dir_delete(getDirApp())
}

#' Download a file from a URL.
#'
#' This function downloads a file from a given URL and saves it to a specified destination directory.
#'
#' @param url The URL of the file to be downloaded.
#' @param destfile The name of the file to save the downloaded content to. By default, it generates a file name based on the URL.
#' @param destdir The destination directory where the downloaded file will be saved. By default, it uses the temporary directory.
#'
#' @return The path of the downloaded file.
#' @keywords internal
#'
#' @examples
#' # Download a file and save it to the current working directory
#' download('http://example.com/file.txt')
#'
#' # Download a file and save it to a specific directory with a custom file name
#' download('http://example.com/file.txt', 'data.txt', '/path/to/directory')
download <- function(url, destfile = fs::path_file(url), destdir = fs::path_temp()) {
  destfile <- fs::path(destdir, destfile)
  if (!fs::file_exists(destfile)) {
    destfile <- curl::curl_download(url = url, destfile = destfile, quiet = FALSE)
  }
  invisible(destfile)
}

#' Open File Explorer at Specified Path
#'
#' This function opens the File Explorer at the specified path.
#'
#' @param path A character string specifying the path to open File Explorer at. Default is the current working directory.
#' @return This function is used for its side effects and does not return any value.
#'
#' @examples
#' explore()
#' explore('C:/Users/')
#'
#' @keywords internal
#'
explore <- function(path = fs::path_home()) {
  path <- normalizePath(path)
  if (is.win()) {
    shell(paste('powershell explorer', shQuote(path)))
  } else {
    message('Sorry, this function is only supported on Windows.')
  }
}

#' Explore the directory cache
#'
#' This function explores the directory cache by executing the \code{explore()} function on the current application directory.
#'
#' @keywords internal
#'
exploreDirCache <- function() {
  explore(getDirApp())
}

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
getChoices <- function(df_values, var_name = 'label', var_code = NULL, overall = NULL) {
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
    values <- factor(values, levels = codes$cde_code, labels = codes$cde_label_short)
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
getGroupedChoices <- function(df_values, var_name = 'label', var_code = NULL, var_group = NULL, overall = NULL) {
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

# styler: block Package description

#' Get package description
#'
#' Return the description of the current package.
#'
#' @param fields A character vector specifying the fields to extract from the package description.
#' If not specified, all fields will be returned.
#'
#' @return A character vector containing the requested fields from the package description.
#'
#' @details This function uses the `utils::packageName()` function to get the name of the current package,
#' and then calls `utils::packageDescription()` to retrieve the package description. The `fields` parameter
#' can be used to specify which fields to extract from the package description. If `fields` is not specified,
#' all fields will be returned.
#'
#' @examples
#' \dontrun{
#' desc <- getPackageDescription()
#' print(desc)
#' }
#'
#' @rdname utils_package
#'
#' @keywords internal
#'
getPackageDescription <- function(fields = NULL) {
  desc <- utils::packageDescription(pkg = utils::packageName(), fields = fields)
  desc
}

getPackageEnv <- function() {
  rlang::ns_env(utils::packageName())
}

#' Get a specific field from the package description
#'
#' This function retrieves a specific field from the package description file.
#'
#' @param field The field to retrieve from the package description. If NULL, the entire package description is returned.
#'
#' @return The value of the specified field. If the field is not found, NULL is returned.
#'
#' @examples
#' getPackageField('Title')
#' getPackageField('Version')
#' getPackageField()
#'
#' @keywords internal
#'
getPackageField <- function(field = NULL) {
  getPackageDescription(fields = field)
}

#' Get Package Identifier
#'
#' This function returns the identifier for a package by converting the package name to lowercase and replacing any '.' or '-' with '_'.
#'
#' The function uses the packageName function from the utils package to get the current package name.
#' It then uses the str_replace_all and str_to_lower functions from the stringr package to convert the package name to lowercase and replace any '.' or '-' with '_'.
#' The resulting identifier is then returned.
#'
#' @examples
#' getPackageIdentifier()
#'
#' @return The package identifier as a character string.
#'
#' @keywords internal
#'
getPackageIdentifier <- function() {
  id <- utils::packageName() |>
    stringr::str_replace_all('(\\.| |-)', '_') |>
    stringr::str_to_lower()

  id
}

getPackageOption <- function(x, default = NULL) {
  getOption(x = paste(utils::packageName(), x, sep = '.'), default = default)
}

#' Wrapper to fs::path_package for the package
#'
#' This function is a wrapper for the \code{fs::path_package} function. It returns the path to the installed package.
#'
#' @param ... Other parameters to be passed to [fs::path_package].
#'
#' @details This function internally calls [fs::path_package] and returns the package path by providing the current package name using \code{utils::packageName()}.
#'
#' @return The path to the installed package.
#'
#' @seealso [fs::path_package] which this function wraps.
#'
#' @examples
#' getPackagePath('data.csv')
#'
#' @keywords internal
#'
getPackagePath <- function(...) {
  fs::path(fs::path_package(package = utils::packageName()), ...)
}

#' Get the package title
#'
#' This function retrieves the value of the 'Title' field in a package description file.
#'
#' @return The package title as a character string.
#' @keywords internal
#'
getPackageTitle <- function() {
  getPackageField('Title')
}

#' Get Package Title in Lowercase
#'
#' This function returns the title of a package in lowercase.
#'
#' This function requires the following packages to be installed: \code{tools}.
#'
#' @return The package title in lowercase.
#'
#' @examples
#' getPackageTitleL()
#' ## 'my package'
#'
#' @keywords internal truc machin
#'
getPackageTitleL <- function() {
  tolower(getPackageTitle())
}

#' Get Package Version
#'
#' This function returns the version of the package.
#'
#' @return A character string representing the version of the package.
#'
#' @param ...
#'
#' @inheritDotParams utils::packageVersion -pkg
#'
#' @examples
#' getPackageVersion()
#'
#' @keywords internal
#'
getPackageVersion <- function(...) {
  utils::packageVersion(pkg = utils::packageName(), ...)
}

#' Get the Shiny Server host
#'
#' Retrieves the Shiny Server host based on the session object.
#'
#' If the app is hosted, the Shiny Server host is set to 'https://apps.msf.net'.
#' Otherwise, the Shiny Server host is set to 'https://apps.msf.net'.
#'
#' @param session The Shiny session object.
#'
#' @return The Shiny Server host.
#'
#' @keywords internal
#'
getShinyServerHost <- function(session) {
  if (isHostedApp()) {
    host <- 'https://apps.msf.net'
  } else {
    host <- 'https://apps.msf.net'
  }
  session$clientData$url_hostname
}

#' Check if the package is in development mode
#'
#' This function checks if the current package is in development mode
#'
#' @return A logical value indicating whether the package is in development mode or not
#'
#' @examples
#' is.dev()
#'
#' @keywords internal
#'
is.dev <- function() {
  dcf <- getPackageDescription()
  if ('Development' %in% names(dcf)) {
    as.logical(getPackageDescription()$Development)
  } else {
    FALSE
  }
}

#' Check if the current operating system is Windows
#'
#' This function checks if the current operating system is Windows.
#'
#' @return \code{TRUE} if the current operating system is Windows, \code{FALSE} otherwise.
#'
#' @examples
#' is.win()
#'
#' @keywords internal
#'
is.win <- function() identical(.Platform$OS.type, 'windows')

#' Check if the R platform is 32-bit.
#'
#' This function checks if the R platform is 32-bit by comparing the value of the 'r_arch' component of the .Platform list to 'x32'.
#'
#' @return \code{TRUE} if the R platform is 32-bit, \code{FALSE} otherwise.
#'
#' @examples
#' is.x32()
#'
#' @keywords internal
#'
is.x32 <- function() identical(.Platform$r_arch, 'x32')

#' Check if the R platform is x64
#'
#' This function checks if the R platform is x64.
#'
#' @return \code{TRUE} if the R platform is x64, \code{FALSE} otherwise.
#'
#' @examples
#' is.x64() # Returns TRUE if R platform is x64
#'
#' @keywords internal
#'
is.x64 <- function() identical(.Platform$r_arch, 'x64')

isColors <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(grDevices::col2rgb(X)),
      error = function(e) FALSE
    )
  })
}

#' Function to check if R package is in development mode
#'
#' This function checks if the current R package is in development mode by
#' examining the package description file. It looks for a specific pattern
#' in the version number, which indicates if it is a development version.
#'
#' @return a logical value indicating if the package is in development mode
#'
#' @examples
#' isDevPackage()
#'
#' @keywords internal
#'
isDevPackage <- function() {
  grepl('\\.9[0-9]{3}$', getPackageVersion())
}

#' Check if the app is hosted on Shiny Server
#'
#' This function checks whether the Shiny Server environment variable 'SHINY_SERVER_VERSION'
#' is set and returns TRUE only if it is true, and the application is running in a shiny session.
#'
#' @return Returns TRUE if the app is hosted on Shiny Server and FALSE otherwise.
#'
#' @examples
#' isHostedApp()
#' # expected output: FALSE
#'
#' @keywords internal
#'
isHostedApp <- function() {
  nzchar(Sys.getenv('SHINY_SERVER_VERSION')) && isShinyApp()
}

#' Check if a package is loaded in the development environment
#'
#' This function checks if a package called 'devtools_shims' is loaded in the development environment by searching for its associated file using the \code{utils::find} function.
#'
#' @return A logical value indicating if the package is loaded in the development environment.
#'
#' @keywords internal
#'
#' @examples
#' isLoadedInDev()
#' # [1] TRUE
#'
isLoadedInDev <- function() {
  'devtools_shims' %in% utils::find('system.file')
}

#' Check if the current R session is a Shiny application
#'
#' This function checks if the \code{shiny} package is loaded and if the current R session is running
#' a Shiny application. It returns a logical value indicating whether or not the current session is
#' a Shiny app.
#'
#' @return A logical value indicating whether the current R session is a Shiny app or not.
#'
#' @examples
#' isShinyApp()
#'
#' @keywords internal
#'
isShinyApp <- function() {
  b <- isNamespaceLoaded('shiny')
  if (b) {
    b <- eval(parse(text = 'shiny::isRunning()'))
  }
  b
}

#' Check if the current R session is running a Shiny server application
#'
#' This function is used to determine whether the current R session is running a Shiny server application.
#' It internally calls the \code{\link{isHostedApp}} function to determine this.
#'
#' @return \code{TRUE} if the current R session is running a Shiny server application, \code{FALSE} otherwise.
#' @examples
#' isShinyServerApp()
#'
#' @keywords internal
#'
isShinyServerApp <- function() {
  isHostedApp()
}

#' Paste Strings with Ignoring NA Values
#'
#' This function concatenates strings together, while ignoring any NA values.
#'
#' @param ... A series of character vectors that need to be concatenated.
#' @param sep The separator to be used between each element in the resulting string (default is a space).
#' @param sort Logical indicating whether to sort the elements in the resulting string (default is FALSE).
#' @param unique Logical indicating whether to remove duplicates from the resulting string (default is FALSE).
#'
#' @return A character vector with the concatenated strings, where any NA values are ignored.
#'
#' @examples
#' paste_ignore_NA('a', 'b', NA, 'c')
#' # Result: 'a b c'
#' paste_ignore_NA('a', 'b', NA, 'c', sep = '-')
#' # Result: 'a-b-c'
#' paste_ignore_NA('a', 'b', NA, 'c', sort = TRUE)
#' # Result: 'a b c'
#' paste_ignore_NA('a', 'b', NA, 'c', unique = TRUE)
#' # Result: 'a b c'
#'
paste_ignore_NA <- function(..., sep = ' ', sort = FALSE, unique = FALSE) {
  x <- purrr::list_transpose(rlang::list2(...))
  x <- purrr::map(x, dropNA)
  if (unique) {
    x <- purrr::map(x, unique)
  }
  if (sort) {
    x <- purrr::map(x, sort)
  }
  x <- purrr::map(x, paste0, collapse = sep)
  x <- unlist(x)
  x[x == ''] <- NA_character_
  x
}

#' Calculates the percentage of a given number
#'
#' @param x A numeric value
#'
#' @return A character string with the percentage of the given number
#'
#' @examples
#' pct(0.5)
#'
#' @keywords internal
#'
pct <- function(x) {
  stopifnot(is.numeric(x))
  sprintf('%s%%', x)
}

#' Progress
#'
#' This function allows you to track progress by printing the current value.
#' By default, the function starts at 1 and increments by 1 each time it is called.
#'
#' @param begin logical. If \code{TRUE}, the progress counter is set to the initial value specified by \code{value} and \code{increment}.
#' @param value numeric. The initial value of the progress counter.
#' @param increment numeric. The amount by which the progress counter should be incremented.
#'
#' @keywords internal
#'
#' @examples
#' progress(begin = TRUE, value = 1L, increment = 2L)
#' progress() # Prints '3'
#' progress() # Prints '5'
#' progress(begin = TRUE, value = 10L, increment = -1L)
#' progress() # Prints '9'
#' progress() # Prints '8'
#'
progress <- function(begin = FALSE, value = 1L, increment = 1L) {
  if (begin) {
    assign('progress.value', value, .globals)
    assign('progress.increment', increment, .globals)
  } else {
    assign(
      'progress.value',
      get('progress.value') +
        get('progress.increment'), .globals
    )
  }
  cat(as.character(get('progress.value')), ' ')
}

#' Create a progress bar
#'
#' This function creates a progress bar with customizable style.
#'
#' @inheritDotParams cli::cli_progress_bar
#'
#' @return The progress bar with the specified style.
#'
#' @examples
#' progressBar()
#'
progressBar <- function(..., msg, detail, .envir = parent.frame()) {
  dots <- rlang::list2(...)
  dots$.envir <- .envir
  if (!('format' %in% names(dots))) {
    dots$format <- '{cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}'
    if (!missing(msg)) {
      dots$format <- paste(msg, dots$format)
    }
    if (!missing(detail)) {
      dots$format <- paste0(dots$format, ': ', detail)
    }
  }
  do.call(cli::cli_progress_bar, dots)
}

#' Progress Done
#'
#' This function is a wrapper around the `cli_progress_done` function from the `cli` package.
#' It marks the progress as done and updates the progress bar accordingly.
#'
#' @inheritDotParams cli::cli_progress_done
#'
#' @examples
#' # Create a progress bar
#' pb <- cli::cli_progress_bar(total = 10L)
#' # Update the progress bar
#' progressDone(pb)
#'
progressDone <- function(..., .envir = parent.frame()) {
  try(cli::cli_progress_done(..., .envir = .envir), silent = TRUE)
}

#' Updates the progress bar
#'
#' This function updates the progress bar displayed by the `cli` package.
#' It passes the arguments to `cli_progress_update()` function from the `cli` package.
#'
#' @inheritDotParams cli::cli_progress_update
#'
#' @return None
#'
progressUpdate <- function(..., .envir = parent.frame()) {
  try(cli::cli_progress_update(..., .envir = .envir), silent = TRUE)
}

#' Read RDS file from cache
#'
#' This function reads an RDS file from the cache directory using the provided name.
#' If the file exists, it is read and returned as an R object. If the file does not exist,
#' NULL is returned.
#'
#' @param name The name of the RDS file (default is 'config').
#' @return The R object read from the RDS file, or NULL if the file does not exist.
#' @keywords internal
#'
#' @examples
#' # Read RDS file from cache
#' data <- readRDSFromCache()
#' print(data)
#'
readRDSFromCache <- function(name = 'config') {
  path <- getFileCache(name, ext = 'rds')
  if (fs::file_exists(path = path)) {
    rds <- readRDS(file = path)
  } else {
    rds <- NULL
  }
  rds
}

#' Save RDS file to cache
#'
#' This function saves an RDS file to the cache directory. If the \code{rds} argument is \code{NULL}, the function will delete the RDS file from the cache if it exists.
#'
#' @keywords internal
#'
#' @param rds The R object to be saved as RDS file.
#' @param name The name of the RDS file in the cache directory. Default is 'config'.
#'
#' @examples
#' # Save an RDS object to cache
#' data <- iris
#' saveRDSToCache(data, 'mydata')
#'
#' # Delete an RDS file from cache
#' saveRDSToCache(NULL, 'mydata')
#'
saveRDSToCache <- function(rds, name = 'config') {
  path <- getFileCache(name, ext = 'rds')
  if (is.null(rds)) {
    if (fs::file_exists(path = path)) {
      fs::file_delete(path = path)
    }
  } else {
    saveRDS(object = rds, file = path)
  }
}

#' splitMsg
#'
#'
#' This function splits a message into its components.
#'
#' @param msg The message to be split.
#' @return A list containing the split components of the message.
#'
#' @keywords internal
#'
#' @examples
#' splitMsg('INFO [2022-03-15 13:25:30] This is a sample log message.| Detail: Sample detail.|12345')
#'
splitMsg <- function(msg) {
  msg <- unlist(stringr::str_match_all(msg, '([A-Z]+) \\[(.*)\\] (.*)\\|(.*)\\|([0-9]*)'))
  list(
    log_type = msg[2L],
    log_time = msg[3L],
    log_message = msg[4L],
    log_detail = msg[5L],
    log_value = as.integer(msg[6L])
  )
}

#' Stop Function
#'
#' This function creates an error object with the given message and stops execution.
#'
#' @param msg A character string representing the error message.
#' @param ... Additional arguments to be passed to the \code{sprintf} function.
#'
#' @keywords internal
#'
#' @return This function does not return a value. It stops the execution by throwing an error.
#'
#' @examples
#' stopf('This is an error message')
#' stopf('Invalid input: %s', input)
#'
stopf <- function(msg, ...) {
  msg <- sprintf(msg, ...)
  obj <- simpleError(msg, call = sys.call(sys.parent()))
  stop(obj)
}

#' @title Tictoc function
#' @description This function evaluates the given expression and measures the execution time.
#' @param expr An expression to be evaluated.
#' @details This function internally uses the tic() and toc() functions from the tictoc package to measure the execution time of the given expression.
#' @return The result of evaluating the expression.
#' @keywords internal
#'
tctoc <- function(expr) {
  tictoc::tic('Expression')
  eval(expr)
  tictoc::toc(func.toc = tctoc_msg)
}

#' Tic-Toc Message
#'
#' This function calculates the time elapsed between two time points (tic and toc) and returns a formatted message with the elapsed time in milliseconds.
#'
#' @param tic The starting time point.
#' @param toc The ending time point.
#' @param msg An optional message that will be included in the output.
#' @return A character string with the formatted message including the elapsed time in milliseconds.
#' @keywords internal
#'
#' @examples
#' tic <- Sys.time()
#' # Some time-consuming operation
#' toc <- Sys.time()
#' message <- 'Operation completed successfully.'
#' tctoc_msg(tic, toc, message)
#'
tctoc_msg <- function(tic, toc, msg) {
  paste0(' (', round((toc - tic) * 1000L, 1L), ' milliseconds elapsed)')
}

#' Write character vector as a UTF-8 encoded file
#'
#' This function writes a character vector as a UTF-8 encoded file.
#'
#' @param x A character vector to be written to the file.
#' @param file The path to the file to be written.
#' @param bom A logical value indicating whether a Byte Order Mark (BOM) should be included in the file. Default is \code{FALSE}.
#' @keywords internal
#'
#' @examples
#' # Create a character vector
#' text <- c('Hello', 'world!')
#'
#' # Write the character vector to a file without BOM
#' writeUtf8(text, 'output.txt')
#'
#' # Write the character vector to a file with BOM
#' writeUtf8(text, 'output.txt', bom = TRUE)
#'
writeUtf8 <- function(x, file, bom = FALSE) {
  con <- file(file, 'wb')
  on.exit(close(con))
  if (bom) {
    writeBin(charToRaw('\xEF\xBB\xBF'), con, endian = 'little')
  }
  writeBin(charToRaw(x), con, endian = 'little')
}

# styler: block Misc

#' Empty Operator
#'
#' This function returns the non-empty input among two given inputs.
#'
#' @param a The first input.
#' @param b The second input.
#' @return The non-empty input between \code{a} and \code{b}.
#' @keywords internal
#' @examples
#' x <- 'Hello'
#' y <- ''
#' z <- x %empty% y
#' # z will be 'Hello'
#'
#' x <- ''
#' y <- 'World'
#' z <- x %empty% y
#' # z will be 'World'
#'
#' x <- ''
#' y <- ''
#' z <- x %empty% y
#' # z will be ''
#'
`%empty%` <- function(a, b) {
  if (a != '') a else b
}

#' Add a class to an object
#'
#' This function adds a new class to an object.
#'
#' @param x An object
#' @param what The name of the class to add
#'
#' @return The object with the new class
#'
#' @examples
#' x <- 1L
#' addClass(x, 'foo')
#' # Output:
#' # [1] 1
#' # attr(,'class')
#' # [1] 'numeric' 'foo'
#' class(x)
#' # Output:
#' # [1] 'numeric' 'foo'
#'
#' @keywords internal
#'
addClass <- function(x, what) {
  stopifnot(is.character(what))
  class(x) <- unique(c(class(x), what))
  stopifnot(methods::is(x, c(class(x), what)))
  x
}

#' Calculate the angle between three points in a two-dimensional Cartesian coordinate system.
#'
#' This function calculates the angle in degrees between three points (x0, y0), (x1, y1), and (x2, y2) in a two-dimensional Cartesian coordinate system.
#'
#' @param x0 The x-coordinate of the first point.
#' @param y0 The y-coordinate of the first point.
#' @param x1 The x-coordinate of the second point.
#' @param y1 The y-coordinate of the second point.
#' @param x2 The x-coordinate of the third point.
#' @param y2 The y-coordinate of the third point.
#'
#' @return The angle in degrees between the three points.
#' @keywords internal
#'
#' @examples
#' angle(0L, 0L, 1L, 0L, 0L, 1L)
#' # Returns 45
#'
#' angle(0L, 0L, 1L, 1L, 0L, 1L)
#' # Returns 90
#'
angle <- function(x0, y0, x1, y1, x2, y2) {
  ab <- c(x1, y1) - c(x0, y0)
  ac <- c(x2, y2) - c(x0, y0)
  a <- acos((ab[1L] * ac[1L] + ab[2L] * ac[2L]) / (sqrt(ab[1L]^2L + ab[2L]^2L) * sqrt(ac[1L]^2L + ac[2L]^2L)))
  a <- a * 180.0 / pi
  a
}

#' Prompt the user for input and return the entered value.
#'
#' @param prompt A character string specifying the prompt to display.
#' @return A character string with the user input.
#'
#' @keywords internal
#'
#' @examples
#' ask('Enter your name: ')
#' ask('Enter a number: ')
#'
ask <- function(prompt) {
  checkmate::assertString(prompt)

  cat(prompt)

  con <- if (interactive()) stdin() else file('stdin')
  out <- scan(file = con, what = character(), sep = ',', nlines = 1L, quiet = TRUE)
  out <- stringr::str_trim(tolower(out))
  if (length(out) == 0L) {
    out <- ''
  }
  cat('\n')
  out
}

#' @title Assertive check for dropClass
#'
#' @description This function checks if a given object belongs to a specific class and raises an error if it does.
#'
#' @param x An object
#' @param what The class to be dropped
#'
#' @keywords internal
#'
#' @examples
#' assert_dropClass('abc', 'character')
#' # Error: !any(class('abc') == 'character') is not TRUE
#' assert_dropClass(123L, 'numeric')
#' # Error: !any(class(123) == 'numeric') is not TRUE
#'
assert_dropClass <- function(x, what) {
  stopifnot(!any(methods::is(x, what)))
}

#' Assign Names to a Vector
#'
#' This function assigns names to elements of a vector.
#'
#' @param x A vector.
#' @param names A character vector specifying the names to assign to the elements of the vector \code{x}.
#' @return A vector with assigned names.
#' @keywords internal
#'
#' @examples
#' c_named(c(1L, 2L, 3L), c('one', 'two', 'three'))
#'
c_named <- function(x, names) {
  x_na <- is.na(names)
  x <- c(x[!x_na])
  names <- names[!x_na]
  names(x) <- names
  x
}

#' Convert Color Names to Hexadecimal Format
#'
#' This function takes a color name and converts it to hexadecimal format.
#'
#' @param col a character vector specifying the color name(s) to be converted.
#' @param alpha a logical value indicating whether to include alpha channel in the resulting hexadecimal color(s). Default is \code{FALSE}.
#' @return a character vector of the color(s) in hexadecimal format.
#' @examples
#' col2hex('red') # '#FF0000'
#' col2hex(c('blue', 'green')) # c('#0000FF', '#00FF00')
#'
#' @keywords internal
#'
col2hex <- function(col, alpha = FALSE) {
  c <- grDevices::col2rgb(col, alpha)
  c <- grDevices::rgb(c[1L], c[2L], c[3L], maxColorValue = 255L)
  c
}

#' Collapse a character vector into a single string with elements separated by a delimiter.
#'
#' This function takes a character vector and collapses it into a single string by concatenating its elements with a specified delimiter. By default, each element is quoted using double quotes. The delimiter can be specified using the `sep` parameter.
#'
#' @param x A character vector to be collapsed.
#' @param quote If `TRUE`, each element of `x` will be quoted using double quotes. Default is `TRUE`.
#' @param sep The delimiter to be used to separate the elements of `x`. Default is `', '`.
#'
#' @return A single string with collapsed elements separated by the specified delimiter.
#'
#' @examples
#' collapse(c('a', 'b', 'c'))
#' # Output: 'a, b, c'
#'
#' collapse(c('a', 'b', 'c'), quote = FALSE, sep = '-')
#' # Output: a-b-c
#'
#' @seealso \code{\link{paste}}
#'
#' @keywords internal collapse string
#'
collapse <- function(x, quote = TRUE, sep = ', ') {
  if (quote) {
    x <- sprintf('"%s"', x)
  }
  paste(x, collapse = sep)
}

#' Convert radians to degrees
#'
#' This function takes an angle in radians and converts it to degrees.
#'
#' @param angle The angle in radians.
#'
#' @return The angle in degrees.
#'
#' @examples
#' degrees(pi / 2L)
#' # Output: 90
#'
#' @keywords internal
#'
degrees <- function(angle) {
  (angle * 180L) / pi
}

#' Convert angle from degrees to radians
#'
#' This function takes an angle in degrees and converts it to radians.
#'
#' @param angle numeric value representing the angle in degrees
#'
#' @return numeric value representing the angle in radians
#'
#' @examples
#' radians(90L)
#'
radians <- function(angle) {
  (angle * pi) / 180L
}

#' Drop a class from an object
#'
#' This function can be used to drop a specific class from an object.
#'
#' @param x An object
#' @param what The class to drop
#'
#' @return An object with the specified class removed
#'
#' @examples
#' x <- structure(1L, class = c('a', 'b'))
#' dropClass(x, 'b')
#'
#' @keywords internal
#'
dropClass <- function(x, what) {
  stopifnot(length(class(x)) > 0L)
  class(x) <- class(x)[class(x) != what]
  stopifnot(!any(class(x) == what))
  stopifnot(length(class(x)) == length(class(x)[class(x) != what]))
  x
}

#' Drops NA Values
#'
#' This function takes a vector and removes any NA values from it.
#'
#' @param x The input vector
#'
#' @return A vector with NA values removed
#'
#' @examples
#' dropNA(c(1L, 2L, NA, 3L, NA, 4L))
#' # [1] 1 2 3 4
#'
#' dropNA(c('a', 'b', NA, 'c', NA, 'd'))
#' # [1] 'a' 'b' 'c' 'd'
#'
#' dropNA(c(TRUE, FALSE, NA, TRUE, NA, FALSE))
#' # [1]  TRUE FALSE  TRUE FALSE
#'
#' @keywords internal
#'
dropNA <- function(x) {
  x[!is.na(x)]
}

#' Drop Null Values
#'
#' This function removes null values from a given input.
#'
#' @param x The input vector or list.
#'
#' @return A vector or list with null values removed.
#'
#' @examples
#' dropNulls(c(1L, NULL, 3L, NULL))
#' # Output: [1] 1 3
#'
#' dropNulls(list('a', NULL, 'b', NULL))
#' # Output: [[1]] [1] 'a'
#' #         [[2]] [1] 'b'
#'
#' @keywords internal
#'
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1L))]
}

#' Drop Nulls or Empty Values
#'
#' Remove elements that are either NULL or empty from a vector or list.
#'
#' @param x A vector or a list.
#'
#' @return A vector or a list with the NULL or empty values removed.
#'
#' @examples
#' dropNullsOrEmpty(c(1L, 2L, NULL, '', 5L))
#' # [1] 1 2 5
#'
#' dropNullsOrEmpty(list(1L, 2L, NULL, '', 5L))
#' # [[1]]
#' # [1] 1
#' #
#' # [[2]]
#' # [1] 2
#' #
#' # [[3]]
#' # [1] 5
#'
#' @keywords internal
#'
dropNullsOrEmpty <- function(x) {
  x[!vapply(x, nullOrEmpty, logical(1L))]
}

#' Compare two files using Beyond Compare
#'
#' This function compares two files using Beyond Compare. It converts the source and target to strings and passes them as arguments to the system command 'bcompare' along with the necessary options for a comparison using SFTP protocol.
#'
#' @param source A character string representing the path to the source file.
#' @param target A character string representing the path to the target file.
#'
#' @examples
#' compare('sourcefile', 'targetfile')
#'
#' @keywords internal truc
#'
compare <- function(source, target, remote = FALSE) {
  checkmate::assert_character(source)
  checkmate::assert_character(target)

  if (remote) {
    target <-
      sprintf(
        'sftp://%s:%s@%s:22/%s',
        getRStudioUser(),
        getRStudioPassword(),
        getRStudioHost(),
        target
      )
  }

  system2(
    command = sysWhich('bcompare'),
    args = sprintf('"%s" "%s"', source, target)
  )
}

#' Returns the local IP address.
#'
#' This function retrieves the local IP address using the 'ipconfig' command in Windows.
#' If the function succeeds it returns the local IP address as a character string. If the function fails it returns an empty string.
#'
#' @details This function is mainly designed for Windows systems where the 'ipconfig' command is available.
#' It uses the 'ipconfig' command in the system shell to retrieve the network configuration information and extract the local IP address.
#' The function filters the output of 'ipconfig' to find the line containing the IPv4 address and extracts the address from it.
#' The extracted IP address is returned as a character string.
#'
#' @return A character string representing the local IP address.
#' If the function fails to retrieve the IP address, an empty string is returned.
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   getIPLocal()
#' }
#' }
#'
#' @keywords internal
#'
getIPLocal <- function() {
  x <- system('ipconfig', intern = TRUE)
  x <- x[grep('IPv4', x, fixed = TRUE)]
  x <- gsub('.*? ([[:digit:]])', '\\1', x)
  x
}

getIdentifierFromLabel <- function(label, prefix) {
  id <- label |>
    stringr::str_replace_all('[^[:alnum:]]+', '_') |>
    stringr::str_replace_all('_+', '_') |>
    stringr::str_replace_all('^_|_$', '') |>
    stringr::str_to_lower()
  if (!missing(prefix)) {
    id <- paste(prefix, id, sep = '_')
  }
  id
}

hasVars <- function(x, var_name) {
  var_name %in% names(x)
}

#' Check if all elements in a vector are whole numbers
#'
#' This function takes a numeric vector and returns TRUE if all elements in the vector are whole numbers, and FALSE otherwise.
#'
#' @param x a numeric vector
#'
#' @return TRUE if all elements in \code{x} are whole numbers, FALSE otherwise
#'
#' @examples
#' is.all.whole.number(c(1L, 2L, 3L)) # should return TRUE
#' is.all.whole.number(c(1.5, 2.5, 3.5)) # should return FALSE
#'
#' @keywords internal
#'
is.all.whole.number <- function(x) {
  all(floor(x) == x, na.rm = TRUE)
}

#' Check if an object is defined
#'
#' This function checks if an object is defined in the R environment.
#'
#' @param x An object to be checked.
#'
#' @return A logical value indicating if the object is defined or not.
#'
#' @examples
#' is.defined(a)
#' # FALSE
#' a <- 5L
#' is.defined(a)
#' # TRUE
#'
#' @keywords internal
#'
is.defined <- function(x) {
  !is.null(x)
}

#' Determine if an object is a try-error
#'
#' This function takes an object as input and returns \code{TRUE} if the object
#' is a try-error object and \code{FALSE} otherwise.
#'
#' @param x An object to check for being a try-error
#'
#' @return \code{TRUE} if \code{x} is a try-error, \code{FALSE} otherwise
#'
#' @examples
#' is.error(tryCatch(stop('An error occurred'), error = function(e) e))
#' # Returns TRUE
#'
#' is.error(10L)
#' # Returns FALSE
#'
#' is.error(NULL)
#' # Returns FALSE
#'
#' @keywords internal
#'
is.error <- function(x) {
  methods::is(x, 'try-error')
}

#' Check if an object is not null
#'
#' This function checks if an R object is not null.
#'
#' @param x The object to be checked
#'
#' @return A logical value indicating whether the object is not null
#'
#' @examples
#' is.not.null(NULL)
#' # FALSE
#'
#' is.not.null(1L)
#' # TRUE
#'
#' @keywords internal
#'
is.not.null <- function(x) {
  !is.null(x)
}

#' Checks if a given object is NULL, empty, or containing only empty strings.
#'
#' This function returns TRUE if the object is NULL, has a length of 0, or is an empty string ('').
#' Otherwise, it returns FALSE.
#'
#' @param x The object to be checked.
#'
#' @return A logical value indicating if the object is NULL, empty, or containing only empty strings.
#'
#' @keywords internal
#'
#' @examples
#' is.null.or.empty(NULL)
#' # TRUE
#' is.null.or.empty(character())
#' # TRUE
#' is.null.or.empty('')
#' # TRUE
#' is.null.or.empty(0L)
#' # FALSE
is.null.or.empty <- function(x) {
  is.null(x) || length(x) == 0L || x == ''
}

#' Check if values in a vector are numeric or character
#'
#' This function determines whether the values in a vector are numeric or character. It can handle atomic vectors or lists.
#'
#' @param x A vector or list
#' @return A logical vector indicating whether each element in \code{x} is numeric or character
#' @keywords internal
is.numeric.character <- function(x) {
  stopifnot(is.atomic(x) || is.list(x))
  is.na(suppressWarnings(as.numeric(x))) == is.na(x)
}

#' Check if a vector has only unique elements
#'
#' This function takes a vector as input and checks if it contains only unique elements.
#'
#' @param x A vector of any type
#'
#' @return TRUE if all elements of the vector are unique, FALSE otherwise.
#'
#' @examples
#' is.unique(c(1L, 2L, 3L)) # TRUE
#' is.unique(c(1L, 1L, 2L)) # FALSE
#'
#' @keywords internal
#'
is.unique <- function(x) {
  anyDuplicated(x) == 0L
}

#' Check if a value is null or empty
#'
#' This function checks if a value is \code{NULL} or has zero length.
#'
#' @param x The value to be checked.
#' @return \code{TRUE} if the value is \code{NULL} or has zero length, \code{FALSE} otherwise.
#' @keywords internal
#'
#' @examples
#' nullOrEmpty(NULL)
#' # [1] TRUE
#' nullOrEmpty(character())
#' # [1] TRUE
#' nullOrEmpty(5L)
#' # [1] FALSE
nullOrEmpty <- function(x) {
  is.null(x) || length(x) == 0L
}
#'
#' This function prompts the user to choose from a list of options.
#'
#' @param prompt A character string representing the prompt displayed to the user. Default is 'Choose between: '.
#' @param choice A character vector containing the available options for the user to choose from. Default is c('Yes', 'No').
#'
#' @return The user's selected option from the provided choices as a character string, or NA if the selection is outside the valid range.
#'
#' @examples
#' promptChoice() # Prompts user with default options 'Yes' and 'No'
#' promptChoice(prompt = 'Please select one of the following:', choice = c('Option A', 'Option B'))
#'
promptChoice <- function(prompt = 'Choose between: ', choice = c('Yes', 'No')) {
  checkmate::assertString(prompt)
  checkmate::assertCharacter(choice, names = 'unnamed')

  if (!rlang::is_interactive()) {
    cli::cli_abort(
      c(
        'User input required, but session is not interactive.',
        'Query: {text}'
      )
    )
  }
  prompt <- paste0(prompt, '\n\n')
  prompt <- paste0(prompt, paste0(' ', seq_along(choice), ': ', choice, collapse = '\n'), '\n')

  cli::cli_alert(prompt)

  n <- promptInteger(prompt = 'Selection', range = seq_along(choice))

  if (n > 0L && n <= length(choice)) {
    out <- choice[n]
  } else {
    out <- NA
  }

  out
}

#' Prompt for an identifier
#'
#' This function prompts the user to enter an identifier and validates
#' the input based on a specified pattern.
#'
#' @param prompt A character string specifying the prompt message.
#'   Defaults to 'Enter an identifier'.
#' @param default A character string specifying the default value for
#'   the identifier. Defaults to 'epi'.
#' @return A valid identifier as a character string.
#' @keywords internal
#'
#' @examples
#' promptIdentifier()
#' promptIdentifier(prompt = 'Please enter a variable name', default = 'var1')
#'
promptIdentifier <- function(prompt = 'Enter an identifier', default = 'epi') {
  checkmate::assertString(prompt)

  id <- readline(prompt = sprintf('%s (default: %s): ', prompt, default))

  if (id == '') {
    id <- default
  }

  id_valid <- checkmate::testString(x = id, pattern = sprintf('[0-9a-z\\.]{%s}', nchar(id)))

  if (!id_valid) {
    promptIdentifier(prompt = prompt)
  }

  id
}

#' Prompt for an Integer
#'
#' This function prompts the user to enter an integer value and returns the
#' entered value as an integer. If the entered value is not a valid integer,
#' the function prompts again until a valid integer is entered.
#'
#' @param prompt The prompt message to display to the user (default: 'Enter an integer')
#' @param default The default value to display in the prompt message (default: 0)
#' @return The entered value as an integer
#' @keywords internal
#'
#' @examples
#' promptInteger()
#' promptInteger('Please enter a positive integer', 1L)
promptInteger <- function(prompt = 'Enter an integer', range = NULL, default = 0L) {
  checkmate::assertString(prompt)

  if (!is.null(range)) {
    prompt <- sprintf('%s (%s-%s): ', prompt, range[1L], range[2L])
  }

  n <- readline(prompt = prompt)

  if (id == '') {
    id <- default
  }

  if (!grepl('^[0-9]+$', n)) {
    promptInteger(prompt = prompt)
  }

  as.integer(n)
}

#' Replace text in a file
#'
#' This function replaces specified patterns in a file with new values.
#'
#' @param path A character string specifying the path to the file to be modified.
#' @param new_path A character string specifying the path where the modified file will be saved.
#' @param replacements A named character vector of patterns and their corresponding replacement values.
#'
#' @return The function writes the modified file to the specified location.
#'
#' @examples
#' # Replace 'old' with 'new' in file.txt and save as new_file.txt
#' replace('file.txt', 'new_file.txt', c(old = 'new'))
#'
replace <- function(path, new_path, replacements) {
  checkmate::assertFile(path, access = 'rw')
  checkmate::assertCharacter(replacements, min.len = 1L, any.missing = FALSE, names = 'named', unique = TRUE)

  rl <- readLines(con = path)

  repl_patterns <- names(replacements)
  repl_patterns <- sprintf('\\{\\{ *%s *\\}\\}', repl_patterns)
  repl_replacements <- unname(replacements)

  for (i in seq_along(repl_patterns)) {
    rl <- stringr::str_replace_all(
      string = rl,
      pattern = repl_patterns[i],
      replacement = repl_replacements[i]
    )
  }

  writeLines(text = rl, con = new_path)
}

#' Save Objects to a File
#'
#' This function saves objects to a file using the `save()` function in R.
#'
#' @param ... objects to be saved
#' @param file character string specifying the file name or path
#'
#' @keywords internal
#'
#' @examples
#' # save two variables to a file named 'my_data.RData'
#' saveit(var1, var2, file = 'my_data.RData')
#'
saveit <- function(..., file) {
  x <- rlang::list2(...)
  save(list = names(x), file = file, envir = list2env(x))
}

#' Calculate a score for uniqueness of elements in a character vector
#'
#' This function calculates a score for the uniqueness of elements in a character vector \code{x}.
#' The score is a value between 0 and 1, where 0 indicates no unique elements and 1 indicates all elements are unique.
#' The uniqueness is determined by counting unique elements in \code{x} and the unique elements after applying the Soundex algorithm.
#' If \code{x} is not a character vector, the function returns 0.
#'
#' @param x A character vector
#'
#' @return A numeric score for the uniqueness of elements in \code{x}
#'
#' @keywords internal
#'
#' @examples
#' score_unique(c('cat', 'dog', 'cat', 'pig'))
#' score_unique(c('John', 'Doe', 'Jane'))
#' score_unique(1:10)
#'
#' @references
#' Levenshtein, V. (1966). Binary codes capable of correcting deletions, insertions, and reversals.
#' Soviet Physics Doklady, 10(8), 707<U+FFFD>710.
#'
#' @seealso
#' \code{\link{stringdist::phonetic}}
#'
score_unique <- function(x) {
  if (is.character(x)) {
    x_sx <- stringdist::phonetic(x, method = 'soundex')
    x_scr <-
      (length(unique(x)) / length(x) / 4L) * 3L +
      (length(unique(x_sx)) / length(x_sx) / 4L)
  } else {
    x_scr <- 0L
  }
  x_scr
}

#' Convert values to character string with specified format
#'
#' This function takes a vector of numeric values and converts them to character
#' string using the specified format. The format can be any valid format
#' specification used in the `sprintf` function. Any occurrences of 'NA' in the
#' resulting string are replaced with '-'.
#'
#' @param x A vector of numeric values
#' @param format A character string specifying the format to use for converting
#'   the values. Default is "%f".
#'
#' @return A character vector with the converted values
#'
#' @examples
#' snumber(1:5, '%.2f')
#' snumber(c(1.234, NA), '%0.3f')
#'
#' @keywords internal
#'
snumber <- function(x, format = '%f') {
  # checkmate::assert_numeric(x, var.name = 'x')
  # checkmate::assert_character(format, var.name = 'format')

  s <- sprintf(format, x)
  s <- gsub('NA', '-', s, fixed = TRUE)
  s
}

#' Sort unique elements in a vector
#'
#' This function takes a vector as input and returns a sorted vector
#' containing only the unique elements of the input vector.
#'
#' @param x A numeric or character vector.
#'
#' @return A sorted vector containing the unique elements of \code{x}.
#'
#' @examples
#' sortu(c(3L, 1L, 2L, 2L, 3L, 1L)) # returns 1, 2, 3
#'
#' @keywords internal
#'
sortu <- function(x) {
  checkmate::assert_vector(x, arg_name = 'x')
  checkmate::assert_compatible(x, is_ordered = FALSE, arg_name = 'x')

  sort(unique(x))
}

#' Wrapper function for string replacement using `[stringr::str_replace_all()]`
#'
#' @inheritParams stringr::str_replace_all
#'
#' @examples
#' strReplaceAll('Hello, world!', '[aeiou]', 'X')
#'
strReplaceAll <- function(string, pattern, replacement) {
  stringr::str_replace_all(string, pattern, replacement)
}

#'
#' Unzip files
#'
#' This function unzips a specified zip file and returns a list of extracted files and their count.
#'
#' @param zipfile The path to the zip file to be extracted.
#' @param pattern A regular expression pattern to filter the extracted files.
#'                Defaults to '.*' which matches all files.
#' @param exdir The path to the directory where the files should be extracted.
#'              If NULL, a temporary directory will be created.
#'
#' @return A list with two elements:
#'   - 'files': A character vector with the paths of the extracted files.
#'   - 'files_count': The number of extracted files.
#'
#' @examples
#' # Unzip a file and get list of files
#' unzip('path/to/zipfile.zip')
#'
#' # Unzip a file, filter by pattern, and get list of files
#' unzip('path/to/zipfile.zip', pattern = 'txt')
#'
#' # Unzip a file and specify the extraction directory
#' unzip('path/to/zipfile.zip', exdir = 'path/to/exdir')
#'
#' @keywords internal
#'
unzip <- function(zipfile, pattern = '.*', exdir = NULL) {
  if (is.null(exdir)) {
    exdir <- fs::path_temp('unzip')
    if (file.exists(exdir)) {
      fs::dir_delete(exdir)
    }
  }
  zip::unzip(zipfile = zipfile, exdir = exdir)
  zipfiles <- fs::dir_ls(exdir, pattern = pattern, full.names = TRUE)
  list(
    files       = zipfiles,
    files_count = length(zipfiles)
  )
}
# styler: block Prompt

#' Prompt the user for input and return the entered value.
#'
#' @param prompt A character string specifying the prompt to display.
#' @return A character string with the user input.
#'
#' @keywords internal
#'
#' @examples
#' ask('Enter your name: ')
#' ask('Enter a number: ')
#'
ask <- function(prompt) {
  checkmate::assertString(prompt)

  cat(prompt)

  con <- if (interactive()) stdin() else file('stdin')
  out <- scan(file = con, what = character(), sep = ',', nlines = 1L, quiet = TRUE)
  out <- stringr::str_trim(out)
  if (length(out) == 0L) {
    out <- ''
  }
  cat('\n')
  out
}
