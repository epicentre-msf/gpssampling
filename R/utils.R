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

sysWhich <- function(name) {
  checkmate::assertString(x = name)

  path <- Sys.which(names = name)

  if (fs::file_exists(path)) {
    return(path)
  }

  if (.Platform$OS.type == 'windows') {
    r <- try(
      utils::readRegistry(
        key = glue::glue(
          'SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\{name}.exe'
        ),
        hive = 'HLM'
      ),
      silent = TRUE
    )

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
  if (requireNamespace('tictoc', quietly = TRUE)) {
    tictoc::toc(quiet = TRUE)
    tictoc::tic()
  }
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
    tryCatch(is.matrix(grDevices::col2rgb(X)), error = function(e) FALSE)
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
        get('progress.increment'),
      .globals
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

#' @title Tictoc function
#' @description This function evaluates the given expression and measures the execution time.
#' @param expr An expression to be evaluated.
#' @details This function internally uses the tic() and toc() functions from the tictoc package to measure the execution time of the given expression.
#' @return The result of evaluating the expression.
#' @keywords internal
#'
tctoc <- function(expr) {
  if (requireNamespace('tictoc', quietly = TRUE)) {
    tictoc::tic('Expression')
    eval(expr)
    tictoc::toc(func.toc = tctoc_msg)
  } else {
    eval(expr)
  }
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
  a <- acos(
    (ab[1L] * ac[1L] + ab[2L] * ac[2L]) /
      (sqrt(ab[1L]^2L + ab[2L]^2L) * sqrt(ac[1L]^2L + ac[2L]^2L))
  )
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
  out <- scan(
    file = con,
    what = character(),
    sep = ',',
    nlines = 1L,
    quiet = TRUE
  )
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

# styler: block Prompt

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
  prompt <- paste0(
    prompt,
    paste0(' ', seq_along(choice), ': ', choice, collapse = '\n'),
    '\n'
  )

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

  id_valid <- checkmate::testString(
    x = id,
    pattern = sprintf('[0-9a-z\\.]{%s}', nchar(id))
  )

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
promptInteger <- function(
  prompt = 'Enter an integer',
  range = NULL,
  default = 0L
) {
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
    files = zipfiles,
    files_count = length(zipfiles)
  )
}
