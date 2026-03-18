# Error handling wrappers for safe operations

#' Safely execute a database operation with error handling
#'
#' Wraps database calls in tryCatch to prevent app crashes from
#' database errors. Logs the error and returns a default value.
#'
#' @param expr Expression to evaluate (a database call).
#' @param default Value to return on error.
#' @param msg Optional context message for logging.
#' @return The result of `expr`, or `default` on error.
#'
#' @keywords internal
#'
safe_db <- function(expr, default = NULL, msg = NULL) {
  tryCatch(
    expr,
    error = function(e) {
      context <- if (!is.null(msg)) paste0("[", msg, "] ") else ""
      logWarn("%sDatabase error: %s", context, conditionMessage(e))
      default
    }
  )
}

#' Safely download a file with error handling
#'
#' Wraps curl_download in tryCatch to handle network failures gracefully.
#'
#' @param url URL to download.
#' @param destfile Destination file path.
#' @param quiet Whether to suppress progress output.
#' @param msg Optional context message for logging.
#' @return The destfile path on success, NULL on error.
#'
#' @keywords internal
#'
safe_download <- function(url, destfile, quiet = TRUE, msg = NULL) {
  tryCatch(
    {
      curl::curl_download(url = url, destfile = destfile, quiet = quiet)
      destfile
    },
    error = function(e) {
      context <- if (!is.null(msg)) paste0("[", msg, "] ") else ""
      logWarn(
        "%sDownload failed for %s: %s",
        context,
        url,
        conditionMessage(e)
      )
      NULL
    }
  )
}

#' Safely copy a file with error handling
#'
#' Wraps fs::file_copy in tryCatch to prevent crashes on I/O failures.
#'
#' @param path Source file path.
#' @param new_path Destination file path.
#' @param overwrite Whether to overwrite existing files.
#' @param msg Optional context message for logging.
#' @return TRUE on success, FALSE on error.
#'
#' @keywords internal
#'
safe_file_copy <- function(path, new_path, overwrite = FALSE, msg = NULL) {
  tryCatch(
    {
      fs::file_copy(path = path, new_path = new_path, overwrite = overwrite)
      TRUE
    },
    error = function(e) {
      context <- if (!is.null(msg)) paste0("[", msg, "] ") else ""
      logWarn(
        "%sFile copy failed from %s to %s: %s",
        context,
        path,
        new_path,
        conditionMessage(e)
      )
      FALSE
    }
  )
}

#' Safely read an RDS file with error handling
#'
#' Wraps readRDS in tryCatch to handle corrupt or missing files.
#'
#' @param file Path to the RDS file.
#' @param default Value to return on error.
#' @param msg Optional context message for logging.
#' @return The deserialized object, or `default` on error.
#'
#' @keywords internal
#'
safe_read_rds <- function(file, default = NULL, msg = NULL) {
  if (!fs::file_exists(file)) {
    return(default)
  }
  tryCatch(
    readRDS(file),
    error = function(e) {
      context <- if (!is.null(msg)) paste0("[", msg, "] ") else ""
      logWarn(
        "%sFailed to read RDS %s: %s",
        context,
        file,
        conditionMessage(e)
      )
      default
    }
  )
}

#' Safely write an RDS file with error handling
#'
#' Wraps saveRDS in tryCatch to handle I/O failures.
#'
#' @param object Object to serialize.
#' @param file Path to save to.
#' @param msg Optional context message for logging.
#' @return TRUE on success, FALSE on error.
#'
#' @keywords internal
#'
safe_save_rds <- function(object, file, msg = NULL) {
  tryCatch(
    {
      saveRDS(object, file)
      TRUE
    },
    error = function(e) {
      context <- if (!is.null(msg)) paste0("[", msg, "] ") else ""
      logWarn(
        "%sFailed to save RDS %s: %s",
        context,
        file,
        conditionMessage(e)
      )
      FALSE
    }
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
