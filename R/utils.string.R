# String manipulation and formatting utilities

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
  checkmate::assertCharacter(
    replacements,
    min.len = 1L,
    any.missing = FALSE,
    names = 'named',
    unique = TRUE
  )

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
  msg <- unlist(stringr::str_match_all(
    msg,
    '([A-Z]+) \\[(.*)\\] (.*)\\|(.*)\\|([0-9]*)'
  ))
  list(
    log_type = msg[2L],
    log_time = msg[3L],
    log_message = msg[4L],
    log_detail = msg[5L],
    log_value = as.integer(msg[6L])
  )
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
#' Soviet Physics Doklady, 10(8), 707-710.
#'
#' @seealso
#' \code{\link{stringdist::phonetic}}
#'
score_unique <- function(x) {
  if (is.character(x)) {
    x_sx <- stringdist::phonetic(x, method = 'soundex')
    x_scr <-
      (length(unique(x)) / length(x) / 4L) *
      3L +
      (length(unique(x_sx)) / length(x_sx) / 4L)
  } else {
    x_scr <- 0L
  }
  x_scr
}
