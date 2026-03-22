# Port management and background process utilities

# styler: block Port Management

#' Function to check if a port is open on localhost
#'
#' @param port Port number to be checked
#' @param timeout Timeout value for the connection attempt (default is 0.1 seconds)
#'
#' @return Logical value indicating if the port is open
#'
#' @examples
#' checkPort(80L)
#' checkPort(443L, timeout = 0.5)
#'
checkPort <- function(port, timeout = 0.1) {
  con <- tryCatch(
    {
      # Attempt to establish a socket connection
      sock <- socketConnection(
        host = 'localhost',
        port = port,
        server = FALSE,
        blocking = TRUE,
        open = 'connect',
        timeout = timeout
      )
      close(sock)
      TRUE # Return TRUE if the connection is successful
    },
    error = function(e) {
      FALSE # Return FALSE if an error occurs during the connection attempt
    }
  )
  if (is.null(con)) {
    return(TRUE)
  }
  con # Return the result of the connection attempt
}

#' Find a free port to use
#'
#' Uses the OS to find a free TCP port by binding to port 0 (which lets the
#' OS assign any available port), then closes the connection and returns the
#' assigned port. This approach works on all platforms (Windows, macOS, Linux).
#'
#' @param start The minimum port number (default 1024 to avoid privileged ports).
#'   When `random = FALSE`, sequential search starts here.
#' @param random Logical. If `TRUE` (recommended), lets the OS pick any free
#'   port. If `FALSE`, searches sequentially from `start`.
#'
#' @return An integer: a free port number.
#'
#' @examples
#' findFreePort(6000L, TRUE)
#' findFreePort(6000L)
#'
findFreePort <- function(start = 1024L, random = TRUE) {
  if (random) {
    # Try random ports in the ephemeral range (49152-65535)
    for (i in seq_len(100L)) {
      port <- sample(49152L:65535L, 1L)
      if (!checkPort(port)) return(port)
    }
    stop('Could not find a free port after 100 random attempts')
  }

  # Sequential search from start
  for (port in seq.int(max(start, 1024L), 65535L)) {
    if (!checkPort(port)) return(as.integer(port))
  }
  stop('Could not find a free port')
}
getProcessPidByPort <- function(port) {
  if (.Platform$OS.type == 'windows') {
    cmd <- sprintf('netstat -aon | findstr ":%d "', port)
  } else {
    cmd <- sprintf('lsof -ti tcp:%d', port)
  }
  result <- tryCatch(
    system(cmd, intern = TRUE, ignore.stderr = TRUE),
    warning = function(w) character(0L),
    error = function(e) character(0L)
  )
  if (length(result) == 0L) return(NULL)

  if (.Platform$OS.type == 'windows') {
    # Parse Windows netstat output: last column is PID
    pids <- stringr::str_extract(result, '\\d+$')
    pids <- unique(as.integer(pids[!is.na(pids)]))
  } else {
    # lsof -ti returns PIDs directly
    pids <- as.integer(trimws(result))
    pids <- unique(pids[!is.na(pids)])
  }

  if (length(pids) >= 1L) pids[1L] else NULL
}

#' Get table of listening TCP ports
#'
#' Returns a tibble of listening TCP ports with host, port, and PID columns.
#' Works on Windows (netstat), macOS (lsof), and Linux (ss or lsof).
#'
#' @return A tibble with columns: locale_host, locale_port, pid
#'
#' @examples
#' getTablePorts()
#'
getTablePorts <- function() {
  if (.Platform$OS.type == 'windows') {
    output <- tryCatch(
      system('netstat -aon', intern = TRUE, ignore.stderr = TRUE),
      error = function(e) character(0L)
    )
    # Filter to LISTENING TCP lines
    output <- output[grepl('LISTENING', output, fixed = TRUE)]
    output <- output[grepl('TCP', output, fixed = TRUE)]
    # Parse: Proto  Local Address  Foreign Address  State  PID
    parsed <- stringr::str_match(
      trimws(output),
      '^TCP\\s+(\\S+):(\\d+)\\s+\\S+\\s+LISTENING\\s+(\\d+)'
    )
    parsed <- parsed[!is.na(parsed[, 1L]), , drop = FALSE]
    tibble::tibble(
      locale_host = parsed[, 2L],
      locale_port = as.integer(parsed[, 3L]),
      pid = as.integer(parsed[, 4L])
    )
  } else {
    # macOS / Linux: use lsof
    output <- tryCatch(
      system(
        'lsof -iTCP -sTCP:LISTEN -nP',
        intern = TRUE,
        ignore.stderr = TRUE
      ),
      error = function(e) character(0L)
    )
    if (length(output) <= 1L) {
      return(tibble::tibble(
        locale_host = character(0L),
        locale_port = integer(0L),
        pid = integer(0L)
      ))
    }
    # Skip header, parse: COMMAND PID USER FD TYPE DEVICE SIZE/OFF NODE NAME
    output <- output[-1L]
    parsed <- stringr::str_match(
      output,
      '^\\S+\\s+(\\d+)\\s+.*\\s(\\S+):(\\d+)\\s'
    )
    parsed <- parsed[!is.na(parsed[, 1L]), , drop = FALSE]
    tibble::tibble(
      locale_host = parsed[, 3L],
      locale_port = as.integer(parsed[, 4L]),
      pid = as.integer(parsed[, 2L])
    )
  }
}

# styler: block Process

runBackgroundApp <- function(
  app = NULL,
  func = NULL,
  port = getOption('shiny.port', 8000L),
  host = getOption('shiny.host', '127.0.0.1'),
  stdout = '',
  stderr = '',
  force = FALSE,
  load_all = TRUE
) {
  runApp <- function(app, func, host, port, load_all) {
    if (load_all && requireNamespace('devtools', quietly = TRUE)) {
      devtools::load_all()
    }

    if (!is.null(func) && is.null(app)) {
      app <- func(port = port)
    } else {
      shiny::runApp(appDir = app, host = host, port = port)
    }
  }

  pid <- getProcessPidByPort(port = port)

  if (!is.null(pid)) {
    if (force) {
      tools::pskill(pid)
    } else {
      cli::cli_abort('Port {port} is already used by a process (pid: {pid}).')
    }
  }

  args <- list(
    app = app,
    func = func,
    host = host,
    port = port,
    load_all = load_all
  )

  app <- callr::r_bg(
    func = runApp,
    args = args,
    stdout = stdout,
    stderr = stderr
  )

  cli::cli_progress_bar('Start shiny application...')
  while (
    !pingr::is_up(
      destination = '127.0.0.1',
      port = port,
      check_online = FALSE,
      timeout = 0.1
    )
  ) {
    cli::cli_progress_update()
  }

  invisible(app)
}
