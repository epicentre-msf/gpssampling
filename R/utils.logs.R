.logs <- new.env(parent = emptyenv())

debug <- function(...) {
  base::debug(...)
}

levelLogger <- function(level = NULL) {
  log4r::level(.logger) <- level
}

logDebug <- function(msg, ...) {
  log4r::debug(.logger, sprintf(msg, ...))
}

logError <- function(msg, ...) {
  log4r::error(.logger, sprintf(msg, ...))
}

logFatal <- function(msg, ...) {
  log4r::fatal(.logger, sprintf(msg, ...))
}

logInfo <- function(msg, ...) {
  log4r::info(.logger, sprintf(msg, ...))
}

logWarn <- function(msg, ...) {
  log4r::warn(.logger, sprintf(msg, ...))
}

logDecorate <- function(classname, funcname, func) {
  force(classname)
  force(funcname)
  force(func)
  wrapper <- function(...) {
    if (is.null(.logs$indent)) {
      .logs$indent <- 0L
    }
    start <- proc.time()[["elapsed"]]
    logDebug(
      "%s> %s$%s",
      strrep(".", .logs$indent),
      classname,
      funcname
    )
    assign("indent", .logs$indent + 1L, .logs)
    out <- func(...)
    elapsed <- round(proc.time()[["elapsed"]] - start, 4L)
    assign("indent", .logs$indent - 1L, .logs)
    logDebug(
      "%s< %s$%s (%s s)",
      strrep(".", .logs$indent),
      classname,
      funcname,
      elapsed
    )
    out
  }
  wrapper
}

decorate_methods <- function(self, private = NULL, trace_methods = NULL) {
  excluded <- c(
    "clone",
    "finalize",
    "initialize",
    "self",
    "super",
    "private"
  )
  funcs_active <- names(self$.__active__)

  funcs <- names(self)[-(1:2)]
  funcs <- funcs[!(funcs %in% excluded)]
  funcs <- funcs[!(funcs %in% funcs_active)]

  if (!is.null(trace_methods)) {
    funcs <- funcs[funcs %in% trace_methods]
  }

  for (func in funcs) {
    if (is.function(self[[func]])) {
      unlockBinding(func, self)
      self[[func]] <- logDecorate(class(self)[1L], func, self[[func]])
      lockBinding(func, self)
    }
  }

  if (!is.null(private)) {
    priv_funcs <- names(private)[-(1:2)]
    priv_excluded <- c("getServer", "console_out")
    priv_funcs <- priv_funcs[!(priv_funcs %in% priv_excluded)]

    if (!is.null(trace_methods)) {
      priv_funcs <- priv_funcs[priv_funcs %in% trace_methods]
    }

    for (func in priv_funcs) {
      if (is.function(private[[func]])) {
        unlockBinding(func, private)
        private[[func]] <- logDecorate(
          class(self)[1L],
          func,
          private[[func]]
        )
        lockBinding(func, private)
      }
    }
  }
}

log_layout <- function(level, ...) {
  paste0(format(Sys.time()), " [", level, "] ", ..., "\n", collapse = "")
}

log_layout_structured <- function(level, ...) {
  entry <- list(
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3"),
    level = level,
    message = paste0(..., collapse = ""),
    pid = Sys.getpid()
  )
  paste0(jsonlite::toJSON(entry, auto_unbox = TRUE), "\n")
}

init_logger <- function() {
  level <- Sys.getenv("GPSSAMPLING_LOG_LEVEL", "DEBUG")

  layout <- if (identical(Sys.getenv("GPSSAMPLING_LOG_FORMAT"), "json")) {
    log_layout_structured
  } else {
    log_layout
  }

  appenders <- list(
    log4r::console_appender(layout = layout)
  )

  log_dir <- Sys.getenv("GPSSAMPLING_LOG_DIR", "")
  if (nzchar(log_dir)) {
    fs::dir_create(log_dir, recurse = TRUE)
    log_file <- fs::path(log_dir, "gpssampling.log")
    rotate_log(log_file)
    appenders <- c(
      appenders,
      list(log4r::file_appender(log_file, append = TRUE, layout = layout))
    )
  }

  log4r::logger(threshold = level, appenders = appenders)
}

rotate_log <- function(log_path, max_files = 5L, max_size_mb = 10L) {
  if (!fs::file_exists(log_path)) {
    return(invisible(NULL))
  }
  if (fs::file_size(log_path) <= max_size_mb * 1024^2) {
    return(invisible(NULL))
  }
  for (i in seq(max_files - 1L, 1L)) {
    from <- paste0(log_path, ".", i)
    to <- paste0(log_path, ".", i + 1L)
    if (fs::file_exists(from)) fs::file_move(from, to)
  }
  fs::file_move(log_path, paste0(log_path, ".1"))
  invisible(NULL)
}

.logger <- init_logger()
