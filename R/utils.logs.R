# styler: block

.logs <- new.env(parent = emptyenv())

debug <- function(...) {
  base::debug(...)
}

decorate <- function(self) {
  funcs_active <- names(self$.__active__)
  funcs <- names(self)[-(1:2)]
  funcs <- funcs[
    !(funcs %in%
      c('clone', 'finalize', 'initialize', 'self', 'super', 'private'))
  ]
  funcs <- funcs[!(funcs %in% funcs_active)]
  for (func in funcs) {
    if (class(self[[func]])[1L] == 'function') {
      logDebug('Decorate %s', func)
      unlockBinding(func, self)
      self[[func]] <- logDecorate(class(self)[1L], func, self[[func]])
      lockBinding(func, self)
    }
  }
}

decorateR6 <- function(object) {
  if (getOption('epi.log', FALSE)) {
    return()
  }

  funcs_active <- names(object$.__active__)

  funcs <- names(object)[-(1:2)]
  funcs <- funcs[
    !(funcs %in%
      c(
        'ui',
        'server',
        'clone',
        'finalize',
        'initialize',
        'object',
        'super',
        'private'
      ))
  ]
  funcs <- funcs[!(funcs %in% funcs_active)]

  for (func in funcs) {
    if (class(object[[func]])[1L] == 'function') {
      logDebug('Decorate %s', func)
      unlockBinding(func, object)
      object[[func]] <- logDecorate(class(object)[1L], func, object[[func]])
      lockBinding(func, object)
    }
  }

  funcs <- names(object$private)[-(1:2)]
  funcs <- funcs[!(funcs %in% c('getServer', 'console_out'))]

  for (func in funcs) {
    if (class(object$private[[func]])[1L] == 'function') {
      logDebug('Decorate %s', func)
      unlockBinding(func, object$private)
      object$private[[func]] <- logDecorate(
        class(object)[1L],
        func,
        object$private[[func]]
      )
      lockBinding(func, object$private)
    }
  }
}

decorator <- function(deco, ...) {
  args <- rlang::list2(...)
  fun <- utils::tail(args, 1L)[[1L]]
  deco <- c(list(deco), utils::head(args, -1L))
  clos <- function() {
    stop('Need to call decorate() on this object first')
  }
  attr(clos, 'deco') <- deco
  attr(clos, 'fun') <- fun
  class(clos) <- c('decorator', class(clos))
  clos
}

levelLogger <- function(level = NULL) {
  log4r::level(.logger) <- level
}

logDebug <- function(msg, ...) {
  log4r::debug(.logger, sprintf(msg, ...))
}

logDecorate <- function(classname, funcname, func) {
  force(classname)
  force(funcname)
  force(func)
  logDebug('Decorate %s %s', classname, funcname)
  wrapper <- function(...) {
    if (is.null(.logs$indent)) {
      .logs$indent <- 0L
    }
    logDebug(
      '%s%s / %s (%s seconds elapsed)',
      strrep('  ', .logs$indent),
      funcname,
      classname,
      .logs$tictoc
    )
    assign('indent', .logs$indent + 1L, .logs)
    has_tictoc <- requireNamespace('tictoc', quietly = TRUE)
    if (has_tictoc) tictoc::tic()
    out <- func(...)
    if (has_tictoc) {
      toc <- tictoc::toc(quiet = TRUE)
      assign('tictoc', round(toc$toc - toc$tic, 4L), .logs)
    }
    assign('indent', .logs$indent - 1L, .logs)
    out
  }
  wrapper
}

logDecorator <- function(func) {
  wrapper <- function(...) {
    start <- proc.time()

    func()

    print(proc.time() - start)
  }
  wrapper
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

log_layout <- function(level, ...) {
  paste0(format(Sys.time()), ' [', level, '] ', ..., '\n', collapse = '')
}

# decorate <- function(x) {
#   decorate_method <- function(deco, fun, env) {
#     meth <- deco(fun)
#     parent.env(environment(meth)) <- env
#     meth
#   }
#   method_names <- ls(x, all.names = TRUE)
#   for (mnm in method_names) {
#     if (inherits(x[[mnm]], "decorator")) {
#       get("unlockBinding", baseenv())(mnm, x)
#       deco <- attr(x[[mnm]], "deco")
#       fun <- attr(x[[mnm]], "fun")
#       env <- environment(x[[mnm]])
#       environment(fun) <- env
#       for (d in deco) {
#         fun <- decorate_method(d, fun, env)
#       }
#       x[[mnm]] <- fun
#       lockBinding(mnm, x)
#     }
#   }
#   x
# }
.logger <- log4r::logger(
  threshold = 'DEBUG',
  appenders = list(
    log4r::console_appender(layout = log_layout)
    # log4r::file_appender(fs::path(getDirApp(), 'log.txt'), append = FALSE, layout = log_layout)
  )
)
