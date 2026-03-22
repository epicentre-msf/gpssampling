# Platform-specific application launcher utilities

#' Create a launcher on the desktop for Windows (.bat), Mac (.command), or Linux (.sh)
#'
#' @details On Windows/Mac/Linux a file named launch.bat/launch.command/launch.sh will be put on the desktop.
#' Double-click the file to launch the specified app app
#'
#' @examples
#' \dontrun{
#' launcher()
#' }
#'
launcher <- function() {
  os <- Sys.info()['sysname']
  if (os == 'Windows') {
    win_launcher()
    # } else if (os == 'Darwin') {
    #   mac_launcher()
    # } else if (os == 'Linux') {
    #   lin_launcher()
  } else {
    message('This function is not available for your platform.')
  }
}

#' Create a launcher and updater for Windows (.bat)
#'
#' @details On Windows a file named 'launch.bat' and one named 'update.bat' will be put on the desktop. Double-click the file to launch the specified app app or update app to the latest version
#'
#' @examples
#' \dontrun{
#' win_launcher()
#' }
#'
win_launcher <- function() {
  if (!interactive()) {
    stop('This function can only be used in an interactive R session')
  }

  if (Sys.info()['sysname'] != 'Windows') {
    message(
      'This function is for Windows only. For Mac use the mac_launcher() function'
    )
  }

  out <- prompt(sprintf(
    'Do you want to create shortcuts for %s on your Desktop? (y/n) ',
    getPackageDescription()$Title
  ))

  if (substr(out, 1L, 1L) %in% c('y', 'Y')) {
    local_dir <- Sys.getenv('R_LIBS_USER')

    if (!fs::file_exists(local_dir)) {
      fs::dir_create(local_dir, recurse = TRUE)
    }

    pt <- fs::path(Sys.getenv('HOME'), 'Desktop')
    if (!fs::file_exists(pt)) {
      pt <- fs::path(Sys.getenv('USERPROFILE'), 'Desktop', fsep = '\\')
    }

    if (!fs::file_exists(pt)) {
      pt <- Sys.getenv('HOME')
      message(paste0(
        '
      The launcher function was unable to find your Desktop. The launcher and
      update files/icons will be put in the directory: ',
        pt
      ))
    }

    pt <- fs::path_norm(pt)

    fp <- fs::path(pt, 'launch.bat')

    commands <- sprintf(
      '"%s" -e "
      if (!require(%s)) {
        install.packages(\'%s\', type = \'binary\',
          repos = c(
            \'http://cran.msf.net\',
            \'http://cloud.r-project.org\'
          )
        )
      };

      library(%s);

      samp <- gpssampling::sampler();
      samp$launch()"',
      Sys.which('R'),
      getPackageDescription()$Package,
      getPackageDescription()$Package,
      getPackageDescription()$Package
    )

    cat(gsub('\n *', '', commands), file = fp, sep = '\n')

    Sys.chmod(fp, mode = '0755')

    fp <- fs::path(pt, 'update.bat')

    commands <- sprintf(
      '"%s" -e "
      install.packages(\'%s\', type = \'binary\',
        repos = c(
          \'http://cran.msf.net\',
          \'http://cloud.r-project.org\'
        )
      );
      suppressWarnings(
        update.packages(\'%s\', ask = FALSE, type = \'binary\',
          lib.loc = .libPaths()[1],
          repos = c(
            \'http://cran.msf.net\',
            \'http://cloud.r-project.org\'
          )
        )
      );
      "',
      Sys.which('R'),
      getPackageDescription()$Package,
      getPackageDescription()$Package
    )

    cat(gsub('\n *', '', commands), file = fp, sep = '\n')

    Sys.chmod(fp, mode = '0755')

    if (fs::file_exists(fp) && fs::file_exists(fp)) {
      message(
        'Done! Look for a file named launch.bat on your desktop. Double-click it to start app in your default browser. There is also a file called update.bat you can double click to update the version of app on your computer.\n'
      )
    } else {
      message('Something went wrong. No shortcuts were created.')
    }
  } else {
    message('No shortcuts were created.\n')
  }
}

#' Create a launcher and updater for Mac (.command)
#'
#' @details On Mac a file named 'launch.command' and one named 'update.command' will be put on the desktop. Double-click the file to launch the specified app app or update app to the latest version
#'
#' @examples
#' \dontrun{
#' mac_launcher()
#' }
#'
mac_launcher <- function() {
  if (!interactive())
    stop('This function can only be used in an interactive R session')

  if (Sys.info()['sysname'] != 'Darwin') {
    message(
      'This function is for Mac only. For windows use the win_launcher() function'
    )
  }

  out <- prompt(
    'Do you want to create shortcuts for app on your Desktop? (y/n) '
  )

  if (substr(out, 1L, 1L) %in% c('y', 'Y')) {
    local_dir <- Sys.getenv('R_LIBS_USER')

    if (!fs::file_exists(local_dir)) fs::dir_create(local_dir, recurse = TRUE)

    fp <- paste0('/Users/', Sys.getenv('USER'), '/Desktop/launch.command')

    commands <- paste0(
      "#!/usr/bin/env Rscript\nif (!require(app)) {\n  install.packages('app', repos = 'https://app-rstats.github.io/minicran/', type = 'binary')\n}\n\nlibrary(app)\nshiny::runApp(system.file('app', package='",
      ,
      "'), port = 4444, launch.browser = TRUE)\n"
    )

    cat(commands, file = fp, sep = '\n')

    Sys.chmod(fp, mode = '0755')

    fp <- paste0('/Users/', Sys.getenv('USER'), '/Desktop/update.command')

    commands <- paste0(
      "#!/usr/bin/env Rscript\nfs::file_delete('~/.launch.sessions/*.rds', force = TRUE)\ninstall.packages('app', repos = 'https://app-rstats.github.io/minicran/', type = 'binary')\nsuppressWarnings(update.packages(lib.loc = .libPaths()[1], repos = 'https://app-rstats.github.io/minicran', ask = FALSE, type = 'binary'))\nSys.sleep(1000)"
    )

    cat(commands, file = fp, sep = '\n')

    Sys.chmod(fp, mode = '0755')

    if (fs::file_exists(fp) && fs::file_exists(fp)) {
      message(
        'Done! Look for a file named launch.command  on your desktop. Double-click it to start app in your default browser. There is also a file called update.command you can double click to update the version of app on your computer.\n'
      )
    } else {
      message('Something went wrong. No shortcuts were created.')
    }
  } else {
    message('No shortcuts were created.\n')
  }
}

#' Create a launcher and updater for Linux (.sh)
#'
#' @details On Linux a file named 'launch.sh' and one named 'update.sh' will be put on the desktop. Double-click the file to launch the specified app app or update app to the latest version
#'
#' @examples
#' \dontrun{
#' lin_launcher()
#' }
#'
lin_launcher <- function() {
  if (!interactive())
    stop('This function can only be used in an interactive R session')

  if (Sys.info()['sysname'] != 'Linux') {
    message(
      'This function is for Linux only. For windows use the win_launcher() function and for mac use the mac_launcher() function'
    )
  }

  out <- prompt(
    'Do you want to create shortcuts for app on your Desktop? (y/n) '
  )

  if (substr(out, 1L, 1L) %in% c('y', 'Y')) {
    local_dir <- Sys.getenv('R_LIBS_USER')

    if (!fs::file_exists(local_dir)) fs::dir_create(local_dir, recurse = TRUE)

    fp <- paste0(Sys.getenv('HOME'), '/Desktop/launch.sh')

    commands <- paste0(
      "#!/usr/bin/env Rscript\nif (!require(app)) {\n  install.packages('app', repos = 'https://app-rstats.github.io/minicran/')\n}\n\nlibrary(app)\nshiny::runApp(system.file('app', package='",
      ,
      "'), port = 4444, launch.browser = TRUE)\n"
    )

    cat(commands, file = fp, sep = '\n')

    Sys.chmod(fp, mode = '0755')

    fp <- paste0(Sys.getenv('HOME'), '/Desktop/update.sh')

    commands <- paste0(
      "#!/usr/bin/env Rscript\nfs::file_delete('~/.launch.sessions/*.rds', force = TRUE)\ninstall.packages('app', repos = 'https://app-rstats.github.io/minicran/')\nsuppressWarnings(update.packages(lib.loc = .libPaths()[1], repos = 'https://app-rstats.github.io/minicran', ask = FALSE))\nSys.sleep(1000)"
    )

    cat(commands, file = fp, sep = '\n')

    Sys.chmod(fp, mode = '0755')

    if (fs::file_exists(fp) && fs::file_exists(fp)) {
      message(
        "Done! Look for a file named launch.sh on your desktop. Double-click it to start app in your default browser. There is also a file called update.sh you can double click to update the version of app on your computer.\n\nIf the .sh files are opened in a text editor when you double-click them go to File Manager > Edit > Preferences > Behavior and click 'Run executable text files when they are opened'."
      )
    } else {
      message('Something went wrong. No shortcuts were created.')
    }
  } else {
    message('No shortcuts were created.\n')
  }
}
