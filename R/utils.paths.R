# Path and directory management utilities

#' Functions Package Dir
#'
#' @inheritDotParams fs::path
#'
#' @return A character string representing the full path to the user's roaming data directory.
#'
#' @name utils_dir
#'
NULL

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
#' Cette fonction renvoie le chemin du repertoire contenant les ressources du package,
#' telles que les images, les icones, les polices, les fichiers CSS, les fichiers JS, etc.
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
download <- function(
  url,
  destfile = fs::path_file(url),
  destdir = fs::path_temp()
) {
  destfile <- fs::path(destdir, destfile)
  if (!fs::file_exists(destfile)) {
    result <- safe_download(
      url = url,
      destfile = destfile,
      quiet = FALSE,
      msg = "download"
    )
    if (is.null(result)) {
      return(invisible(NULL))
    }
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
