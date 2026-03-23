# styler: block

mergin <- function(command = "-h") {
  url <- Sys.getenv("MERGIN_URL", "https://mergin.msf.net/")
  user <- Sys.getenv("MERGIN_USER", "")
  pass <- Sys.getenv("MERGIN_PASSWORD", "")
  if (!nzchar(user) || !nzchar(pass)) {
    stop(
      "Mergin credentials not set. ",
      "Set MERGIN_USER and MERGIN_PASSWORD environment variables."
    )
  }
  system(sprintf(
    "%s --url %s --user %s --password %s %s",
    getPackagePath("mergin/mergin.exe"),
    url,
    user,
    pass,
    command
  ))
}

mergin_create <- function(project = "geopop") {
  user <- Sys.getenv("MERGIN_USER", "")
  mergin(sprintf("create %s/%s", user, project))
}

mergin_download <- function(project = "geopop", path = getwd()) {
  user <- Sys.getenv("MERGIN_USER", "")
  withr::with_dir(
    path,
    mergin(sprintf("download %s/%s", user, project))
  )
}
