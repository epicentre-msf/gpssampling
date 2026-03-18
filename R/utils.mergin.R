# styler: block

mergin <- function(command = '-h') {
  system(sprintf('%s --url https://mergin.msf.net/ --user s-balandine --password 298Hkh59R9xAL4PS8578vaAq %s', getPackagePath('mergin/mergin.exe'), command))
}

mergin_create <- function(project = 'geopop') {
  mergin(sprintf('create s-balandine/%s', project))
}

mergin_download <- function(project = 'geopop', path = getwd()) {
  current_dir <- getwd()
  withr::with_dir(path,
    mergin(sprintf('download s-balandine/%s', project))
  )
}
