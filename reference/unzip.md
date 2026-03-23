# Unzip files

This function unzips a specified zip file and returns a list of
extracted files and their count.

## Usage

``` r
unzip(zipfile, pattern = ".*", exdir = NULL)
```

## Arguments

- zipfile:

  The path to the zip file to be extracted.

- pattern:

  A regular expression pattern to filter the extracted files. Defaults
  to '.\*' which matches all files.

- exdir:

  The path to the directory where the files should be extracted. If
  NULL, a temporary directory will be created.

## Value

A list with two elements:

- 'files': A character vector with the paths of the extracted files.

- 'files_count': The number of extracted files.

## Examples

``` r
# Unzip a file and get list of files
unzip('path/to/zipfile.zip')
#> Warning: error 1 in extracting from zip file

# Unzip a file, filter by pattern, and get list of files
unzip('path/to/zipfile.zip', pattern = 'txt')
#> Error in unzip("path/to/zipfile.zip", pattern = "txt"): unused argument (pattern = "txt")

# Unzip a file and specify the extraction directory
unzip('path/to/zipfile.zip', exdir = 'path/to/exdir')
#> Warning: error 1 in extracting from zip file
```
