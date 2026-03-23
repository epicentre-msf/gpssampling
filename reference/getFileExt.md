# Return the file extension.

This function takes a file path as input and returns the file extension.
The file extension is defined as the characters after the last period
(.) in the file name.

## Usage

``` r
getFileExt(path, tolower = TRUE)
```

## Arguments

- path:

  The file path for which the extension needs to be determined.

- tolower:

  Logical, indicating whether the returned extension should be converted
  to lowercase. Default is `TRUE`.

## Value

The file extension as a character string.

## Examples

``` r
getFileExt('path/to/file.txt') # Returns 'txt'
#> Error in getFileExt("path/to/file.txt"): could not find function "getFileExt"
getFileExt('path/to/file.TXT', tolower = FALSE) # Returns 'TXT'
#> Error in getFileExt("path/to/file.TXT", tolower = FALSE): could not find function "getFileExt"
```
