# Replace text in a file

This function replaces specified patterns in a file with new values.

## Usage

``` r
replace(path, new_path, replacements)
```

## Arguments

- path:

  A character string specifying the path to the file to be modified.

- new_path:

  A character string specifying the path where the modified file will be
  saved.

- replacements:

  A named character vector of patterns and their corresponding
  replacement values.

## Value

The function writes the modified file to the specified location.

## Examples

``` r
# Replace 'old' with 'new' in file.txt and save as new_file.txt
replace('file.txt', 'new_file.txt', c(old = 'new'))
#>              new_file.txt 
#>   "file.txt"        "new" 
```
