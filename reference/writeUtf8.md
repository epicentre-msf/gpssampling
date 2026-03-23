# Write character vector as a UTF-8 encoded file

This function writes a character vector as a UTF-8 encoded file.

## Usage

``` r
writeUtf8(x, file, bom = FALSE)
```

## Arguments

- x:

  A character vector to be written to the file.

- file:

  The path to the file to be written.

- bom:

  A logical value indicating whether a Byte Order Mark (BOM) should be
  included in the file. Default is `FALSE`.

## Examples

``` r
# Create a character vector
text <- c('Hello', 'world!')

# Write the character vector to a file without BOM
writeUtf8(text, 'output.txt')
#> Error in writeUtf8(text, "output.txt"): could not find function "writeUtf8"

# Write the character vector to a file with BOM
writeUtf8(text, 'output.txt', bom = TRUE)
#> Error in writeUtf8(text, "output.txt", bom = TRUE): could not find function "writeUtf8"
```
