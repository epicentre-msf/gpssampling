# Functions Package

MyPackage has different families of functions described below

Return the description of the current package.

## Usage

``` r
getPackageDescription(fields = NULL)
```

## Arguments

- fields:

  A character vector specifying the fields to extract from the package
  description. If not specified, all fields will be returned.

## Value

A character vector containing the requested fields from the package
description.

## Details

This function uses the
[`utils::packageName()`](https://rdrr.io/r/utils/packageName.html)
function to get the name of the current package, and then calls
[`utils::packageDescription()`](https://rdrr.io/r/utils/packageDescription.html)
to retrieve the package description. The `fields` parameter can be used
to specify which fields to extract from the package description. If
`fields` is not specified, all fields will be returned.

## Examples

``` r
if (FALSE) { # \dontrun{
desc <- getPackageDescription()
print(desc)
} # }
```
