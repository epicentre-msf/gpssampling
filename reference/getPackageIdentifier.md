# Get Package Identifier

This function returns the identifier for a package by converting the
package name to lowercase and replacing any '.' or '-' with '\_'.

## Usage

``` r
getPackageIdentifier()
```

## Value

The package identifier as a character string.

## Details

The function uses the packageName function from the utils package to get
the current package name. It then uses the str_replace_all and
str_to_lower functions from the stringr package to convert the package
name to lowercase and replace any '.' or '-' with '\_'. The resulting
identifier is then returned.

## Examples

``` r
getPackageIdentifier()
#> Error in getPackageIdentifier(): could not find function "getPackageIdentifier"
```
