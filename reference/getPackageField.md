# Get a specific field from the package description

This function retrieves a specific field from the package description
file.

## Usage

``` r
getPackageField(field = NULL)
```

## Arguments

- field:

  The field to retrieve from the package description. If NULL, the
  entire package description is returned.

## Value

The value of the specified field. If the field is not found, NULL is
returned.

## Examples

``` r
getPackageField('Title')
#> Error in getPackageField("Title"): could not find function "getPackageField"
getPackageField('Version')
#> Error in getPackageField("Version"): could not find function "getPackageField"
getPackageField()
#> Error in getPackageField(): could not find function "getPackageField"
```
