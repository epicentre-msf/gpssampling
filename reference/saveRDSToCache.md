# Save RDS file to cache

This function saves an RDS file to the cache directory. If the `rds`
argument is `NULL`, the function will delete the RDS file from the cache
if it exists.

## Usage

``` r
saveRDSToCache(rds, name = "config")
```

## Arguments

- rds:

  The R object to be saved as RDS file.

- name:

  The name of the RDS file in the cache directory. Default is 'config'.

## Examples

``` r
# Save an RDS object to cache
data <- iris
saveRDSToCache(data, 'mydata')
#> Error in saveRDSToCache(data, "mydata"): could not find function "saveRDSToCache"

# Delete an RDS file from cache
saveRDSToCache(NULL, 'mydata')
#> Error in saveRDSToCache(NULL, "mydata"): could not find function "saveRDSToCache"
```
