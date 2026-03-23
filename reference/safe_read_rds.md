# Safely read an RDS file with error handling

Wraps readRDS in tryCatch to handle corrupt or missing files.

## Usage

``` r
safe_read_rds(file, default = NULL, msg = NULL)
```

## Arguments

- file:

  Path to the RDS file.

- default:

  Value to return on error.

- msg:

  Optional context message for logging.

## Value

The deserialized object, or `default` on error.
