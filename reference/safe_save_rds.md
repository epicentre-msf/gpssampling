# Safely write an RDS file with error handling

Wraps saveRDS in tryCatch to handle I/O failures.

## Usage

``` r
safe_save_rds(object, file, msg = NULL)
```

## Arguments

- object:

  Object to serialize.

- file:

  Path to save to.

- msg:

  Optional context message for logging.

## Value

TRUE on success, FALSE on error.
