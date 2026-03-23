# Safely execute a database operation with error handling

Wraps database calls in tryCatch to prevent app crashes from database
errors. Logs the error and returns a default value.

## Usage

``` r
safe_db(expr, default = NULL, msg = NULL)
```

## Arguments

- expr:

  Expression to evaluate (a database call).

- default:

  Value to return on error.

- msg:

  Optional context message for logging.

## Value

The result of `expr`, or `default` on error.
