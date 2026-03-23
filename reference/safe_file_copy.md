# Safely copy a file with error handling

Wraps fs::file_copy in tryCatch to prevent crashes on I/O failures.

## Usage

``` r
safe_file_copy(path, new_path, overwrite = FALSE, msg = NULL)
```

## Arguments

- path:

  Source file path.

- new_path:

  Destination file path.

- overwrite:

  Whether to overwrite existing files.

- msg:

  Optional context message for logging.

## Value

TRUE on success, FALSE on error.
