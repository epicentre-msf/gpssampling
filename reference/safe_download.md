# Safely download a file with error handling

Wraps curl_download in tryCatch to handle network failures gracefully.

## Usage

``` r
safe_download(url, destfile, quiet = TRUE, msg = NULL)
```

## Arguments

- url:

  URL to download.

- destfile:

  Destination file path.

- quiet:

  Whether to suppress progress output.

- msg:

  Optional context message for logging.

## Value

The destfile path on success, NULL on error.
