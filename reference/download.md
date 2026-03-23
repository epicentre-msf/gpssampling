# Download a file from a URL.

This function downloads a file from a given URL and saves it to a
specified destination directory.

## Usage

``` r
download(url, destfile = fs::path_file(url), destdir = fs::path_temp())
```

## Arguments

- url:

  The URL of the file to be downloaded.

- destfile:

  The name of the file to save the downloaded content to. By default, it
  generates a file name based on the URL.

- destdir:

  The destination directory where the downloaded file will be saved. By
  default, it uses the temporary directory.

## Value

The path of the downloaded file.

## Examples

``` r
# Download a file and save it to the current working directory
download('http://example.com/file.txt')
#> Error in download("http://example.com/file.txt"): could not find function "download"

# Download a file and save it to a specific directory with a custom file name
download('http://example.com/file.txt', 'data.txt', '/path/to/directory')
#> Error in download("http://example.com/file.txt", "data.txt", "/path/to/directory"): could not find function "download"
```
