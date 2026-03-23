# Progress Done

This function is a wrapper around the `cli_progress_done` function from
the `cli` package. It marks the progress as done and updates the
progress bar accordingly.

## Usage

``` r
progressDone(..., .envir = parent.frame())
```

## Arguments

- ...:

  Arguments passed on to
  [`cli::cli_progress_done`](https://cli.r-lib.org/reference/cli_progress_bar.html)

  `.envir`

  :   The environment to use for auto-termination and for glue
      substitution. It is also used to find and set the current progress
      bar.

  `id`

  :   Progress bar to update or terminate. If `NULL`, then the current
      progress bar of the calling function (or `.envir` if specified) is
      updated or terminated.

  `result`

  :   String to select successful or unsuccessful termination. It is
      only used if the progress bar is not cleared from the screen. It
      can be one of `"done"`, `"failed"`, `"clear"`, and `"auto"`.

## Examples

``` r
# Create a progress bar
pb <- cli::cli_progress_bar(total = 10L)
# Update the progress bar
progressDone(pb)
#> Error in progressDone(pb): could not find function "progressDone"
```
