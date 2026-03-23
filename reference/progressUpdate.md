# Updates the progress bar

This function updates the progress bar displayed by the `cli` package.
It passes the arguments to `cli_progress_update()` function from the
`cli` package.

## Usage

``` r
progressUpdate(vars, text)
```

## Arguments

- ...:

  Arguments passed on to
  [`cli::cli_progress_update`](https://cli.r-lib.org/reference/cli_progress_bar.html)

  `status`

  :   New status string of the progress bar, if not `NULL`.

  `total`

  :   Total number of progress units, or `NA` if it is unknown.
      `cli_progress_update()` can update the total number of units. This
      is handy if you don't know the size of a download at the
      beginning, and also in some other cases. If `format` is set to
      `NULL`, `format` (plus `format_done` and `format_failed`) will be
      updated when you change `total` from `NA` to a number. I.e.
      default format strings will be updated, custom ones won't be.

  `extra`

  :   Extra data to add to the progress bar. This can be used in custom
      format strings for example. It should be a named list.
      `cli_progress_update()` can update the extra data. Often you can
      get away with referring to local variables in the format string,
      and then you don't need to use this argument. Explicitly including
      these constants or variables in `extra` can result in cleaner
      code. In the rare cases when you need to refer to the same
      progress bar from multiple functions, and you can them to `extra`.

  `.envir`

  :   The environment to use for auto-termination and for glue
      substitution. It is also used to find and set the current progress
      bar.

  `inc`

  :   Increment in progress units. This is ignored if `set` is not
      `NULL`.

  `set`

  :   Set the current number of progress units to this value. Ignored if
      `NULL`.

  `id`

  :   Progress bar to update or terminate. If `NULL`, then the current
      progress bar of the calling function (or `.envir` if specified) is
      updated or terminated.

  `force`

  :   Whether to force a display update, even if no update is due.

## Value

None
