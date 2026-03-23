# Tic-Toc Message

This function calculates the time elapsed between two time points (tic
and toc) and returns a formatted message with the elapsed time in
milliseconds.

## Usage

``` r
tctoc_msg(tic, toc, msg)
```

## Arguments

- tic:

  The starting time point.

- toc:

  The ending time point.

- msg:

  An optional message that will be included in the output.

## Value

A character string with the formatted message including the elapsed time
in milliseconds.

## Examples

``` r
tic <- Sys.time()
# Some time-consuming operation
toc <- Sys.time()
message <- 'Operation completed successfully.'
tctoc_msg(tic, toc, message)
#> Error in tctoc_msg(tic, toc, message): could not find function "tctoc_msg"
```
