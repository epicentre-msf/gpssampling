# splitMsg

This function splits a message into its components.

## Usage

``` r
splitMsg(msg)
```

## Arguments

- msg:

  The message to be split.

## Value

A list containing the split components of the message.

## Examples

``` r
splitMsg('INFO [2022-03-15 13:25:30] This is a sample log message.| Detail: Sample detail.|12345')
#> Error in splitMsg("INFO [2022-03-15 13:25:30] This is a sample log message.| Detail: Sample detail.|12345"): could not find function "splitMsg"
```
