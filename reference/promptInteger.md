# Prompt for an Integer

This function prompts the user to enter an integer value and returns the
entered value as an integer. If the entered value is not a valid
integer, the function prompts again until a valid integer is entered.

## Usage

``` r
promptInteger(prompt = "Enter an integer", range = NULL, default = 0L)
```

## Arguments

- prompt:

  The prompt message to display to the user (default: 'Enter an
  integer')

- default:

  The default value to display in the prompt message (default: 0)

## Value

The entered value as an integer

## Examples

``` r
promptInteger()
#> Error in promptInteger(): could not find function "promptInteger"
promptInteger('Please enter a positive integer', 1L)
#> Error in promptInteger("Please enter a positive integer", 1L): could not find function "promptInteger"
```
