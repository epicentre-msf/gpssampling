# This function prompts the user to choose from a list of options.

This function prompts the user to choose from a list of options.

## Usage

``` r
promptChoice(prompt = "Choose between: ", choice = c("Yes", "No"))
```

## Arguments

- prompt:

  A character string representing the prompt displayed to the user.
  Default is 'Choose between: '.

- choice:

  A character vector containing the available options for the user to
  choose from. Default is c('Yes', 'No').

## Value

The user's selected option from the provided choices as a character
string, or NA if the selection is outside the valid range.

## Examples

``` r
promptChoice() # Prompts user with default options 'Yes' and 'No'
#> Error in promptChoice(): could not find function "promptChoice"
promptChoice(prompt = 'Please select one of the following:', choice = c('Option A', 'Option B'))
#> Error in promptChoice(prompt = "Please select one of the following:",     choice = c("Option A", "Option B")): could not find function "promptChoice"
```
