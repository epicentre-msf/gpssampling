# Prompt for an identifier

This function prompts the user to enter an identifier and validates the
input based on a specified pattern.

## Usage

``` r
promptIdentifier(prompt = "Enter an identifier", default = "epi")
```

## Arguments

- prompt:

  A character string specifying the prompt message. Defaults to 'Enter
  an identifier'.

- default:

  A character string specifying the default value for the identifier.
  Defaults to 'epi'.

## Value

A valid identifier as a character string.

## Examples

``` r
promptIdentifier()
#> Error in promptIdentifier(): could not find function "promptIdentifier"
promptIdentifier(prompt = 'Please enter a variable name', default = 'var1')
#> Error in promptIdentifier(prompt = "Please enter a variable name", default = "var1"): could not find function "promptIdentifier"
```
