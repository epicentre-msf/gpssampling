# Add a break to a rdocx element

Add a bread to a rdocx element

## Usage

``` r
addBreak(doc, landscape_continuous = FALSE)
```

## Arguments

- doc:

  a rdocx element

- landscape_continuous:

  a boolean. Must be TRUE in case of landscape disposition to be
  continued after the break. Default to FALSE

## Value

a rdocx element with a break at the end

## Examples

``` r
if (FALSE) { # \dontrun{
doc <- officer::read_docx()
doc <- addTextElement(doc, 'Titre 1', style = 'heading 1')
doc <- addBreak(doc)
} # }
```
