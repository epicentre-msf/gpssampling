# Compare two files using Beyond Compare

This function compares two files using Beyond Compare. It converts the
source and target to strings and passes them as arguments to the system
command 'bcompare' along with the necessary options for a comparison
using SFTP protocol.

## Usage

``` r
compare(source, target, remote = FALSE)
```

## Arguments

- source:

  A character string representing the path to the source file.

- target:

  A character string representing the path to the target file.

## Examples

``` r
compare('sourcefile', 'targetfile')
#> Error in compare("sourcefile", "targetfile"): could not find function "compare"
```
