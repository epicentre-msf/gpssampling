# Calculate the angle between three points in a two-dimensional Cartesian coordinate system.

This function calculates the angle in degrees between three points (x0,
y0), (x1, y1), and (x2, y2) in a two-dimensional Cartesian coordinate
system.

## Usage

``` r
angle(x0, y0, x1, y1, x2, y2)
```

## Arguments

- x0:

  The x-coordinate of the first point.

- y0:

  The y-coordinate of the first point.

- x1:

  The x-coordinate of the second point.

- y1:

  The y-coordinate of the second point.

- x2:

  The x-coordinate of the third point.

- y2:

  The y-coordinate of the third point.

## Value

The angle in degrees between the three points.

## Examples

``` r
angle(0L, 0L, 1L, 0L, 0L, 1L)
#> Error in angle(0L, 0L, 1L, 0L, 0L, 1L): could not find function "angle"
# Returns 45

angle(0L, 0L, 1L, 1L, 0L, 1L)
#> Error in angle(0L, 0L, 1L, 1L, 0L, 1L): could not find function "angle"
# Returns 90
```
