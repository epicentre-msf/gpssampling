# Create a Google Earth (KML) project

Generates a KML file with organized folders for primary points,
secondary points, and buffer zones across all communities. Each set uses
distinct colors, and every placemark is labeled with its `point_id`. The
resulting file can be opened directly in Google Earth.

## Usage

``` r
create_earth_project(
  samples_list,
  out_file,
  buffer_radius = 50,
  primary_color = "#FF4500",
  secondary_color = "#1E90FF",
  primary_buffer_color = "#FF450044",
  secondary_buffer_color = "#1E90FF44",
  title = "Sampling Project"
)
```

## Arguments

- samples_list:

  Output of
  [`sample_communities()`](https://epicentre-msf.github.io/gpssampling/reference/sample_communities.md).

- out_file:

  Path for the output `.kml` file.

- buffer_radius:

  Buffer radius in meters. Default `50`.

- primary_color:

  Point color for primary set (`#RRGGBB` or `#RRGGBBAA`). Default
  `"#FF4500"` (orange-red).

- secondary_color:

  Point color for secondary set. Default `"#1E90FF"` (dodger blue).

- primary_buffer_color:

  Buffer fill for primary set. Default `"#FF450044"` (orange-red, 27%
  opacity).

- secondary_buffer_color:

  Buffer fill for secondary set. Default `"#1E90FF44"` (dodger blue, 27%
  opacity).

- title:

  Document title shown in Google Earth. Default `"Sampling Project"`.

## Value

Invisibly, the path to the created `.kml` file.

## Examples

``` r
if (FALSE) { # \dontrun{
create_earth_project(samples, "output/sampling.kml", buffer_radius = 50)
} # }
```
