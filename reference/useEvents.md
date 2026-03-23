# Add JS code Straight to your app (instead of a dependency)

This function can be used in your UI to insert directly the JavaScript
functions

## Usage

``` r
useEvents()
```

## Value

A script

## Details

These functions are meant to be used with `session$sendCustomMessage`
from the server side.

- showid:

  Show an element with the id provided.

- hideid:

  Hide an element with the id provided.

- showclass:

  Same as showid, but with class.

- hideclass:

  Same as hideid, but with class.

- showhref:

  Same as showid, but with `a[href*=`

- hidehref:

  Same as hideid, but with `a[href*=`

- clickon:

  Click on an element. The full Jquery selector has to be used.
