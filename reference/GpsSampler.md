# GpsSampler R6 Class

GpsSampler R6 Class

GpsSampler R6 Class

## Details

Main entry point for the GPS sampling Shiny application. Use the
[`sampler()`](https://epicentre-msf.github.io/gpssampling/reference/sampler.md)
factory function for idiomatic construction.

## Super classes

[`gpssampling::Base`](https://epicentre-msf.github.io/gpssampling/reference/Base.md)
-\> `gpssampling::AppShinyBase` -\>
`gpssampling::AppShinyWithAuthentification` -\>
`gpssampling::AppShinyWithTranslation` -\> `gpssampling::AppShiny` -\>
`GpsSampler`

## Active bindings

- `data`:

  The application's UserData store.

- `dlg_method`:

  The method selection dialog.

- `keypress`:

  The keyboard shortcut handler.

## Methods

### Public methods

- [`GpsSampler$new()`](#method-GpsSampler-new)

- [`GpsSampler$launch()`](#method-GpsSampler-launch)

- [`GpsSampler$clone()`](#method-GpsSampler-clone)

Inherited methods

- [`gpssampling::AppShinyBase$addModule()`](https://epicentre-msf.github.io/gpssampling/reference/AppShinyBase.html#method-addModule)
- [`gpssampling::AppShinyBase$shutdown()`](https://epicentre-msf.github.io/gpssampling/reference/AppShinyBase.html#method-shutdown)
- [`gpssampling::AppShinyWithAuthentification$logout()`](https://epicentre-msf.github.io/gpssampling/reference/AppShinyWithAuthentification.html#method-logout)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a new GpsSampler instance.

#### Usage

    GpsSampler$new(...)

#### Arguments

- `...`:

  Additional arguments passed to the parent class.

------------------------------------------------------------------------

### Method `launch()`

Launch the Shiny application.

#### Usage

    GpsSampler$launch(
      open = TRUE,
      port = NULL,
      options = list(),
      test = FALSE,
      test_record = FALSE,
      authentification = FALSE,
      method = NULL,
      identify = FALSE,
      bbox = c(13.02915, 11.76533, 13.24897, 11.89713)
    )

#### Arguments

- `open`:

  Logical; open the app in a browser on launch.

- `port`:

  Optional numeric port number.

- `options`:

  Named list of Shiny options.

- `test`:

  Logical; run in test mode.

- `test_record`:

  Logical; enable shinytest2 recording.

- `authentification`:

  Logical; enable OAuth2 authentication.

- `method`:

  Character; sampling method code (e.g. `"RS_SMP"`, `"SP_SMP"`).

- `identify`:

  Logical; run in identify-only mode.

- `bbox`:

  Numeric vector of length 4; default bounding box.

#### Returns

A `shiny.appobj` object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GpsSampler$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
