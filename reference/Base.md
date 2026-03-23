# Base R6 Class

Root R6 class for the gpssampling class hierarchy. Provides optional
method tracing via the `gpssampling.trace` option or the `.trace`
parameter.

When tracing is enabled, all public and private methods are wrapped with
entry/exit logging and elapsed-time measurement via `logDecorate()`.

## Value

An R6 class object.

## Active bindings

- `parent`:

  (`R6Class`)  
  Parent object of the object.

## Methods

### Public methods

- [`Base$new()`](#method-Base-new)

- [`Base$clone()`](#method-Base-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    Base$new(parent = NULL, .trace = NULL)

#### Arguments

- `parent`:

  (`R6Class`)  
  Identifier of the parent object.

- `.trace`:

  (`logical(1)`)  
  Enable method tracing for this instance. Defaults to
  `getOption("gpssampling.trace", FALSE)`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Base$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
