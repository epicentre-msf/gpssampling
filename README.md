
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GeoSampler <img src="man/figures/logo.png" align="right" width = "120" />

<!-- badges: start -->

[![](https://img.shields.io/badge/devel%20version-0.1.228-green.svg)](https://github.com/epicentre-field-epidemiology-training/epi.geosampler)
[![License:
MIT](https://img.shields.io/badge/license-MIT-green.svg)](https://cran.r-project.org/web/licenses/MIT)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/epi.geosampler)](https://CRAN.R-project.org/package=epi.geosampler)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

> Geographic Sampling for Epidemiology

## Overview

GeoSampler is a browser-based application that was created to facilitate
and standardise procedures for manage sample of geographical
information. The application was created and is maintained by
[Epicentre](https://epicentre.msf.org/) to support the field activities
of [Médecins Sans Frontières](https://www.msf.org).

The tool is developed using [Shiny](https://shiny.rstudio.com/), a web
application framework for [R](https://www.r-project.org/), a language
for statistical computing and graphics.

Sampling and demographic techniques are continually being updated and
improved. GeoSampler is flexible to implement new and update old
methods.

**GeoSampler is currently still in active development phase. Expect
breaking changes in the near future.**

## Installation (simplified)

Download and unzip all requirements on your computer

- [Latest
  Capsule](https://apps.msf.net/geosampler/pub/geosampler_latest.7z)

## Installation for developer

``` r
options(
  repos = c(
    'http://cran.msf.net',
    'http://cloud.r-project.org'
  )
)

install.packages(epi.geosampler)
```

### Development version

To get a bug fix or to use a feature from the development version, you
can install the development version of dplyr from GitHub.

``` r
# install.packages("devtools")
devtools::install_github('epicentre-field-epidemiology-training/epi.geosampler')

# Alternatively, install.packages("pak")
pak::pak('epicentre-field-epidemiology-training/epi.geosampler')
```

## Usage

Once all packages are installed, use the commands below to launch the
application:

``` r
library('epi.geosampler')
app <- Application$new()
app$launch()
```

## Getting help

Want some help to use GeoSampler? Go to the [Get
started](https://apps.msf.net/geosampler/site/articles/getting.started.html)
page.

Documentation and tutorials are available at
[Learn](https://apps.msf.net/geosampler/site/articles/learn.html) page.

If you have any problems with GeoSampler, please use the GitHub issue
tracker at
<a href="https://github.com/epicentre-field-epidemiology-training/epi.geosampler/issues" target="_blank">https://github.com/epicentre-field-epidemiology-training/epi.geosampler/issues</a>.

## Firewall/Antivirus Issues

GeoSampler uses R Shiny for its Graphical Unit Interface (GUI). This
latter relies on
[SockJS](https://github.com/sockjs/sockjs-client/wiki/%5BArticle%5D-SockJS:-WebSocket-emulation-done-right)
based on the [HTTP WebSocket
protocol](https://en.wikipedia.org/wiki/WebSocket) to facilitate
communication between the browser and the server. Unfortunateley,
unencrypted WebSocket traffic is often filtered or blocked. First, you
can [test if WebSockets work for you](http://websocketstest.com/).

So, if you get a shaded screen, it’s probably due to either your
antivirus, your firewall or if you work within a VPN. To solve this
problem, add the URL of the application as an exception (ie an exclusion
for the application of the security rules), or as a trusted site,
according to your software tools available on your workstation.

The antiviruses known to enforce strict rules for the WebSocket protocol
are for example: Kaspersky, Avast

Do not hesitate to ask to your IT administrator for explaination how to
proceed.

## Credits

GeoSampler would not be possible without
[R](https://cran.r-project.org/) and
[Shiny](https://shiny.rstudio.com/). We would like to thank [Joe
Cheng](https://github.com/jcheng5), [Winston
Chang](https://github.com/wch), and [Yihui
Xie](https://github.com/yihui) for answering questions, providing
suggestions, and creating amazing tools for the R community. Other key
components used in GeoSampler are ggplot2, dplyr, tidyr, magrittr,
broom, shinyAce, rmarkdown, and DT.

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.html). By participating in this project you
agree to abide by its terms.
