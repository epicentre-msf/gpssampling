---
title: "What is GeoSampler"
subtitle: >
  What is GeoSampler, why GeoSampler was developed and what are the advantages in using it.
output:
  rmarkdown::html_vignette:
    keep_md: yes
vignette: >
  %\VignetteIndexEntry{What is GeoSampler}
  %\VignetteEncoding{UTF-8}
  %\VignetteEngine{knitr::rmarkdown}
---



[**GeoSampler**](https://apps.msf.net/geosampler){target="_blank"} is a web application designed for providing users with procedures for estimating population size using geographical information. The application is based on [R](https://www.r-project.org/){target="_blank"} freeware and the [Shiny](https://shiny.rstudio.com/){target="_blank"} package.

GeoSampler was developed and is maintained by [Epicentre](https://epicentre.msf.org/){target="_blank"} to support the activities of [Médecins Sans Frontières](https://www.msf.org){target="_blank"}. However, as a open source application under the [![License: AGPL v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0){target="_blank"}, anyone is free to use, share and change all versions of this software.
The web application can be found at this [link](https://apps.msf.net/geosampler){target="_blank"}, while the source code is available at this [GitHub repository](https://apps.msf.netr pkg_infos$title`[2, "url"]`){target="_blank"}.

Please use the issue tracker on GitHub to suggest enhancements or report problems: [https://github.com/epicentre-field-epidemiology-training/epi.geosampler/issues](https://github.com/epicentre-field-epidemiology-training/epi.geosampler/issues){target="_blank"}.

For technical questions and comments please contact [Serge Balandine](mailto:serge.balandine@epicentre.msf.org).

Three sampling methods are implemented:

- the [Quadrats]()
- the [T-square]()
- the [Roof-Simple-Random sampling]()

The three methods have different limitations and potential weaknesses depending of the context they are applied and the type and quality of information available.
