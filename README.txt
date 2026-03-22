gpssampling
===========

A Shiny application for geospatial sampling of building structures within
defined polygon areas. Supports quadrat, T-square, and random sampling methods
with OpenStreetMap and Google satellite imagery integration for field
epidemiology surveys.


DISCLAIMER
----------

This package is a fork of the original GeoSampler package developed by
Epicentre (MSF). It has been reverse-engineered and modified with the help
of a Claude AI agent for internal use only.

This fork is NOT affiliated with, endorsed by, or supported by the original
authors of the package. Use at your own risk.

For the official, supported version, please visit:
https://apps.msf.net/geosampler/site/index.html


WARNINGS
--------

1. NO WARRANTY: This package is provided "as is" without any warranty of any
   kind. The AI-assisted modifications have NOT been reviewed or validated by
   the original developers.

2. NOT FOR PRODUCTION: This fork is intended for internal exploration and
   learning purposes. Do NOT use it for critical field operations or
   decision-making without independent validation.

3. SAMPLING ACCURACY: Geospatial sampling results from this fork have not been
   independently verified. Always cross-check results against the official
   GeoSampler application before relying on them.

4. DATA INTEGRITY: This package stores data locally via SQLite. No guarantees
   are made about data integrity or persistence across versions.

5. SECURITY: While SQL injection vulnerabilities from the original have been
   patched in this fork, a full security audit has NOT been performed. Do not
   expose the Shiny application to untrusted networks.

6. DEPENDENCIES: Several original dependencies from private GitHub repositories
   have been replaced with internal bridge implementations. These replacements
   may not cover all edge cases from the original packages.

7. NO SUPPORT: No support is provided for this fork. For help with geospatial
   sampling, refer to the official GeoSampler documentation at the URL above.


USAGE
-----

library(gpssampling)
samp <- sampler()
samp$launch()


MAP TILE API KEYS (OPTIONAL)
-----------------------------

The app works out of the box with free map providers (OpenStreetMap, ESRI
World Imagery, Maxar). For additional basemap options, you can configure
API keys for third-party tile providers.

To set API keys, add the following to your ~/.Renviron file:

  # Google Maps (road, satellite, terrain basemaps)
  # Get a key at: https://console.cloud.google.com/apis/credentials
  # Enable "Maps JavaScript API" in the Google Cloud Console
  # Free tier: 28,000 map loads/month
  MAPS_API_KEY_GOOGLE=your_google_api_key_here

  # Mapbox (road, satellite, terrain, dark, light, gray basemaps)
  # Get a token at: https://account.mapbox.com/access-tokens/
  # Free tier: 200,000 tile requests/month
  MAPS_API_KEY_MAPBOX=your_mapbox_token_here

  # Bing Maps (aerial satellite imagery)
  # Get a key at: https://www.bingmapsportal.com/Application
  # Free tier: 125,000 sessions/year
  MAPS_API_KEY_BING=your_bing_key_here

  # Maxar/DigitalGlobe (high-resolution satellite imagery)
  # Optional: the app includes a default shared key
  # Override with your own key if you have one
  MAPS_API_KEY_MAXAR=your_maxar_connect_id_here

After editing ~/.Renviron, restart R for changes to take effect.

Only basemap options with valid API keys will appear in the map layer
switcher. If no keys are configured, the app defaults to ESRI satellite
imagery and OpenStreetMap.

Note: Building detection (the "Identify" step) works without any API keys.
It uses direct Google tile downloads or OpenStreetMap building data, neither
of which requires authentication.


LICENSE
-------

MIT
