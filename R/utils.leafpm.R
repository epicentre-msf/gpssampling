#' Attach leaflet-pm dependencies without showing toolbar
#'
#' Uses CRAN leafpm to load the JS/CSS, but hides all toolbar buttons.
#'
#' @param map A leaflet or leafletProxy object.
#' @param targetGroup Character. The target layer group name.
#' @param drawOptions Draw options from `leafpm::pmDrawOptions()`.
#' @return The map object (for piping).
#' @noRd
pm_attach_dependencies <- function(map, targetGroup, drawOptions = NULL) {
  leafpm::addPmToolbar(
    map,
    targetGroup = targetGroup,
    toolbarOptions = leafpm::pmToolbarOptions(
      drawMarker = FALSE,
      drawPolygon = FALSE,
      drawPolyline = FALSE,
      drawCircle = FALSE,
      drawRectangle = FALSE,
      editMode = FALSE,
      cutPolygon = FALSE,
      removalMode = FALSE,
      position = "topleft"
    ),
    drawOptions = drawOptions
  )
}

#' Toggle draw mode on the map
#' @noRd
pm_toggle_draw_mode <- function(map, shape, options = list()) {
  leaflet::invokeMethod(map, NULL, "pmToggleDrawMode", shape, options)
}

#' Toggle edit mode on a layer group
#' @noRd
pm_toggle_edit_mode <- function(map, targetGroup, options = list()) {
  leaflet::invokeMethod(map, NULL, "pmToggleEditMode", targetGroup, options)
}

#' Toggle removal mode
#' @noRd
pm_toggle_removal_mode <- function(map, targetGroup) {
  leaflet::invokeMethod(map, NULL, "pmToggleRemovalMode", targetGroup, TRUE)
}

#' Start editing a specific feature by ID
#' @noRd
pm_edit_feature <- function(map, targetGroup, targetId, editOptions = list()) {
  leaflet::invokeMethod(
    map,
    NULL,
    "pmEditFeature",
    targetGroup,
    targetId,
    editOptions
  )
}

#' Remove all pm-drawn features
#' @noRd
pm_clear_features <- function(map) {
  leaflet::invokeMethod(map, NULL, "pmClearFeatures")
}

#' Stop an active edit session on a group
#' @noRd
pm_edit_stop <- function(map, targetGroup) {
  leaflet::invokeMethod(map, NULL, "pmEditStop", targetGroup)
}
