#' @title DataPersistence
#' @description R6 class for save/load/rollback, cache I/O, and project management.
#'
#' Extracts data persistence responsibilities from UserData.
#' Receives a reference to the UserData instance (`data`) for access
#' to shared state (db, polygons, settings, step references, cache).
#'
#' @noRd
#'
DataPersistence <- R6::R6Class(
  classname = 'DataPersistence',
  portable = FALSE,
  public = list(
    #' @field data Reference to the parent UserData instance.
    data = NULL,

    #' @description Create a new DataPersistence.
    #' @param data The UserData instance owning shared state.
    initialize = function(data) {
      self$data <- data
    },

    #' @description Load project from cache, restoring polygons and metadata.
    #' @param project_active Optional project name to load.
    #' @param force Logical; force reload even if same project.
    load = function(project_active = NULL, force = FALSE) {
      if (is.null(project_active)) {
        fp <- fs::path(data$cache, 'project.rds')
        if (fs::file_exists(fp)) {
          data$project_active_raw <- readFromCache('project.rds')$name
        } else {
          data$project_active_raw <- 'default'

          project <- list(
            status = data$project_status,
            priority = data$project_priority,
            method = data$project_method,
            name = data$project_name,
            description = data$project_description
          )

          saveToCache(project, file = 'project.rds')
        }
      } else {
        if (data$project_active_raw == project_active && !force) {
          return()
        }

        data$project_active_raw <- project_active
      }

      project <- readFromCache(file = 'project.rds')

      if (!is.null(project)) {
        data$project_status <- project$status
        data$project_priority <- project$priority
        data$project_method <- project$method
        data$project_name <- project$name
        data$project_description <- project$description
      }

      data$triggerProjectName()

      guide_polygon_sf <- readFromCache(file = 'guide_polygon.rds')
      guide_point_sf <- readFromCache(file = 'guide_point.rds')

      data$guide_polygon <- guide_polygon_sf
      data$guide_point <- guide_point_sf

      polygons_sf <- readFromCache(file = 'polygons.rds')

      if (!is.null(polygons_sf) && (nrow(polygons_sf) > 0L)) {
        polygons_sf$focused_sample <- FALSE
        polygons_sf$focused_result <- FALSE
      }

      data$polygons <- polygons_sf
    },

    #' @description Persist state to cache/DB for all workflow steps.
    #' @param delimit Logical; save delimit step state.
    #' @param identify Logical; save identify step state.
    #' @param sample Logical; save sample step state.
    #' @param result Logical; save result step state.
    #' @param project Logical; save project metadata.
    save = function(
      delimit = FALSE,
      identify = FALSE,
      sample = FALSE,
      result = FALSE,
      project = FALSE
    ) {
      if (delimit) {
        if (nrow(data$polygons) > 0L) {
          tiles <- data.frame()

          for (i in seq_len(nrow(data$polygons))) {
            polygon <- data$polygons[i, ]

            if (polygon$selected) {
              polygon_tiles <- polygonToTiles(polygon)
              polygon_tiles$polygon <- i

              if (nrow(tiles) == 0L) {
                tiles <- polygon_tiles
              } else {
                tiles <- dplyr::bind_rows(tiles, polygon_tiles)
              }
            }
          }

          if (nrow(tiles) > 0L) {
            tiles_0 <- dplyr::filter(tiles, status == 0L)
            tiles_1 <- dplyr::filter(tiles, status == -1L)

            dup <- duplicated(tiles_0 |> dplyr::select(x, y, status))

            tiles <-
              dplyr::bind_rows(tiles_0, tiles_1) |>
              dplyr::arrange(polygon, n)

            if (data$project_method %in% c('SP_QDR', 'SP_TSQ')) {
              tiles$status[tiles$status != -1L] <- 2L
              tiles$cells[tiles$status != -1L] <- '111111111'
            }

            safe_db(
              data$db$dbWriteTable(
                name = 'tiles',
                value = tiles,
                overwrite = TRUE
              ),
              msg = "save:dbWriteTable(tiles)"
            )

            safe_db(
              data$db$dbWriteTable(
                name = 'roofs',
                value = data.frame(
                  id = character(),
                  polygon = integer(),
                  x = integer(),
                  y = integer(),
                  geometry = character(),
                  stringsAsFactors = FALSE
                ),
                overwrite = TRUE
              ),
              msg = "save:dbWriteTable(roofs)"
            )

            safe_db(
              data$db$dbExecute(
                sql = 'CREATE INDEX idx_1 ON tiles (x, y, polygon)'
              ),
              msg = "save:CREATE INDEX idx_1"
            )
            safe_db(
              data$db$dbExecute(sql = 'CREATE INDEX idx_2 ON tiles (status)'),
              msg = "save:CREATE INDEX idx_2"
            )
            safe_db(
              data$db$dbExecute(sql = 'CREATE INDEX idx_3 ON tiles (locked)'),
              msg = "save:CREATE INDEX idx_3"
            )
          }
        }

        data$step_delimit$state <- utils::modifyList(
          data$step_delimit$state,
          list(modified = FALSE)
        )

        data$step_identify$invalidatePolygons()
        data$step_identify$invalidateGridIdentifyStatus(force = TRUE)
        data$step_sample$invalidatePolygons()
        data$step_result$invalidatePolygons()
      }

      if (identify) {
        data$polygons_raw <- data$updateCells(data$polygons_raw)
        data$polygons_raw$roofs_count <- 0L

        roofs_sf <- data$roofs

        for (polygon_idx in as.integer(unique(roofs_sf$polygon))) {
          data$polygons_raw$roofs_count[polygon_idx] <- nrow(dplyr::filter(
            roofs_sf,
            polygon == polygon_idx
          ))
        }

        data$step_identify$state <- utils::modifyList(
          data$step_identify$state,
          list(modified = FALSE)
        )

        data$step_identify$invalidatePolygons()
        data$step_identify$invalidateGridIdentifyStatus(force = TRUE)
        data$step_sample$invalidatePolygons()
        data$step_result$invalidatePolygons()
      }

      if (sample) {
        data$step_sample$state <- utils::modifyList(
          data$step_sample$state,
          list(modified = FALSE)
        )

        data$step_sample$invalidatePolygons()
        data$step_result$invalidatePolygons()
      }

      if (result) {
        data$step_result$state <- utils::modifyList(
          data$step_result$state,
          list(modified = FALSE)
        )

        data$step_result$invalidatePolygons()
      }

      data$polygons_memento <- data$polygons

      project <- list(
        status = data$project_status,
        priority = data$project_priority,
        method = data$project_method,
        name = data$project_name,
        description = data$project_description
      )

      saveToCache(project, file = 'project.rds')
      saveToCache(data$guide_polygon, file = 'guide_polygon.rds')
      saveToCache(data$guide_point, file = 'guide_point.rds')
      saveToCache(data$polygons, file = 'polygons.rds')
    },

    #' @description Reset data for selected workflow steps.
    #' @param delimit Logical; clear delimit step.
    #' @param identify Logical; clear identify step.
    #' @param sample Logical; clear sample step.
    #' @param result Logical; clear result step.
    clear = function(
      delimit = FALSE,
      identify = FALSE,
      sample = FALSE,
      result = FALSE
    ) {
      if (delimit) {
        data$guide_polygon <- NULL
        data$guide_point <- NULL
        data$polygons <- NULL

        shinyjs::delay(
          500L,
          data$application$dlg_method$show(ns = shiny::NS(NULL))
        )
      }

      if (identify) {
        data$polygons_raw$roofs_count <- 0L
        data$polygons_raw$cells_count <- 0L

        safe_db(
          data$db$dbExecute('DELETE FROM roofs'),
          msg = "clear:DELETE FROM roofs"
        )
        safe_db(
          data$db$dbExecute(
            'UPDATE tiles SET status = 0, cells = "000000000" WHERE status <> -1'
          ),
          msg = "clear:UPDATE tiles"
        )

        invalidate(data$roofs_changed)
      }

      if (sample) {
        data$polygons_raw <- data$polygons_raw |>
          dplyr::mutate(
            focused_sample = FALSE,
            sample_df = list(tibble::tibble()),
            samples_sf = list(sf_empty()),
            samples_quadrat_sf = list(sf_empty())
          )
      }

      if (result) {
        for (i in seq_len(nrow(data$polygons))) {
          if (nrow(data$polygons[i, ]$samples_sf[[1L]]) > 0L) {
            data$polygons_raw[i, ]$samples_sf[[1L]]$d1 <- NA
            data$polygons_raw[i, ]$samples_sf[[1L]]$d2 <- NA
            data$polygons_raw[i, ]$samples_sf[[1L]]$pop_u5_1 <- NA
            data$polygons_raw[i, ]$samples_sf[[1L]]$pop_a5_1 <- NA
            data$polygons_raw[i, ]$samples_sf[[1L]]$pop_u5_2 <- NA
            data$polygons_raw[i, ]$samples_sf[[1L]]$pop_a5_2 <- NA
            data$polygons_raw[i, ]$samples_sf[[1L]]$pop_u5 <- NA
            data$polygons_raw[i, ]$samples_sf[[1L]]$pop_a5 <- NA
            data$polygons_raw[i, ]$samples_sf[[1L]]$comment <- NA
          }
        }
      }

      save(
        delimit = delimit,
        identify = identify,
        sample = sample,
        result = result
      )
    },

    #' @description Restore from memento, reset workflow step states.
    #' @param delimit Logical; rollback delimit step.
    #' @param identify Logical; rollback identify step.
    #' @param sample Logical; rollback sample step.
    #' @param result Logical; rollback result step.
    rollback = function(
      delimit = FALSE,
      identify = FALSE,
      sample = FALSE,
      result = FALSE
    ) {
      data$polygons <- data$polygons_memento

      if (delimit) {
        data$step_delimit$state <- utils::modifyList(
          data$step_delimit$state,
          list(modified = FALSE)
        )
      }

      if (identify) {
        data$step_identify$state <- utils::modifyList(
          data$step_identify$state,
          list(modified = FALSE)
        )
      }

      if (sample) {
        data$step_sample$state <- utils::modifyList(
          data$step_sample$state,
          list(modified = FALSE)
        )
      }

      if (result) {
        data$step_result$state <- utils::modifyList(
          data$step_result$state,
          list(modified = FALSE)
        )
      }
    },

    #' @description Read an RDS file from the project cache directory.
    #' @param file Filename within the cache directory.
    #' @return The deserialized object, or NULL on failure.
    readFromCache = function(file) {
      fp <- fs::path(data$cache, file)
      safe_read_rds(fp, default = NULL, msg = "readFromCache")
    },

    #' @description Write an RDS file to the project cache directory.
    #' @param rds The object to serialize, or NULL to delete.
    #' @param file Filename within the cache directory.
    saveToCache = function(rds, file) {
      fp <- fs::path(data$cache, file)
      if (is.null(rds)) {
        tryCatch(
          if (fs::file_exists(fp)) fs::file_delete(fp),
          error = function(e) {
            logWarn(
              "[saveToCache] Failed to delete %s: %s",
              fp,
              conditionMessage(e)
            )
          }
        )
      } else {
        safe_save_rds(rds, fp, msg = "saveToCache")
      }
    },

    #' @description Create a new project with metadata.
    #' @param status Project status code (D/P/A).
    #' @param priority Project priority code (U/H/M/L).
    #' @param name Project name.
    #' @param description Project description.
    addProject = function(status, priority, name, description) {
      data$project_status <- status
      data$project_priority <- priority
      data$project_name <- name
      data$project_description <- description
      data$project_active_raw <- name

      project <- list(
        status = data$project_status,
        priority = data$project_priority,
        name = data$project_name,
        description = data$project_description
      )

      saveToCache(project, file = 'project.rds')

      load(name, force = TRUE)
    },

    #' @description Clone a project's files and create under a new name.
    #' @param status Project status code.
    #' @param priority Project priority code.
    #' @param name New project name.
    #' @param description Project description.
    projectClone = function(status, priority, name, description) {
      safe_file_copy(
        path = fs::dir_ls(
          getDirAppUsers(
            data$application$user$email,
            data$project_name
          ),
          full.names = TRUE
        ),
        new_path = getDirAppUsers(data$application$user$email, name),
        msg = "projectClone"
      )

      addProject(status, priority, name, description)
    },

    #' @description Update project metadata.
    #' @param status Project status code.
    #' @param priority Project priority code.
    #' @param description Project description.
    projectEdit = function(status, priority, description) {
      data$project_status <- status
      data$project_priority <- priority
      data$project_description <- description

      save()
    },

    #' @description Delete current project (cannot delete 'default').
    projectDelete = function() {
      if (data$project_name == 'default') {
        return()
      }

      tryCatch(
        fs::dir_delete(data$cache),
        error = function(e) {
          logWarn(
            "[projectDelete] Failed to delete %s: %s",
            data$cache,
            conditionMessage(e)
          )
        }
      )

      projectSelect()
    },

    #' @description Switch active project.
    #' @param name Project name to switch to (default: 'default').
    projectSelect = function(name = 'default') {
      load(name)

      data$step_delimit$state <- utils::modifyList(
        data$step_delimit$state,
        list(modified = FALSE)
      )

      safe_save_rds(
        list(project = data$project_name),
        fs::path(fs::path_dir(path = data$cache), 'project.rds'),
        msg = "projectSelect"
      )
    }
  )
)
