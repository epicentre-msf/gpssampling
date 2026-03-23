Project <- R6::R6Class(
  classname = 'Project',
  inherit = Base,
  portable = FALSE,
  active = list(
    application = function() {
      parent
    },
    state = function() {
      private$.state_trigger$depend()
      private$.state
    }
  ),
  public = list(
    initialize = function(parent = NULL) {
      super$initialize(parent = parent)

      private$.state$can_commit <- FALSE
      private$.state$can_rollback <- FALSE
      private$.state$can_clear <- FALSE
      private$.state$modified <- FALSE
      private$.state_trigger <- reactiveTrigger()

      private$.project_status <- 'D'
      private$.project_priority <- 'U'
      private$.project_name <- 'default'
      private$.project_description <- ''

      private$project_name_trigger <- reactiveTrigger()

      # f <- fs::path(fs::path_dir(path = data$cache), 'project.rds')

      # user_projects <- getChoicesProjets()

      # if (length(user_projects) > 1) {
      #   if (fs::file_exists(f)) {
      #     private$.project_name <- readRDS(fs::path(fs::path_dir(path = data$cache), 'project.rds'))$project
      #   } else {
      #     private$.project_name <- 'default'
      #   }
      # } else {
      #   clear()
      # }

      private$step_trigger <- reactiveTrigger()
    },
    load = function() {
      logDebug('Load project')
      project <- data$readFromCache(file = 'project.rds')
      private$setProject(project = project$project, copy = TRUE)
      private$step <- project$step
      private$step$about <- FALSE
    },
    select = function(name = 'default', user = user) {
      logDebug('Select project')
      private$.project_name <- name
      data$step_delimit$load()
      data$step_identify$load()
      data$step_sample$load()
      data$step_result$load()
      load()
      private$.project_name <- name

      saveRDS(
        list(project = private$.project_name),
        fs::path(fs::path_dir(path = data$cache), 'project.rds')
      )
    },
    add = function(status, priority, name, description = NA) {
      private$.project_status <- status
      private$.project_priority <- priority
      private$.project_name <- name
      private$.project_description <- description
      private$setProject(project = private$project, copy = TRUE)
      save()
    },
    edit = function(status, priority, name, description = NA) {
      private$.project_status <- status
      private$.project_priority <- priority
      private$.project_name <- name
      private$.project_description <- description
      private$setProject(project = private$project, copy = TRUE)
    },
    delete = function() {
      if (private$.project_name == 'default') {
        dir_project <- data$cache
        select('default')
        fs::dir_delete(dir_project)
      }
    },
    save = function() {
      project <- list(
        project = private$project,
        step = private$step
      )
      data$saveToCache(project, file = 'project.rds')
    },
    save_as = function(status, priority, name, description = NA) {
      fs::file_copy(
        path = fs::dir_ls(
          getDirAppUsers(user$email, private$.project_name),
          full.names = TRUE
        ),
        new_path = getDirAppUsers(user$email, name)
      )
      private$.project_status <- status
      private$.project_priority <- priority
      private$.project_name <- name
      private$.project_description <- description
      private$setProject(project = private$project, copy = TRUE)
      save()
    },
    invalidateStatus = function() {
      private$.state_trigger$trigger()
    },
    commit = function() {
      private$.state$modified <- FALSE
      private$setProject(project = private$project, copy = TRUE)
      save()
    },
    rollback = function() {
      private$.state$modified <- FALSE
      private$setProject(project = private$project_copy, copy = FALSE)
    },
    clear = function() {
      if (private$.state$modified) {
        rollback()
      } else {
        add(
          status = 'D',
          priority = 'U',
          name = 'default',
          description = ''
        )
        commit()
      }
    },
    post = function() {
      private$.state$modified <- TRUE
      private$.state$is_editing <- FALSE
      private$checkState()
    },
    getProject = function() {
      private$project_name_trigger$depend()
      private$project
    },
    getSteps = function() {
      private$step_trigger$depend()
      private$step
    },
    getStep = function(step) {
      steps <- getSteps()
      steps[[step]]
    },
    setStep = function(step, value) {
      steps <- private$step
      steps[[step]] <- value
      if (!identical(private$step, steps)) {
        private$step <- steps
        private$step_trigger$trigger()
        # save()
      }
    }
  ),
  private = list(
    .state = NULL,
    .state_trigger = NULL,
    .project_status = NULL,
    .project_priority = NULL,
    .project_name = NULL,
    .project_description = NULL,
    project = NULL,
    project_copy = NULL,
    project_name_trigger = NULL,
    step_trigger = NULL,
    step = list(
      about = FALSE,
      stratify = FALSE
    ),
    step_current = NULL,
    statut = list(
      is_editing = FALSE,
      modified = FALSE
    ),
    setProject = function(project, copy = FALSE) {
      private$project <- project
      private$project_name_trigger$trigger()
      if (copy) {
        private$project_copy <- private$project
      }
      private$checkState()
    },
    checkState = function() {
      private$.state$can_commit <- private$.state$modified
      private$.state$can_rollback <- private$.state$modified
      private$.state$can_clear <- !private$.state$modified

      private$.state_trigger$trigger()
    }
  )
)
