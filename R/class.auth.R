#' Authentication Class
#'
ShinyAuth <- R6::R6Class(
  'ShinyAuth',
  public = list(

    #' @field url_auth_template URL template for auth entrypoint.
    url_auth_template = 'https://{auth_domain}/auth/realms/{realm}/protocol/openid-connect/auth?client_id={client_id}&redirect_uri={encoded_redirect_uri}&response_type=code&state={state}&scope={encoded_scope}',

    #' @field url_token_template URL template for token entrypoint.
    url_token_template = 'https://{auth_domain}/auth/realms/{realm}/protocol/openid-connect/token',

    #' @field url_userinfo_template URL template for userinfo entrypoint.
    url_userinfo_template = 'https://{auth_domain}/auth/realms/{realm}/protocol/openid-connect/userinfo',

    #' @field url_logout_template URL template for logout entrypoint.
    url_logout_template = 'https://{auth_domain}/auth/realms/{realm}/protocol/openid-connect/logout?redirect_uri={encoded_redirect_uri}',

    #' @description
    #' Initialize Authentication class for shiny app
    #'
    #' @param client_id Client ID from Service Provider
    #' @param client_secret Client Secret from Service Provider
    #' @param app_url This application entrypoint url
    #' @param auth_domain The domain at which to reach authentication server
    #' @param scope Scope is a mechanism in OAuth 2.0 to limit an application's access to a user's account. An application
    #' can request one or more scopes, this information is then presented to the user in the consent screen, and the access token issued to the application will be limited to the scopes granted.
    #' @param ... Other parameters that will get stored in Service Provider settings. Can be used in url templates and in
    #' provider classes.
    #'
    #' @return A new authentication object
    initialize = function(client_id, client_secret, auth_domain, scope = 'openid profile email', ...) {
      encoded_scope <- utils::URLencode(scope, reserved = TRUE, repeated = TRUE)
      private$.service_provider_settings <- c(as.list(environment()), list(...))
    },
    #' @noRd
    initialiseRedirectURI = function(client_data) {

      url_protocol <- shiny::isolate(client_data$url_protocol)
      url_hostname <- shiny::isolate(client_data$url_hostname)
      url_pathname <- shiny::isolate(client_data$url_pathname)
      url_port <- shiny::isolate(client_data$url_port)

      if (is.defined(url_port)) {
        app_url <- sprintf('%s//%s:%s%s', url_protocol, url_hostname, url_port, url_pathname)
      } else {
        app_url <- sprintf('%s//%s%s', url_protocol, url_hostname, url_pathname)
      }

      encoded_redirect_uri <- utils::URLencode(app_url, reserved = TRUE, repeated = TRUE)
      private$.service_provider_settings$app_url <- app_url
      private$.service_provider_settings$encoded_redirect_uri <- encoded_redirect_uri
    },

    #' @description
    #' Generate URL for auth entrypoint
    #'
    #' @param state State to use for XSRF detection
    #'
    #' @return URL for auth entrypoint
    url_auth = function(state) {
      params <- c(
        private$.service_provider_settings,
        state = utils::URLencode(state, reserved = TRUE, repeated = TRUE)
      )
      glue::glue(self$url_auth_template, .envir = params)
    },

    #' @description
    #' Generate URL for token entrypoint
    #'
    #' @return URL for token entrypoint
    url_token = function() {
      glue::glue(self$url_token_template, .envir = private$.service_provider_settings)
    },

    #' @description
    #' Generate URL for userinfo entrypoint
    #'
    #' @return URL for userinfo entrypoint
    url_userinfo = function() {
      glue::glue(self$url_userinfo_template, .envir = private$.service_provider_settings)
    },

    #' @description
    #' Generate URL for logout entrypoint
    #'
    #' @param return_url Where user is redirected to after successful logout. Defaults to app url.
    #' @return URL for logout entrypoint
    url_logout = function(return_url = NULL) {
      params <- private$.service_provider_settings
      if (!is.null(return_url)) {
        params$encoded_redirect_uri <- utils::URLencode(return_url, reserved = TRUE, repeated = TRUE)
      }
      glue::glue(self$url_logout_template, .envir = params)
    },
    logout = function() {
      shinyjs::runjs(sprintf('location.replace("%s")', self$url_logout()))
    },

    #' @description
    #' Authenticating shiny server wrapper.
    #' It takes a shiny server functions, and starts it once we have successfully authenticated the user.
    #'
    #' @param server Shiny server function
    #' @return Shiny server function
    server = function(server) {
      function(input, output, session) {
        # Why shiny::isolate?
        params <- shiny::parseQueryString(shiny::isolate(session$clientData$url_search))

        self$initialiseRedirectURI(session$clientData)

        if (!self$has_auth_code(params)) {
          # No auth code detected. Initiate redirect to authentication server
          # Generating random state
          # The state parameter is used to protect against XSRF.
          # >pplication generates a random string and send it to the authorization server using the state parameter.
          # The authorization server send back the state parameter.
          # If both state are the same => OK.session
          # If state parameters are differents, someone else has initiated the request.
          state <- self$generate_state()
          # Saving state in browser session storage through websocket message -> JS call.
          session$sendCustomMessage('setOauth2State', state)
          # Redirect user to auth server
          session$sendCustomMessage('redirect', self$url_auth(state = state))
          # Do nothing more until user returns.
          return()
        }
        # We have a auth code. Now we need to validate it, and fetch user info.
        # str(params)
        code <- params$code
        state <- params$state
        shiny::observe(
          {
            # Awaiting oauth2 state from browser session storage.
            req(input$oauth2State)
            # Compare incoming sate from auth server with browser storage.
            if (input$oauth2State != state) {
              # If different, we did not initiate login or response is tampered with.
              stop('Oauth2 state not matching')
            }

            access_token <- self$get_access_token(code)
            # We have valid access token, use it to fetch user info
            # Apply all user info to shiny session user property.
            # This is approximate same way Shiny Server Pro would put it.
            session$user <- self$get_userinfo(access_token)
            # Everything should be ready to fire up the Shiny Application Server

            shiny::updateQueryString(private$.service_provider_settings$app_url)

            server(input, output, session)
          },
          label = 'Authenticating server wrapper'
        )
      }
    },

    #' @description
    #' Authenticating UI wrapper for shiny ui function
    #' It will serve a minimalist shiny ui if user needs authentication.
    #' Will run given ui if user seems to come back from authenticating server by redirect.
    #' It is then up to the server function to validate and populate ui.
    #'
    #' @param ui Shiny ui function or page object (ex. fluidPage)
    #' @return Shiny ui
    ui = function(ui) {
      function(req) {
        uiValue <- NULL
        if (self$has_auth_code(shiny::parseQueryString(req$QUERY_STRING))) {
          # We beleive we are authenticated, so we serve provided ui.
          # Borrowed from Shiny UI
          if (is.function(ui)) {
            if (length(formals(ui)) > 0L) {
              # No corresponding ..stacktraceoff.., this is pure user code
              uiValue <- ..stacktraceon..(ui(req))
            } else {
              # No corresponding ..stacktraceoff.., this is pure user code
              uiValue <- ..stacktraceon..(ui())
            }
          } else {
            uiValue <- ui
          }
          uiValue <-
            shiny::tagList(
              self$scripts(),
              uiValue
            )
        } else {
          # We are not authenticated. Serve minimalist ui for redirection.
          uiValue <- shiny::fillPage(self$scripts(), bootstrap = FALSE)
        }
        uiValue
      }
    },

    #' @description
    #' Convenience function to wrap single file app in one go
    #'
    #' @param ui Shiny ui function
    #' @param server Shiny server function
    #'
    #' @return A full authenticated shiny app
    app = function(ui, server, onStart = NULL, options = list(), uiPattern = '/', enableBookmarking = NULL) {
      shiny::shinyApp(
        ui = self$ui(ui),
        server = self$server(server),
        onStart = onStart,
        options = options,
        uiPattern = uiPattern,
        enableBookmarking = enableBookmarking
      )
    },

    #' @description
    #' Injects needed JavaScript into user interface to be able to redirect and data/retrieve state
    #'
    #' @return Script tag
    scripts = function() {
      shiny::tags$head(
        shiny::tags$script(
          shiny::HTML("Shiny.addCustomMessageHandler('redirect', url => location.replace(url));"),
          shiny::HTML("Shiny.addCustomMessageHandler('setOauth2State', state => sessionStorage.setItem('state', state));"),
          shiny::HTML("$(document).on('shiny:sessioninitialized', event => {
                      Shiny.setInputValue('oauth2State', sessionStorage.getItem('state'));
                      history.replaceState(history.state, '', '/');
                    });")
        )
      )
    },

    #' @description
    #' Check if URL query parameters contains an auth code.
    #'
    #' @param params List of parsed url parameters.
    #'
    #' @return Boolean if code exists.
    has_auth_code = function(params) {
      'code' %in% names(params)
    },

    #' @description
    #' Get access token from authentication server.
    #' Used to get userinfo.
    #'
    #' @param code Authentication code from provider
    #'
    #' @return Access token
    get_access_token = function(code) {
      #' Got into some problems with httr and http/2 with Auth0.
      httr::set_config(httr::config(http_version = 0L))
      httr::set_config(httr::config(ssl_verifypeer = 0L))
      resp <- httr::POST(self$url_token(),
        body = list(
          client_id     = private$.service_provider_settings$client_id,
          grant_type    = 'authorization_code',
          scope         = private$.service_provider_settings$scope,
          redirect_uri  = private$.service_provider_settings$app_url,
          client_secret = private$.service_provider_settings$client_secret,
          code          = code
        ),
        encode = 'form'
      )
      if (resp$status_code != 200L) {
        stop('Error getting access token')
      }
      jsonlite::fromJSON(rawToChar(resp$content))$access_token
    },

    #' @description
    #' Get user info from authentication server
    #' Usually contains info on names, unique keys, email and sometimes roles.
    #' Usually somewhat configurable at service provider, so it could be used to pass along user roles/groups
    #'
    #' @seealso get_access_token
    #
    #' @param access_token Access token
    #'
    #' @return User info
    get_userinfo = function(access_token) {
      resp <- httr::GET(self$url_userinfo(), httr::add_headers(Authorization = paste0('Bearer ', access_token)))
      if (resp$status_code != 200L) {
        stop('Error getting userinfo')
      }
      httr::content(resp, encoding = 'UTF-8')
    },

    #' @description
    #' Generating random state
    #' The state parameter is used to protect against XSRF.
    #' Application generates a random string and send it to the authorization server using the state parameter.
    #' The authorization server send back the state parameter.
    #' If both state are the same => OK.
    #' If state parameters are different, someone else has initiated the request.
    #'
    #' @return 10 random alphanumeric characters
    generate_state = function() {
      paste0(sample(c(LETTERS, letters, 0:9), size = 10L, replace = TRUE), collapse = '')
    }
  ),
  private = list(
    .service_provider_settings = list()
  )
)
