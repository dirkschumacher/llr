namespace <- R6::R6Class(
  "Namespace",
  public = list(
    initialize = function(parent_envir) {
      private$envir <- new.env(parent = parent_envir)
    },
    set = function(symbol, value) {
      x <- substitute(symbol)
      assign(as.character(x), value, envir = private$envir)
      x
    },
    get = function(symbol) {
      name <- as.character(substitute(symbol))
      get0(name, envir = private$envir)
    },
    get_envir = function() {
      private$envir
    },
    import = function(ns) {
      env <- ns$get_envir()
      for (val in ls(env)) {
        assign(val, get0(val, env), envir = self$get_envir())
      }
    }
  ),
  private = list(
    envir = NULL
  )
)

namespace_manager <- R6::R6Class(
  "NamespaceManager",
  public = list(
    initialize = function(parent_envir) {
      private$envir <- new.env()
      private$parent_envir <- parent_envir
    },
    create = function(symbol) {
      if (exists(symbol, envir = private$envir)) {
        private$current_envir <- self$get(symbol)
        return(private$current_envir)
      }
      ns <- namespace$new(private$parent_envir)
      assign(symbol, ns, envir = private$envir)
      ns$import(self$get("llr.core"))
      private$current_envir <- ns
      ns
    },
    use = function(name) {
      name <- substitute(name)
      ns <- self$get_current_ns()
      ns$import(self$get(as.character(name)))
    },
    get_current_ns = function() {
      private$current_envir
    },
    get = function(symbol) {
      get0(symbol, envir = private$envir)
    },
    val_by_ns = function(ns_name, symbol_name) {
      ns <- self$get(as.character(substitute(ns_name)))
      stopifnot(!is.null(ns))
      eval(expr(ns$get(!!substitute(symbol_name))))
    }
  ),
  private = list(
    envir = NULL,
    parent_envir = NULL,
    current_envir = NULL
  )
)
