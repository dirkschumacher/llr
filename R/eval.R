#' @import rlang
#' @include ast-translator.R
#' @include constants.R
#' @include std-lib.R
#' @export
llr_env <- R6::R6Class("LLREnv",
  public = list(
    initialize = function() {
      private$exec_envir <- new.env(parent = global_env())
      private$load_core_lib()
      self$eval("
        (ns user)
        ;(use llr.core)
      ") # switch to user namespace
    },
    eval = function(code_text) {
      stopifnot(is.character(code_text), length(code_text) == 1)
      token_iter <- iterator_from_str(code_text)
      last_value <- NULL
      ns_manager <- get0("*ns_manager*", envir = private$exec_envir)
      stopifnot(!is.null(ns_manager))
      repeat {
        ns_envir <- ns_manager$get_current_ns()$get_envir()
        ast <- read(token_iter, ns_envir)
        if (is_exhausted(ast)) {
          return(last_value)
        }
        ast <- macroexpand(ast, ns_envir)
        r_expression <- translate_to_r(ast, ns_envir)
        last_value <- eval(r_expression, ns_envir)
      }
    },
    envir = function() {
      private$exec_envir
    },
    repl = function() {
      message("Welcome to the LLR REPL! Hit <ESC> to exit.")
      input_stream_gen <- generator(function() {
        repeat {
          input <- trimws(readline("llr:> "))
          for (token in tokenize(input)) {
            yield(token)
          }
        }
      })
      token_iter <- input_stream_gen()
      last_value <- NULL
      ns_manager <- get0("*ns_manager*", envir = private$exec_envir)
      stopifnot(!is.null(ns_manager))
      repeat {
        ns_envir <- ns_manager$get_current_ns()$get_envir()
        ast <- read(token_iter, ns_envir)
        if (is_exhausted(ast)) {
          return(last_value)
        }
        ast <- macroexpand(ast, ns_envir)
        r_expression <- translate_to_r(ast, ns_envir)
        output <- withVisible(
          tryCatch(eval(r_expression, ns_envir),
            error = function(e) {
              message(e)
              cat("\n")
            }
          )
        )
        is_visible <- output$visible
        if (is_visible) {
          print(output$value)
        }
      }
      message("LLR repl closed")
    }
  ),
  private = list(
    exec_envir = NULL,
    load_core_lib = function() {
      envir <- private$exec_envir
      ns_manager <- namespace_manager$new(envir)
      assign("*ns_manager*", ns_manager, envir = envir)
      ns_manager$create("llr.core")
      ns_envir <- ns_manager$get_current_ns()$get_envir()
      for (name in ls(llr_core_env)) {
        assign(name, get(name, envir = llr_core_env), envir = ns_envir)
      }
      self$eval(llr_core_code)
    }
  )
)

#' @import coro
iterator_from_str <- function(str) {
  tokens <- tokenize(str)
  token_generator <- generator(function() {
    for (token in tokens) {
      yield(token)
    }
  })
  token_generator()
}
