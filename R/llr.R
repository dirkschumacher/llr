#' @include tokenizer.R ast.R compiler.R parser.R
NULL

#' Evaluates lisp-like R
#'
#' @param code the code as string
#' @param envir the environment in which you want to run the code
#' @export
llr <- function(code, envir = parent.frame()) {
  statements <- parse(tokenize(code))
  return_val <- invisible(NULL)
  for(statement in statements) {
    return_val <- rlang::eval_bare(compiler::compile(compile(statement, envir)), env = envir)
  }
  return_val
}

#' A simple repl
#'
#' You can exit it with `(llr:exit)`
#'
#' @param envir the parent environment of the repl
#'
#' @export
repl <- function(envir = parent.frame()) {
  message("Welcome to the LLR REPL! Type (llr:exit) to exit.")
  repl_env <- rlang::new_environment()
  parent.env(repl_env) <- envir
  standard_lib <- local({
    first <- function(x) x[[1L]]
  }, envir = repl_env)
  while((input <- trimws(readline("> "))) != "(llr:exit)")  {
    input <- trimws(input)
    if (input == "") {
      next
    }
    output <- withVisible(tryCatch(llr(input, repl_env),
             error = function(e) {
               message(e)
               cat("\n")
               }))
    is_visible <- output$visible
    if (is_visible) {
      print(output$value)
    }
  }
  message("LLR repl closed")
}
