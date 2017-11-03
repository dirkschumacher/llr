# The "compiler". Does nothing execpt macro expansion
# Also is recursive, which will lead to problems for larger ASTs
compile <- function(x, ...) UseMethod("compile", x)
compile_identity <- function(x, envir) x
recursive_compile <- function(x, envir) {
  for(i in seq_along(x)) {
    x[i] <- list(compile(x[[i]], envir))
  }
  x
}
compile.numeric <- compile_identity
compile.integer <- compile_identity
compile.logical <- compile_identity
compile.llr_macro <- compile_identity
compile.character <- compile_identity
compile.if <- recursive_compile
compile.function <- compile_identity
compile.name <- compile_identity
compile.NULL <- compile_identity
compile.macro_node <- compile_identity
compile.list <- recursive_compile
compile.call <- function(x, envir) {
  operator <- x[[1L]]
  if (is.name(operator) &&
      exists(as.character(operator), envir = envir)) {
    operator <- get(as.character(operator), envir = envir)
    if (inherits(operator, "llr_macro")) {
      stopifnot(length(x) >= 2L)
      args <- lapply(x[2L:length(x)], function(x) rlang::get_expr(rlang::quo(quote(!!x))))
      operator <- rlang::set_env(operator, envir)
      code <- rlang::quo((!!operator)(rlang::UQS(args)))
      res <- rlang::eval_tidy(code, env = envir)
      return(res)
    }
  }
  recursive_compile(x, envir)
}
