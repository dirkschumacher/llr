macroexpand <- function(ast, envir) {
  UseMethod("macroexpand")
}

macroexpand.default <- function(ast, envir) {
  ast
}

macroexpand.quote_call <- function(ast, envir) {
  ast
}

macroexpand.squote_call <- function(ast, envir) {
  ast
}

macroexpand.ral_list <- function(ast, envir) {
  if (length(ast) == 0) {
    return(ast)
  }
  if (!is_symbol(ast[[1]])) {
    return(ast)
  }
  fun_name <- as.character(ast[[1]])
  if (is_macro(fun_name, envir)) {
    fun <- eval(expr(`*ns_manager*`$get_current_ns()$get(!!fun_name)), envir = envir)
    return(macroexpand(exec(fun, !!!ast[-1]), envir))
  } else {
    if (length(ast) > 1) {
      for (i in 2:length(ast)) {
        ast[[i]] <- macroexpand(ast[[i]], envir)
      }
    }
  }
  ast
}

macroexpand.list <- function(ast, envir) {
  old_attributes <- attributes(ast)
  # TODO: nested macro expansions
  # TODO: map macro expansion
  # TODO: set macro expansion
  for (i in seq_along(ast)) {
    ast[[i]] <- macroexpand(ast[[i]], envir)
  }
  attributes(ast) <- old_attributes
  ast
}

#' @include constants.R
is_macro <- function(fun_name, macro_env) {
  if (length(fun_name) != 1 || !is.character(fun_name)) {
    return(FALSE)
  }
  if (is.null(meta_data <- get0(RAL_META_DATA_NAME, macro_env))) {
    return(FALSE)
  }
  if (is.null(meta_data <- meta_data[[fun_name]])) {
    return(FALSE)
  }
  inherits(meta_data, "ral_map") && isTRUE(meta_data$get(":macro"))
}
