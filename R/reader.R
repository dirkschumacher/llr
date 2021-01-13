read <- function(iterator, envir = parent.frame()) {
  if (is.character(iterator)) {
    iterator <- iterator_from_str(iterator)
  }
  consume_token(iterator, envir)
}

regular_tokenhandler <- function(element, token_iterator, envir) {
  if (element == "'") {
    # TODO: check if quote is re-def? isTRUE(node[[1]] == "quote")
    return(ral_list(sym("quote"), consume_token(token_iterator, envir),
      .subclass = "quote_call"
    ))
  }
  if (element == "~") {
    return(ral_list(
      sym("UQ"), consume_token(token_iterator, envir),
      .subclass = "unquote_call"
    ))
  }
  if (element == "~@") {
    return(ral_list(
      sym("UQS"), consume_token(token_iterator, envir),
      .subclass = "unquote_splice_call"
    ))
  }
  if (element == "`") {
    return(ral_list(sym("squote"),
      consume_token(token_iterator, envir),
      .subclass = "squote_call"
    ))
  }
  if (is_integer(element)) {
    return(ral_integer(element))
  }
  if (is_numeric(element)) {
    return(ral_double(element))
  }
  if (is_string(element)) {
    return(ral_string(substr(element, 2, nchar(element) - 1)))
  }
  if (is_valid_keyword(element)) {
    return(new_keyword_node(element))
  }
  if (is_valid_boolean(element)) {
    return(new_boolean_node(element == "true"))
  }
  if (is_valid_symbolic_value(element)) {
    return(new_symbolic_value_node(element))
  }
  if (startsWith(element, "r/") && is_valid_symbol(element)) {
    return(new_r_symbol(element))
  }
  if (is_valid_symbol(element)) {
    return(new_symbol_node(element))
  }
  if (is_meta(element)) {
    # we expect two more tokens
    # they are evaluated at read time
    # Problem, we cannot set attributes to symbols, so we need to
    # assign our own runtime bookkeeping
    meta_data <- consume_token(token_iterator, envir)
    value <- consume_token(token_iterator, envir)
    stopifnot(inherits(meta_data, "ral_map"))
    value <- set_meta_data(value, meta_data, envir)
    return(value)
  }
  if (is_list_end(element)) {
    return(new_list_end_node())
  }
  if (is_vector_end(element)) {
    return(new_vector_end_node())
  }
  if (is_setmap_end(element)) {
    return(new_setmap_end_node())
  }
  if (is_vector_start(element)) {
    the_list <- list()
    repeat {
      el <- consume_token(token_iterator, envir)
      if (inherits(el, "vector_end_node")) {
        break()
      }
      the_list[[length(the_list) + 1]] <- el
    }
    return(ral_vector(.data = the_list))
  }
  if (is_list_start(element)) {
    the_list <- list()
    repeat {
      el <- consume_token(token_iterator, envir)
      if (inherits(el, "list_end_node")) {
        break()
      }
      the_list[[length(the_list) + 1]] <- el
    }
    if (inherits(the_list[[1]], "r_name")) {
      return(ral_list(.data = the_list, .subclass = "r_call"))
    }
    if (is_symbol(the_list[[1]])) {
      head_sym <- as.character(the_list[[1]])
      return(
        switch(head_sym,
          fn = ral_list(.data = the_list, .subclass = "fn_call"),
          def = ral_list(.data = the_list, .subclass = "def_call"),
          let = ral_list(.data = the_list, .subclass = "let_call"),
          quote = ral_list(.data = the_list, .subclass = "quote_call"),
          loop = ral_list(.data = the_list, .subclass = "loop_call"),
          ns = ral_list(.data = the_list, .subclass = "ns_call"),
          recur = ral_list(.data = the_list, .subclass = "recur_call"),
          ral_list(.data = the_list)
        )
      )
    }
    return(ral_list(.data = the_list))
  }
  if (is_map_start(element)) {
    the_list <- list()
    repeat {
      el <- consume_token(token_iterator, envir)
      if (inherits(el, "setmap_end_node")) {
        break()
      }
      the_list[[length(the_list) + 1]] <- el
    }
    keys <- the_list[seq(1, length(the_list) - 1, 2)]
    vals <- the_list[seq(2, length(the_list), 2)]
    return(ral_map(keys = keys, values = vals))
  }
  stop("Parser error at element '", element, "'")
}

dispatch_tokenhandler <- function(element, token_iterator, envir) {
  if (element == "_") {
    res <- consume_token(token_iterator, envir) # discard the next token
    if (is_exhausted(res)) {
      stop("WAT: Unexpected EOF")
    }
    return(consume_token(token_iterator, envir))
  }
  if (is_set_start(element)) {
    the_list <- list()
    repeat {
      el <- consume_token(token_iterator, envir)
      if (inherits(el, "setmap_end_node")) {
        break()
      }
      the_list[[length(the_list) + 1]] <- el
    }
    return(new_set_node(the_list))
  }
  stop("Parser error at element '", element, "'")
}

consume_token <- function(token_iterator, envir) {
  element <- token_iterator()
  if (is_exhausted(element)) {
    return(element)
  }
  if (element == "#") {
    return(dispatch_tokenhandler(token_iterator(), token_iterator, envir))
  }
  return(regular_tokenhandler(element, token_iterator, envir))
}

is_valid_symbol <- function(str) {
  if (str %in% c("::", "$")) {
    return(TRUE)
  }
  grepl(
    x = str,
    pattern = "^[a-zA-Z\\*\\+\\!\\_\\'\\?`=<>/&-][a-zA-Z0-9\\.\\*\\':\\+\\!`\\_\\'\\?=<>#\\$/&-]*$"
  )
}
is_valid_keyword <- function(str) {
  # keywords without namespaces yet
  grepl(
    x = str,
    pattern = "^:[a-zA-Z\\*\\+\\!\\_\\'\\?<>&-][a-zA-Z0-9\\.\\*\\':\\+\\!\\_\\'\\?\\$<>#&-]*$"
  )
}
is_valid_boolean <- function(x) {
  x %in% c("true", "false")
}
is_valid_symbolic_value <- function(x) {
  x %in% c("##Inf", "##-Inf", "##NaN")
}
is_meta <- function(x) {
  x == "^"
}
is_list_start <- function(x) {
  x == "("
}
is_list_end <- function(x) {
  x == ")"
}
is_vector_start <- function(x) {
  x == "["
}
is_vector_end <- function(x) {
  x == "]"
}
is_map_start <- function(x) {
  x == "{"
}
is_set_start <- function(x) {
  x == "{"
}
is_setmap_end <- function(x) {
  x == "}"
}
is_string <- function(x) {
  nchar(x) >= 2 &&
    substr(x, 1, 1) == "\"" &&
    substr(x, nchar(x), nchar(x)) == "\""
}

is_integer <- function(x) {
  x2 <- suppressWarnings(as.integer(x))
  !is.na(x2) && x2 == as.numeric(x)
}

is_numeric <- function(x) !is.na(suppressWarnings(as.numeric(x)))

#' @export
set_meta_data.name <- function(x, val, envir = NULL) {
  if (is.null(get0(RAL_META_DATA_NAME, envir = envir))) {
    assign(RAL_META_DATA_NAME, list(), envir = envir)
  }
  name <- as.character(x)
  assign("ral_______tmp_val", val, envir = envir)
  eval(expr((!!sym(RAL_META_DATA_NAME))[[!!name]] <- ral_______tmp_val), envir = envir)
  eval(quote(rm(ral_______tmp_val)), envir = envir)
  x
}
