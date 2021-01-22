translate_to_r <- function(node, envir) {
  UseMethod("translate_to_r", node)
}

translate_to_r.node <- function(node, envir) {
  stop("translation error")
}

translate_to_r.ral_string <- function(node, envir) {
  node
}

translate_to_r.integer <- function(node, envir) {
  ral_integer(node)
}

translate_to_r.ral_integer <- function(node, envir) {
  node
}

translate_to_r.ral_double <- function(node, envir) {
  node
}

translate_to_r.numeric <- function(node, envir) {
  ral_double(node)
}

translate_to_r.name <- function(node, envir) {
  node_str <- as.character(node)
  if (nchar(node_str) >= 3 && grepl("/", node_str, fixed = TRUE)) {
    split_str <- strsplit(node_str, "/", fixed = TRUE)[[1]]
    ns <- sym(split_str[[1]])
    symbol <- sym(split_str[[2]])
    return(
      expr(
        `*ns_manager*`$val_by_ns(!!ns, !!symbol)
      )
    )
  }
  sym(node)
}

translate_to_r.ns_call <- function(node, envir) {
  expr(`*ns_manager*`$create(!!as.character(sym(node[[2]]))))
}

translate_to_r.r_name <- function(node, envir) {
  r_val_name <- as.character(node)
  r_val <- substr(r_val_name, 3, nchar(r_val_name))
  tryCatch(
    parse(text = r_val)[[1]],
    error = function(e) sym(r_val)
  )
}

translate_to_r.keyword_node <- function(node, envir) {
  as.character(node)
}

translate_to_r.llr_boolean <- function(node, envir) {
  node
}

translate_to_r.symbolic_value_inf_node <- function(node, envir) {
  Inf
}

translate_to_r.symbolic_value_minf_node <- function(node, envir) {
  -Inf
}

translate_to_r.symbolic_value_nan_node <- function(node, envir) {
  NaN
}

translate_to_r.def_call <- function(node, envir) {
  target <- node[[2]]
  expr({
    `*ns_manager*`$get_current_ns()$set(
      !!target,
      !!translate_to_r(node[[3]], envir)
    )
  })
}

translate_to_r.if_call <- function(node, envir) {
  yes <- translate_to_r(node[[3]], envir)
  no <- if (length(node) > 3) {
    translate_to_r(node[[4]], envir)
  } else {
    NULL # TODO: replace with nil once there is a nil
  }
  expr(
    (function() {
      test <- !!translate_to_r(node[[2]], envir)
      `if`(
        !( # NULL (later nil) and false are the only values that evaluate to false
          is.null(test) ||
          (is.logical(test) && length(test) == 1 && !is.na(test) && !test)
        )
        ,
        !!!list(yes, no)
      )
    })()
  )
}

translate_to_r.meta_node <- function(node, envir) {
  expr((function() {
    val <- quote(!!translate_to_r(node$value, envir))
    meta <- !!translate_to_r(node$meta_data, envir)
    attr(val, "ral_meta_data") <- meta
    val
  })())
}

translate_to_r.fn_call <- function(node, envir) {
  stopifnot(length(node) > 2)
  has_name <- is_symbol(node[[2]])
  fun_name <- if (has_name) {
    node[[2]]
  }
  # here is a break point, either we observe a vector, then we have
  # case 1 or we observe a list, then we can expect case 2
  # at the moment we just implement case 1
  is_multi_fun <- inherits(node[[has_name + 2]], "ral_list")
  if (is_multi_fun) {
    fun_defs <- if (has_name) node[-2] else node
    code <- lapply(fun_defs[-1], function(fun_def) {
      fun_node <- ral_list(.data = fun_def, .subclass = "fn_call")
      create_fun_r_ast(FALSE, fun_name, fun_node)
    })
    # TODO: assumes that dots are last
    code <- lapply(code, function(x) {
      if (is.finite(x$len_args)) {
        expr(
          if (arg_len == !!x$len_args) {
            return(
              (!!x$fun)(...)
            )
          }
        )
      } else {
        expr(
          return(
            (!!x$fun)(...)
          )
        )
      }
    })
    recur <- if (has_name) {
      fun_name
    } else {
      quote(wat________) # TODO
    }
    expr((function() {
      `<-`(!!recur, function(...) {
        arg_len <- ...length()
        !!!code
        stop("Invalid arity provided")
      })
    })())
  } else {
    create_fun_r_ast(has_name, fun_name, node[-1])[["fun"]]
  }
}

create_fun_r_ast <- function(has_name, fun_name, node) {
  has_condition_map <- inherits(node[[has_name + 2]], "map_node")
  body <- translate_to_r(node[[has_name + has_condition_map + 2]], envir)
  args <- node[[has_name + 1]]
  has_any_ampand <- length(args) > 1 && args[[length(args) - 1]] == "&"
  if (has_any_ampand) {
    args <- args[-(length(args) - 1)]
    dot_name <- sym(args[[length(args)]])
    args[[length(args)]] <- quote(`...`)
    body <- expr({
      `<-`(!!dot_name, list(...))
      !!body
    })
  }
  if (has_name) {
    body <- expr({
      `<-`(!!fun_name, Recall)
      !!body
    })
  }
  if (has_condition_map) {
    stop("not implemented")
  }
  # args are symbols at compile time
  stopifnot(
    all(
      vapply(args, inherits, logical(1), "name")
    )
  )
  arg_names <- vapply(args, function(x) {
    paste0(deparse(x), collapse = "")
  }, character(1))
  arg_values <- lapply(args, function(x) {
    quote(alist(a = ))[[2]]
  })
  names(arg_values) <- arg_names
  len_params <- if (has_any_ampand) Inf else length(args)
  list(
    len_args = len_params,
    fun = expr(rlang::new_function(!!arg_values, quote(!!body)))
  )
}

translate_to_r.let_call <- function(node, envir) {
  args <- node[[2]]
  body_exprs <- lapply(node[-(1:2)], function(x) {
    translate_to_r(x, envir)
  })
  stopifnot(length(args) %% 2 == 0)
  var_def <- mapply(
    function(name, value) {
      stopifnot(is.name(name))
      expr(`<-`(!!name, !!translate_to_r(value, envir)))
    },
    as.list(args[seq(1, (length(args) - 1), 2)]),
    as.list(args[seq(2, length(args), 2)])
  )
  expr(
    (function() {
      !!!var_def
      !!!body_exprs
    })()
  )
}

translate_to_r.squote_call <- function(node, envir) {
  expr(squote(!!node[[2]]))
}

translate_to_r.quote_call <- function(node, envir) {
  expr(quote(!!node[[2]]))
}

translate_to_r.ral_vector <- function(node, envir) {
  vals <- lapply(node, function(x) {
    translate_to_r(x, envir)
  })
  expr(ral_vector(!!!vals, .meta = !!meta_data(node)))
}

translate_to_r.ral_map <- function(node, envir) {
  keys <- lapply(node$keys(), function(x) {
    translate_to_r(x, envir)
  })
  vals <- lapply(node$values(), function(x) {
    translate_to_r(x, envir)
  })
  expr(
    ral_map(
      keys = list(!!!keys),
      values = list(!!!vals)
    )
  )
}

translate_to_r.ral_list <- function(node, envir) {
  if (length(node) >= 1 && inherits(node[[1]], "r_name")) {
    return(translate_to_r.r_call(node, envir))
  }
  x <- lapply(node, function(x) {
    translate_to_r(x, envir)
  })
  if (length(x) >= 1) {
    return(expr((!!x[[1]])(!!!x[-1])))
  }
  expr(!!ral_list())
}

translate_to_r.list <- translate_to_r.ral_list

translate_to_r.loop_call <- function(node, envir) {
  args <- node[[2]]
  body_exprs <- lapply(node[-(1:2)], function(x) {
    translate_to_r(x, envir)
  })
  stopifnot(length(args) %% 2 == 0)
  names <- as.list(args[seq(1, (length(args) - 1), 2)])
  init_vals <- as.list(args[seq(2, length(args), 2)])
  var_assigns <- mapply(function(name, value) {
    stopifnot(is.name(name))
    expr(assign(!!as.character(name), !!translate_to_r(value, envir), envir = ral_______eval_env))
  }, names, init_vals)
  var_gets <- lapply(names, function(name) {
    expr(`<-`(!!name, get0(!!as.character(name), envir = ral_______eval_env)))
  })
  var_assigns_idx <- mapply(function(i, name) {
    stopifnot(is.name(name))
    expr({
      if (i == !!i) {
        assign(!!as.character(name), ...elt(i), envir = ral_______eval_env)
      }
    })
  }, seq_along(names), names)
  expr(
    (function() {
      ral_______eval_env <- new.env()
      ral_______return_val <- NULL
      !!!var_assigns
      ral____tmp_recur_next <- FALSE
      ral____tmp_recur <- function(...) {
        for (i in seq_len(...length())) {
          !!!var_assigns_idx
        }
        ral____tmp_recur_next <<- TRUE
      }
      repeat {
        !!!var_gets
        !!!body_exprs[-length(body_exprs)]
        ral_______return_val <- {
          !!body_exprs[[length(body_exprs)]]
        }
        if (ral____tmp_recur_next) {
          ral____tmp_recur_next <- FALSE
          next()
        } else {
          break()
        }
      }
      ral_______return_val
    })()
  )
}

translate_to_r.recur_call <- function(node, envir) {
  new_values <- lapply(node[-1], function(x) {
    translate_to_r(x, envir)
  })
  expr({
    ral____tmp_recur(!!!new_values)
  })
}

#' @importFrom stats setNames
translate_to_r.r_call <- function(node, envir) {
  r_fun <- translate_to_r(node[[1]])
  args <- list()
  free_slot <- 1
  i <- 2
  while (i <= length(node)) {
    val <- node[[i]]
    if (inherits(val, "keyword_node")) {
      stopifnot(i + 1 <= length(node))
      args[[free_slot]] <- setNames(
        list(expr(!!translate_to_r(node[[i + 1]]))),
        as.character(drop_colon(val))
      )
      i <- i + 2
    } else {
      args[[free_slot]] <- translate_to_r(val, envir)
      i <- i + 1
    }
    free_slot <- free_slot + 1
  }
  # TODO: revisit the next line
  args <- as.list(unlist(args, recursive = FALSE))
  rlang::call2(expr(!!r_fun), !!!args)
}

drop_colon <- function(keyword) {
  stopifnot(startsWith(keyword, ":"))
  sym(substr(keyword, 2, nchar(keyword)))
}

# translate_to_r(read_tokens(tokenize("\"abc\"")))
