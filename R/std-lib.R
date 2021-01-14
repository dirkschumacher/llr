meta_fun <- function(x, envir = parent.frame()) {
  if (inherits(x, "name")) {
    return(get0(RAL_META_DATA_NAME, envir = envir)[[as.character(x)]])
  }
  meta_data(x)
}

with_meta <- function(obj, meta, envir = parent.frame()) {
  set_meta_data(obj, meta, envir)
  obj
}

# TODO: UQ in maps and sets
squote <- function(x, envir = parent.frame()) {
  eval_UQ(x, envir)
}

eval_UQ <- function(node, envir) {
  UseMethod("eval_UQ")
}

eval_UQ.default <- function(node, envir) {
  node
}

eval_UQ.unquote_splice_call <- function(node, envir) {
  eval(translate_to_r(node[[2]]), envir = envir)
}

eval_UQ.unquote_call <- function(node, envir) {
  eval(translate_to_r(node[[2]]), envir = envir)
}

eval_UQ.ral_map <- function(node, envir) {
  keys <- node$keys()
  for (key in keys) {
    value <- node$get(key)
    if (inherits(key, "unquote_call")) {
      new_key <- eval_UQ(key, envir)
      node <- node$remove(key)
      key <- new_key
      node <- node$set(key, value)
    }
    if (inherits(value, "unquote_call")) {
      new_value <- eval_UQ(value, envir)
      node <- node$set(key, new_value)
    }
  }
  node
}

eval_UQ.list <- function(node, envir) {
  old_attributes <- attributes(node)
  new_node <- list()
  j <- 1 # the next free list slot
  for (i in seq_along(node)) {
    if (inherits(node[[i]], "unquote_splice_call")) {
      vals <- eval_UQ(node[[i]], envir)
      stopifnot(inherits(vals, "list"))
      for (val in vals) {
        new_node[[j]] <- val
        j <- j + 1
      }
    } else {
      new_node[[j]] <- eval_UQ(node[[i]], envir)
      j <- j + 1
    }
  }
  attributes(new_node) <- old_attributes
  new_node
}

do <- function(...) {
  value <- NULL
  for (i in seq_len(...length())) {
    value <- ...elt(i)
  }
  value
}

eval_fun <- function(ast, envir = parent.frame()) {
  eval(translate_to_r(ast, envir), envir)
}

#' @include vector.R
#' @include reader.R
#' @include map.R
llr_core_env <- as.environment(list(
  meta = meta_fun,
  first = function(x) x[[1]],
  last = function(x) x[[length(x)]],
  rest = function(x) x[-1],
  conj = `c`,
  do = do,
  # `=` = `==`,
  `with-meta` = with_meta,
  `read-string` = read,
  eval = eval_fun,
  squote = squote,
  ral_map = ral_map,
  ral_list = ral_list,
  ral_vector = ral_vector,
  `instance?` = function(class, obj) {
    class <- rlang::enexpr(class)
    inherits(obj, as.character(class))
  }
))

llr_core_code <- paste0(
  readLines(system.file("std-lib.clj", package = "llr")),
  collapse = "\n"
)
