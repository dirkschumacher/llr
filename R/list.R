#' @export
#' @import vctrs
ral_list <- function(..., .data = NULL, .subclass = NULL, .meta = NULL) {
  val <- if (is.null(.data)) {
    list(...)
  } else {
    stopifnot(is.list(`.data`))
    `.data`
  }
  class(val) <- c(.subclass, "ral_list")
  ral_meta_data <- if (!is.null(.meta) && inherits(.meta, "ral_map")) {
    .meta
  }
  new_vctr(val, ral_meta_data = ral_meta_data, class = class(val))
}

#' @export
set_meta_data <- function(x, val, envir = NULL) {
  UseMethod("set_meta_data")
}

#' @export
meta_data <- function(x) {
  UseMethod("meta_data")
}

#' @export
set_meta_data.ral_list <- function(x, val, envir = NULL) {
  attr(x, "ral_meta_data") <- val
  x
}

#' @export
meta_data.ral_list <- function(x) {
  attr(x, "ral_meta_data", exact = TRUE)
}

default_print <- function(x, ...) {
  cat(format(x, ...), "\n")
}

#' @export
format.ral_list <- function(x, ...) {
  paste0(
    "(",
    paste0(vapply(x, function(x) format(x), character(1)), collapse = " "),
    ")"
  )
}

#' @export
#' @include list.R
print.ral_list <- default_print

#' @export
as.character.ral_list <- function(x, ...) {
  format(x)
}
