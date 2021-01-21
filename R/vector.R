#' @export
ral_vector <- function(..., .data = NULL, .subclass = NULL, .meta = NULL) {
  val <- if (is.null(.data)) {
    list(...)
  } else {
    stopifnot(is.list(`.data`))
    `.data`
  }
  class(val) <- c(.subclass, "ral_vector", "list")
  ral_meta_data <- if (!is.null(.meta) && inherits(.meta, "ral_map")) {
    .meta
  }
  new_vctr(val, ral_meta_data = ral_meta_data, class = class(val))
}

#' @export
set_meta_data.ral_vector <- function(x, val, envir = NULL) {
  attr(x, "ral_meta_data") <- val
  x
}

#' @export
meta_data.ral_vector <- function(x) {
  attr(x, "ral_meta_data", exact = TRUE)
}

#' @export
#' @include format.R
format.ral_vector <- function(x, ...) {
  paste0(
    "[",
    paste0(vapply(x, function(x) llr_format(x), character(1)), collapse = " "),
    "]"
  )
}

#' @export
#' @include format.R
print.ral_vector <- default_print


#' @export
vec_ptype2.ral_vector.list <- function(x, y, ...) {
  ral_vector()
}

#' @export
vec_ptype2.list.ral_vector <- function(x, y, ...) {
  ral_vector()
}

#' @export
as.character.ral_vector <- function(x, ...) {
  format(x)
}
