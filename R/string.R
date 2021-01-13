#' @export
ral_string <- function(value) {
  val <- as.character(value)
  stopifnot(is.character(val), length(val) == 1)
  structure(val, class = c("ral_string", "character"))
}

#' @export
format.ral_string <- function(x, ...) {
  NextMethod("format", x)
}

#' @export
#' @include list.R
print.ral_string <- default_print
