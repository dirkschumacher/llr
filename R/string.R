#' @export
ral_string <- function(value) {
  val <- as.character(value)
  stopifnot(length(val) == 1)
  structure(val, class = c("ral_string", "character"))
}

#' @export
format.ral_string <- function(x, ...) {
  paste0("\"", NextMethod("format", x), "\"", collapse = "")
}

#' @export
#' @include format.R
print.ral_string <- default_print
