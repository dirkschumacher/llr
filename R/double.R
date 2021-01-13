#' @export
ral_double <- function(value) {
  dbl_val <- as.numeric(value)
  stopifnot(length(dbl_val) == 1)
  structure(dbl_val, class = c("ral_double", "double"))
}

#' @export
format.ral_double <- function(x, ...) {
  NextMethod("format", x)
}

#' @export
#' @include list.R
print.ral_double <- default_print
