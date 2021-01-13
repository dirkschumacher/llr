#' @export
ral_integer <- function(value) {
  int_val <- as.integer(value) # 32 bit limit :(
  stopifnot(is.numeric(int_val), length(int_val) == 1)
  structure(int_val, class = c("ral_integer", "integer"))
}

#' @export
format.ral_integer <- function(x, ...) {
  NextMethod("format", x)
}

#' @export
#' @include list.R
print.ral_integer <- default_print
