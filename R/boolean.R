#' @export
new_llr_boolean <- function(value = logical()) {
  vec_assert(value, logical())
  stopifnot(!is.na(value), length(value) == 1)
  new_vctr(value, class = "llr_boolean", inherit_base_type = TRUE)
}
methods::setOldClass(c("llr_boolean", "vctrs_vctr"))

#' @export
format.llr_boolean <- function(x, ...) {
  if (isTRUE(x)) {
    "true"
  } else {
    "false"
  }
}

#' @include format.R
#' @export
print.llr_boolean <- default_print

#' @export
vec_cast.logical.llr_boolean <- function(x, to, ...) {
  vec_data(x)
}

#' @export
#' @method vec_arith llr_boolean
#' @export vec_arith.llr_boolean
vec_arith.llr_boolean <- function(op, x, y, ...) {
  UseMethod("vec_arith.llr_boolean", y)
}

#' @export
#' @method vec_arith.llr_boolean default
vec_arith.llr_boolean.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.llr_boolean MISSING
vec_arith.llr_boolean.MISSING <- function(op, x, y, ...) {
  switch(
    op,
    "!" = new_llr_boolean(!vec_data(x)),
    stop_incompatible_op(op, x, y)
  )
}
