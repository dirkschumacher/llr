llr_format <- function(x) {
  UseMethod("llr_format")
}

llr_format.ral_map <- function(x) {
  format(x)
}

llr_format.ral_list <- function(x) {
  format(x)
}

llr_format.ral_vector <- function(x) {
  format(x)
}

llr_format.ral_string <- function(x) {
  format(x)
}

llr_format.ral_integer <- function(x) {
  format(x)
}

llr_format.keyword_node <- function(x) {
  format(x)
}

llr_format.llr_boolean <- function(x) {
  format(x)
}

llr_format.default <- function(x) {
  paste0("#r_object[", paste0(class(x), collapse = ",") ,"]")
}

default_print <- function(x, ...) {
  cat(format(x, ...), "\n")
}
