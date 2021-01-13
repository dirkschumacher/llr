new_list_end_node <- function() {
  structure(list(), class = c("list_end_node", "node", "list"))
}

new_setmap_end_node <- function() {
  structure(list(), class = c("setmap_end_node", "node"))
}

new_set_node <- function(x) {
  structure(list(
    type = "set",
    value = x
  ), class = c("set_node", "node"))
}

new_vector_end_node <- function() {
  structure(list(), class = c("vector_end_node", "node"))
}

new_r_symbol <- function(value) {
  stopifnot(startsWith(value, "r/"))
  structure(value, class = c("r_name"))
}

new_symbol_node <- function(value) {
  as.symbol(value)
}

new_keyword_node <- function(value) {
  structure(as.character(value), class = c("keyword_node", "node", "character"))
}

new_boolean_node <- function(value) {
  stopifnot(is.logical(value), length(value) == 1)
  structure(value, class = c("boolean_node", "node", "logical"))
}

new_symbolic_value_node <- function(value) {
  value <- match.arg(value, c("##Inf", "##-Inf", "##NaN"))
  switch(
    value,
    `##Inf` = structure(Inf, class = c("symbolic_value_inf_node", "symbolic_value_node", "node")),
    `##-Inf` = structure(-Inf, class = c("symbolic_value_minf_node", "symbolic_value_node", "node")),
    `##NaN` = structure(NaN, class = c("symbolic_value_nan_node", "symbolic_value_node", "node"))
  )
}

default_print <- function(x, ...) {
  cat(format(x, ...), "\n")
}

#' @export
format.keyword_node <- function(x, ...) {
  x
}

#' @export
print.keyword_node <- default_print
