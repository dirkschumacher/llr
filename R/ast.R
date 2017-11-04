# Some methods to construct AST elements
# Since we use R's data structures, they all return code
call_node <- function(operator, elements) {
  rlang::get_expr(rlang::quo(rlang::UQE(operator)(rlang::UQS(elements))))
}

function_node <- function(arguments, body) {
  argument_names <- as.character(arguments)
  arguments <- lapply(argument_names, function(x) quote(alist(a = ))[[2L]])
  names(arguments) <- argument_names
  fun_ast <- rlang::new_function(arguments, body)
  rlang::get_expr(rlang::quo(!!fun_ast))
}

assign_node <- function(name, body) {
  rlang::get_expr(
    rlang::quo(assign(rlang::UQS(c(list(as.character(name), body)))))
  )
}

quote_node <- function(expr) {
  rlang::get_expr(rlang::quo(rlang::get_expr(rlang::quo(!!expr))))
}

macro_node <- function(name, arguments, body) {
  fun <- rlang::eval_tidy(function_node(arguments, body))
  structure(
    rlang::get_expr(rlang::quo(!!assign_node(name, rlang::get_expr(rlang::quo(structure(!!fun, class = "llr_macro")))))),
    class = "llr_macro")
}
