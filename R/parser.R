# Don't laugh ;)
# A better reader will be written eventually
parse <- function(tokens) {
  is_integer <- function(x) !is.na(suppressWarnings(as.integer(x)))
  is_numeric <- function(x) !is.na(suppressWarnings(as.numeric(x)))
  token <- tokens[[1L]]
  if (token == "(") {
    list_tokens <- consume_tokens(tokens, "(", ")")
    is_empty <- length(list_tokens) == 2L
    if (is_empty) {
      if (3L <= length(tokens)) {
        remaining_ast <- parse(tokens[3L:length(tokens)])
      } else {
        remaining_ast <- NULL
      }
      return(c(list(list()), remaining_ast))
    }
    # operator can be a single token
    # or a list
    op_token <- list_tokens[[2L]]
    if (op_token == "(") {
      op_list <- consume_tokens(list_tokens[2:length(list_tokens)], "(", ")")
      operator <- parse(op_list)[[1L]]
      i <- 1L + length(op_list) + 1L
    } else {
      operator <- parse(op_token)[[1L]]
      i <- 3L
    }
    arguments <- list()
    while(TRUE) {
      if (length(list_tokens) < i) break
      element <- list_tokens[[i]]
      if (element == ")") break
      if (element == "(") {
        sub_call <- consume_tokens(list_tokens[i:length(list_tokens)], "(", ")")
        arg <- parse(sub_call)[[1L]]
        i <- i + length(sub_call)
      } else if (element == "[") {
        sub_call <- consume_tokens(list_tokens[i:length(list_tokens)], "[", "]")
        arg <- parse(sub_call)[[1L]]
        i <- i + length(sub_call)
      } else {
        arg <- parse(list_tokens[[i]])[[1L]]
        i <- i + 1
      }
      arguments <- c(arguments, list(arg))
    }
    if ((i + 1L) <= length(tokens)) {
      remaining_ast <- parse(tokens[(i + 1L):length(tokens)])
    } else {
      remaining_ast <- NULL
    }
    if (op_token == "fn") {
      stopifnot(length(arguments) == 2L)
      return(c(list(function_node(
        arguments[[1L]],
        arguments[[2L]]
      )), remaining_ast))
    } else if (op_token == "def") {
      stopifnot(length(arguments) == 2L)
      return(c(list(assign_node(
        arguments[[1L]],
        arguments[[2L]]
      )), remaining_ast))
    }else if (op_token == "defmacro") {
      stopifnot(length(arguments) == 3L)
      return(c(list(macro_node(
        arguments[[1L]],
        arguments[[2L]],
        arguments[[3L]]
      )), remaining_ast))
    } else if (op_token == "quote") {
      stopifnot(length(arguments) == 1L)
      return(c(list(quote_node(
        arguments[[1L]]
      )), remaining_ast))
    } else {
      return(c(list(call_node(operator, arguments)), remaining_ast))
    }
  }
  if (token == "[") {
    vec_tokens <- consume_tokens(tokens, "[", "]")

    # we simple act like it is a list and then transform to vector
    is_empty <- length(vec_tokens) == 2L
    if ((1L + length(vec_tokens)) <= length(tokens)) {
      remaining_ast <- parse(tokens[(1L + length(vec_tokens)):length(tokens)])
    } else {
      remaining_ast <- NULL
    }
    if (is_empty) return(c(list(list()), remaining_ast))

    vec_tokens <- vec_tokens[2L:(length(vec_tokens) - 1L)]
    vec_tokens <- c("(", vec_tokens, ")")
    parsed_expr <- as.list(parse(vec_tokens)[[1L]])
    return(c(
      list(parsed_expr),
      remaining_ast))
  }
  if (length(tokens) == 1L) {
    if (token == "NULL") {
      token <- NULL
    } else if (token == "UQ") {
      token <- call_node(
        as.symbol("::"),
        list(as.symbol("rlang"), as.symbol("UQ"))
      )
    }  else if (token == "TRUE") {
      token <- TRUE
    } else if (token == "FALSE") {
      token <- FALSE
    } else if (grepl(pattern = "^\\\".*\\\"$", x = token)) {
      removed_quotes <- gsub(pattern = "\"",
                             replacement = "",
                             x = token, fixed = TRUE)
      token <- removed_quotes
    } else if (grepl(pattern = "^.+::.+$", x = token, fixed = FALSE)) {
      lr <- strsplit(token, "::", fixed = TRUE)[[1]]
      stopifnot(length(lr) == 2L)
      token <- call_node(
        parse("::")[[1L]],
        lapply(lr, function(x) parse(x)[[1L]])
      )
    } else if (grepl(pattern = "^.+:.+$", x = token, fixed = FALSE)) {
      lr <- strsplit(token, ":", fixed = TRUE)[[1]]
      stopifnot(length(lr) == 2L)
      token <- call_node(
        parse(":")[[1L]],
        lapply(lr, function(x) parse(x)[[1L]])
      )
    } else if (grepl(pattern = "^\\D.*$", x = token)) {
      token <- as.symbol(token)
    } else if (grepl(pattern = "^\\d+$", x = token) && is_integer(token)) {
      token <- as.integer(token)
    } else if (grepl(pattern = "^\\d+L$", x = token) &&
               is_integer(stringr::str_sub(token, end = -2L))) {
      token <- as.integer(stringr::str_sub(token, end = -2L))
    } else if (is_numeric(token)) {
      token <- as.numeric(token)
    } else {
      stop("waaaa")
    }
    return(list(token))
  }
}

consume_tokens <- function(tokens, opening_token, closing_token) {
  stop_token_idx <- -1L
  opening_tokens <- 0L
  for(i in seq_along(tokens)) {
    if (tokens[[i]] == opening_token) {
      opening_tokens <- opening_tokens + 1L
    }
    if (tokens[[i]] == closing_token) {
      opening_tokens <- opening_tokens - 1L
      if (opening_tokens == 0L) {
        stop_token_idx <- i
        break
      }
    }
  }
  if (stop_token_idx == -1L) stop("ahhhh")
  tokens[1L:stop_token_idx]
}
