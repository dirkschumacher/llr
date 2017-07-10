# inspired by Peter Norvig's article http://norvig.com/lispy.html

# tokenizes an expressions
tokenize <- function(text) {
  text <- gsub("(", " ( ", text, fixed = TRUE)
  text <- gsub(")", " ) ", text, fixed = TRUE)
  text <- gsub("\n", " ", text, fixed = TRUE)
  text <- strsplit(text, " ", fixed = TRUE)[[1]]
  trimws(text[nchar(text) > 0])
}

# parses tokens to an abstract syntax tree
# in this case, simply a nested list
parse <- function(tokens) {
  ast <- list()
  fun <- function(acc, token) {
    result <- acc
    if (token == "(") {
      # when an open bracket is found, create a new list
      # we need to create a new nested list
      if (length(result$path) == 0) {
        new_pos <- length(result$ast) + 1
        result$ast[[new_pos]] <- list()
      } else {
        new_pos <- length(result$ast[[result$path]]) + 1
        result$ast[[result$path]][[new_pos]] <- list()
      }
      result$path <- c(result$path, new_pos)
    } else if (token == ")") {
      # with a closing bracket, move one level up
      if (length(result$path) == 1) {
        result$path <- integer(0L)
      } else {
        result$path <- result$path[1:(length(result$path) - 1)]
      }
    } else {
      # otherwise just insert the token
      path <- result$path
      sub_ast <- result$ast[[path]]
      pos <- length(sub_ast) + 1
      result$ast[[path]] <- c(result$ast[[path]], token)
    }
    result
  }
  res <- Reduce(x = tokens,
                init = list(path = integer(0), ast = list()),
                f = fun)
  stopifnot(length(res$path) == 0) # not aligned brackets
  res$ast
}

# this takes a single lisp expression (e.g. (+ 1 1))
# and compiles it to R
compile_expression <- function(ast, envir) {
  # check i
  is_string <- function(x) grepl(pattern = "^'.*'$", x = x)

  # this converts a token to an R expression
  to_r <- function(x) {
    if (is_string(x)) {
      gsub("^'|'$", "", x)
    } else if (!is.na(suppressWarnings(as.numeric(x)))) {
      as.numeric(x)
    } else if (!is.na(suppressWarnings(as.logical(x)))) {
      as.logical(x)
    } else {
      as.symbol(x)
    }
  }
  # here define aliases for function names
  translate_dict <- c("eq" = "==",
                      "map" = "Map")
  command <- ast[[1]]
  r_command <- to_r(command)

  # some hardcoded language features
  if (command == "define") {
    substitute(
      assign(x = name,
             value = value),
      list(name = ast[[2]],
           value = compile_expression(ast[[3]], envir)))
  } else if (command == "first") {
    substitute(x[[1]], list(x = compile_expression(ast[[2]], envir)))
  } else if (command == "exists?") {
    substitute(x %in% y, list(x = compile_expression(ast[[2]], envir),
                              y = compile_expression(ast[[3]], envir)))
  } else if (command == "rest") {
    substitute((function() {
      l <- x
      if (length(l) <= 1) NULL else l[2:length(l)]
      })(), list(x = compile_expression(ast[[2]], envir)))
  } else if (command %in% names(translate_dict)) {
    matched_name <- translate_dict[command]
    stopifnot(length(ast) > 1)
    compile_expression(c(matched_name, ast[2:length(ast)]))
  } else if (command == "lambda") {
    args <- ast[[2]]
    body <- ast[[3]]
    args_expression <- lapply(args, function(x) NULL)
    names(args_expression) <- as.character(args)
    as.function(c(args_expression, compile_expression(body, envir)))
  } else {
    if (length(ast) > 1) {
      compiled_args <- lapply(ast[2:length(ast)], function(x) {
        if (length(x) > 1) {
          compile_expression(x, envir)
        } else {
          to_r(x)
        }
      })
      substitute(do.call(f, args), list(
        f = r_command,
        args = compiled_args
      ))
    } else {
      if (is.symbol(r_command) && is.function(get(command, envir = envir))) {
        substitute(f(), list(f = r_command))
      } else {
        substitute(f, list(f = r_command))
      }
    }
  }
}

# this takes many lisp expression and compiles them to an expression
compile <- function(expressions, envir) {
  as.expression(lapply(expressions, compile_expression, envir))
}

#' Evaluates lisp-like R
#'
#' @param code the code as string
#' @param envir the environment in which you want to run the code
#'
#' @export
llr <- function(code, envir = parent.frame()) {
  compiled_code <- compile(parse(tokenize(code)), envir)
  eval(compiled_code, envir = envir)
}
