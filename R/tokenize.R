tokenize <- function(str) {
  chars <- to_char_vec(str)
  if (length(chars) <= 0) {
    return(chars)
  }
  first_char <- chars[[1]]
  state <- "expect_token"
  tokens <- character(length = length(chars)) # at most length(chars)
  i <- 1
  char_i <- 1
  # this is a state machine
  # you can always write to the current token index
  # TODO: be not ashamed of the code
  for (char_i in seq_along(chars)) {
    current_char <- chars[[char_i]]
    if (state == "expect_token") {
      if (current_char %in% c("(", ")", "[", "]", "{", "}")) {
        tokens[[i]] <- current_char
        i <- i + 1
      } else if (is_whitespace(current_char)) {
        # we skip this as it is a white space
      } else if (current_char == ";") {
        state <- "comment_token"
      } else if (current_char == "'") {
        state <- "expect_token"
        tokens[[i]] <- "'"
        i <- i + 1
      } else if (current_char == "`") {
        state <- "expect_token"
        tokens[[i]] <- "`"
        i <- i + 1
      } else if (current_char == "~" && (char_i + 1 <= length(chars)) && chars[[char_i + 1]] == "@") {
        state <- "uqs_token"
      } else if (current_char %in% c("#", "~", "_")) {
        state <- "expect_token"
        tokens[[i]] <- current_char
        i <- i + 1
      } else if (!is_whitespace(current_char)) {
        state <- if (current_char == "\"") "character_token" else "token"
        tokens[[i]] <- paste(tokens[[i]], current_char, collapse = "", sep = "")
      }
    } else if (state == "uqs_token") {
      state <- "expect_token"
      tokens[[i]] <- "~@"
      i <- i + 1
    } else if (state == "token") {
      if (current_char %in% c("(", ")", "[", "]", "{", "}") &&
          !(startsWith(tokens[[i]], "r/") && current_char == "[")) {
        state <- "expect_token"
        i <- i + 1
        tokens[[i]] <- current_char
        i <- i + 1
      } else if (is_whitespace(current_char)) {
        # found a whitespace while reading a token, let's wait for the next one
        state <- "expect_token"
        i <- i + 1
      } else if (!is_whitespace(current_char)) {
        tokens[[i]] <- paste(tokens[[i]], current_char, collapse = "", sep = "")
      } else if (current_char == ";") {
        state <- "expect_token"
      }
    } else if (state == "character_token") {
      if (current_char == "\"" && (char_i == 0 || chars[[char_i - 1]] != "\\")) {
        # we reached the end of a character sequence => new token
        tokens[[i]] <- paste(tokens[[i]], current_char, collapse = "", sep = "")
        state <- "expect_token"
        i <- i + 1
      } else {
        tokens[[i]] <- paste(tokens[[i]], current_char, collapse = "", sep = "")
      }
    } else if (state == "comment_token") {
      if (current_char != "\n") {
        # a comment is kipped
      } else if (current_char == "\n") {
        state <- "expect_token"
      }
    } else {
      stop("unexpected state")
    }
  }
  tokens[nchar(tokens) > 0]
}

to_char_vec <- function(str) {
  vapply(seq_len(nchar(str)), function(i) {
    substr(str, i, i)
  }, character(1))
}

is_whitespace <- function(char) {
  grepl("^\\s+$", char, perl = TRUE)
}
