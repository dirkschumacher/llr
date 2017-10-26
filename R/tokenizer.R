# tokenizes an expressions
tokenize <- function(text) {
  # regexp based on MAL https://github.com/kanaka/mal/blob/master/process/guide.md#step1
  # License Mozilla Public License 2 Copyright (C) 2015 Joel Martin <github@martintribe.org>
  # https://github.com/kanaka/mal/blob/master/LICENSE
  pattern <- "[\\s,]*([\\[\\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"|;.*|[^\\s\\[\\]{}('\"`,;)]*)"
  tokens <- stringr::str_match_all(text, pattern = pattern)[[1L]][, 2L]
  tokens[nchar(tokens) > 0L]
}
