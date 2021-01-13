test_that("tokenizer tokenizes #1", {
  res <- tokenize("(+ 1 2232)")
  expect_equal(res, c("(", "+", "1", "2232", ")"))
})

test_that("tokenizer tokenizes #2", {
  res <- tokenize("(+ abc \"abc\")")
  expect_equal(res, c("(", "+", "abc", "\"abc\"", ")"))
})

test_that("tokenizer tokenizes #3", {
  res <- tokenize("(fun [a b c] (+ a b c))")
  expect_equal(res, c(
    "(", "fun", "[", "a", "b", "c", "]",
    "(", "+", "a", "b", "c", ")", ")"
  ))
})

test_that("tokenizer supports comments", {
  res <- tokenize("(defn [a b c]
                  ; This is a comment
                  (+ a b c)
                  )")
  expect_equal(res, c(
    "(", "defn", "[", "a", "b", "c", "]",
    "(", "+", "a", "b", "c", ")", ")"
  ))
})

test_that("tokenizer supports multiple expressions", {
  res <- tokenize("
                  (+ 1 2)
                  (* 1 2)
                  ")
  expect_equal(res, c("(", "+", "1", "2", ")", "(", "*", "1", "2", ")"))
})

test_that("syntax quote special case", {
  res <- tokenize("`~1")
  expect_equal(res, c("`", "~", "1"))
  res <- tokenize("`(~@[1 2])")
  expect_equal(res, c("`", "(", "~@", "[", "1", "2", "]", ")"))
})

test_that("hash token", {
  res <- tokenize("#_1")
  expect_equal(res, c("#", "_", "1"))
})
