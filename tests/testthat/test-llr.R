context("integration")
test_that("some basic calculations", {
  expect_true(llr("(== 1 1)"))
  expect_false(llr("(== 1 2)"))
  expect_equal(llr("(+ 1 1)"), 2)
  expect_equal(3, llr("3"))
  expect_equal(3L, llr("3L"))
  expect_equal(3.5, llr("3.5"))
  expect_equal(NULL, llr("NULL"))
  expect_false(llr("FALSE"))
  expect_true(llr("TRUE"))
})

test_that("lambda function", {
  code <- "(Map (fn (x) (+ x 10)) [1 2 3])"
  expect_equal(llr(code), list(11, 12, 13))
})

test_that("lambda function #2", {
  expect_equal(llr("((fn (x) (+ x 1)) 1)"), 2)
})

test_that("we support strings", {
  expect_equal(llr("(nchar \"string\")"), 6)
})

test_that("logicals are supported", {
  expect_equal(llr("(if TRUE 1 2)"), 1)
})

test_that("define works", {
  res <- llr("
    (def x 1)
    x
  ")
  expect_equal(res, 1)
})

test_that("functions without arguments", {
  x <- function() 2
  y <- 1
  expect_equal(llr("y"), 1)
  expect_equal(llr("(x)"), 2)
})

test_that("macros!!!", {
  first <- function(x) x[[1L]]
  rest <- function(x) tail(x, length(x) - 1L)
  result <- llr("
    (defmacro infix (code)
      (quote
        ((UQ (first (rest code)))
         (UQ (first code))
         (UQ (first (rest (rest code)))))))
    (* 10 (infix (1 + 1)))
  ")
  expect_s3_class(infix, "llr_macro")
  expect_equal(result, 20)

  result <- llr("(if (> (infix (1 + 1)) 1) 1 0)")
  expect_equal(result, 1)
})

test_that("empty list", {
  expect_equal(list(), llr("()"))
  expect_equal(list(), llr("[]"))
  expect_equal(list(), llr("[] []"))
  expect_equal(list(), llr("() ()"))
})

test_that("colon operator", {
  expect_equal(1:3, llr("1:3"))
  expect_equal(utils::head, llr("utils::head"))
})
