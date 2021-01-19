test_that("def works", {
  res <- llr_test("(def x 1)
           x")
  expect_equal(res, 1, ignore_attr = TRUE)
})

test_that("fn works", {
  res <- llr_test("(fn [a b] (+ a b))")
  expect_equal(res(1, 2), 3)
})

test_that("fn works with a name", {
  res <- llr_test("(fn myname [a b] (+ a b))")
  expect_equal(res(1, 2), 3)
})

test_that("meta data works 1", {
  interp <- llr_env$new()
  res <- interp$eval("(def x ^{:wat 1} [1 2 3])
                  (meta x)")
  expect_equal(res$get(":wat"), ral_integer(1L))
  res <- interp$eval("(def ^{:wat 1} y [1 2 3])
                  (meta y)")
  expect_equal(res, NULL)
  res <- interp$eval("(meta 'y)")
  expect_equal(res$get(":wat"), 1, ignore_attr = TRUE)
})

test_that("fn multi methods", {
  res <- llr_test("
                  (fn
                    ([] 0)
                    ([a] a)
                    ([a b] (+ a b)))
                  ")
  expect_equal(res(), 0, ignore_attr = TRUE)
  expect_equal(res(32), 32, ignore_attr = TRUE)
  expect_equal(res(32, 10), 42, ignore_attr = TRUE)
})

test_that("fn multi methods with dots", {
  interp <- llr_env$new()
  interp$eval("
  (def plus2 (fn plus
          ([] 0)
          ([a] a)
          ([a b] (r/base::`+` a b))
          ([a b & more] (reduce plus (conj [a b] more)))))
  ")
  expect_equal(interp$eval("(plus2)"), 0, ignore_attr = TRUE)
  expect_equal(interp$eval("(plus2 42)"), 42, ignore_attr = TRUE)
  expect_equal(interp$eval("(plus2 42 1)"), 43, ignore_attr = TRUE)
  expect_equal(interp$eval("(plus2 1 2 3 4 5 6 7 8 9 10)"), sum(1:10), ignore_attr = TRUE)
})

test_that(":: works", {
  res <- llr_test("(r/base::round 10.4)")
  expect_equal(res, round(10.4), ignore_attr = TRUE)
})

test_that("$ works", {
  res <- llr_test("(r/$ r/datasets::mtcars hp)")
  expect_equal(res, mtcars$hp)
})

test_that("defmacro works", {
  res <- llr_test("
  (defmacro test [] (Sys.getpid))
  (def a (test))
  a
  ")
  expect_equal(res, Sys.getpid(), ignore_attr = TRUE)
})

test_that("defmacro #2", {
  res <- llr_test("
  (defmacro infix [a b c]
    `(~b ~a ~c))
  (infix 1 + 1)
  ")
  expect_equal(res, 2, ignore_attr = TRUE)
})

test_that("variadic fn args", {
  res <- llr_test("
  (def sum_list
    (fn [vals] (reduce + 0 vals)))
  (def myplus
    (fn [a b & more] (+ (* a b) (sum_list more))))
  (myplus 1 2 3)
  ")
  expect_equal(res, 1 * 2 + 3, ignore_attr = TRUE)
})

test_that("let works", {
  code <- "
(def a 42)
(def x (let [a 1 b (+ a a)]
  (* a b)))
[a x]
"
  res <- llr_test(code)
  expect_equal(res[[1]], 42, ignore_attr = TRUE)
  expect_equal(res[[2]], 2, ignore_attr = TRUE)
})

test_that("loop and recur", {
  res <- llr_test("
    (loop [n 10 acc 0]
      (if (zero? n) acc (recur (dec n) (inc acc))))
  ")
  expect_equal(res, 10, ignore_attr = TRUE)
})

test_that("loop and recur", {
  res <- llr_test("
    (loop [n 10 acc 0]
      (let [a n]
      (if (zero? n) acc (recur (dec a) (inc acc)))))
  ")
  expect_equal(res, 10, ignore_attr = TRUE)
})

test_that("if works", {
  res <- llr_test("(if 1 42 1)")
  expect_equal(res, 42, ignore_attr = TRUE)
  res <- llr_test("(if true 42 1)")
  expect_equal(res, 42, ignore_attr = TRUE)
  res <- llr_test("(if r/mtcars 42 1)")
  expect_equal(res, 42, ignore_attr = TRUE)
  res <- llr_test("(if false 42 1)")
  expect_equal(res, 1, ignore_attr = TRUE)
  res <- llr_test("(if r/NULL 42 1)")
  expect_equal(res, 1, ignore_attr = TRUE)
})
