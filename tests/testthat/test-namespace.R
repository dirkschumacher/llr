test_that("namespace #1", {
  res <- llr_test("
    (ns wat)
    (def a 1)
    (ns wut)
    (def a 2)
    (ns wat)
    a
  ")
  expect_equal(res, 1, ignore_attr = TRUE)
})

test_that("namespace refer", {
  res <- llr_test("
    (ns wat)
    (def a 42)
    (ns wut)
    wat/a
  ")
  expect_equal(res, 42, ignore_attr = TRUE)
})

test_that("namespace llr.core is always imported", {
  res <- llr_test("
      (ns wat)
      (= 1 1)
    ")
  expect_true(res)
})

test_that("use works", {
  res <- llr_test("
    (ns wat)
    (def x 1)
    (ns wut)
    (use wat)
    x
  ")
  expect_equal(res, 1, ignore_attr = TRUE)
})
