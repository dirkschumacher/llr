[![Travis-CI Build Status](https://travis-ci.org/dirkschumacher/llr.svg?branch=master)](https://travis-ci.org/dirkschumacher/llr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/dirkschumacher/llr?branch=master&svg=true)](https://ci.appveyor.com/project/dirkschumacher/llr) [![Coverage Status](https://img.shields.io/codecov/c/github/dirkschumacher/llr/master.svg)](https://codecov.io/github/dirkschumacher/llr?branch=master)

llr
===

Lisp like R (llr). A work in progress, just for fun package to implement a lisp interpreter in R that can parse a lisp flavoured dialect of R. Inspired by [Peter Norvig's article](http://norvig.com/lispy.html).

The package is not yet recommended for mission critical production systems ;). It works for the examples below though.

Install
-------

``` r
devtools::install_github("dirkschumacher/llr")
```

Examples
--------

``` r
llr("(map (lambda (x) (+ x 10)) (list 1 2 3))")
#> [[1]]
#> [1] 11
#> 
#> [[2]]
#> [1] 12
#> 
#> [[3]]
#> [1] 13
```

``` r
llr("
  (define x (list 1 2 3 4 5))
  (Reduce (lambda (acc x) (+ acc x)) 
    (Filter (lambda (y) (> y 20)) (Map (lambda (x) (* x 10)) x)))
")
#> [1] 120
```

``` r
llr("
 (library dplyr)
 (library tibble)
 (as_tibble (filter mtcars (> hp 180) (> cyl 6)))
")
#> # A tibble: 7 x 11
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  14.3     8   360   245  3.21 3.570 15.84     0     0     3     4
#> 2  10.4     8   472   205  2.93 5.250 17.98     0     0     3     4
#> 3  10.4     8   460   215  3.00 5.424 17.82     0     0     3     4
#> 4  14.7     8   440   230  3.23 5.345 17.42     0     0     3     4
#> 5  13.3     8   350   245  3.73 3.840 15.41     0     0     3     4
#> 6  15.8     8   351   264  4.22 3.170 14.50     0     1     5     4
#> 7  15.0     8   301   335  3.54 3.570 14.60     0     1     5     8
```

``` r
llr("(first (list 1 2 3))")
#> [1] 1
```

``` r
llr("(as.integer (rest (list 1 2 3)))")
#> [1] 2 3
```

``` r
llr("(if (< 1 10) 'hello' 'world')")
#> [1] "hello"
```

``` r
llr("(exists? 1 (list 1 2 3))")
#> [1] TRUE
```

``` r
llr("
  (library purrr)
  (define x (list 1 2 3 4 5 6 7))
  (as.numeric (keep x (lambda (x) (> x 5))))
")
#> [1] 6 7
```

``` r
llr("(pi)")
#> [1] 3.141593
```

``` r
llr("
  (define some_fun (lambda () 1))
  (some_fun)
")
#> [1] 1
```

It also has a repl :)

``` r
llr::repl() # exit by typing (llr:exit)
```
