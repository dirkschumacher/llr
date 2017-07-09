[![Travis-CI Build Status](https://travis-ci.org/dirkschumacher/llr.svg?branch=master)](https://travis-ci.org/dirkschumacher/llr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/dirkschumacher/llr?branch=master&svg=true)](https://ci.appveyor.com/project/dirkschumacher/llr) [![Coverage Status](https://img.shields.io/codecov/c/github/dirkschumacher/llr/master.svg)](https://codecov.io/github/dirkschumacher/llr?branch=master)

llr
===

Lisp like R. A work in progress, just for fun package to implement a lisp interpreter in R that can parse a lisp flavored dialect of R. Inspired by ![Peter Norvig's article](http://norvig.com/lispy.html).

The package is not yet recommended for mission critical production systems ;). It works for the examples below though.

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
 (suppressPackageStartupMessages (library dplyr))
 (library tibble)
 (as_tibble (filter mtcars (> hp 100) (> cyl 6)))
")
#> # A tibble: 14 x 11
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  18.7     8 360.0   175  3.15 3.440 17.02     0     0     3     2
#>  2  14.3     8 360.0   245  3.21 3.570 15.84     0     0     3     4
#>  3  16.4     8 275.8   180  3.07 4.070 17.40     0     0     3     3
#>  4  17.3     8 275.8   180  3.07 3.730 17.60     0     0     3     3
#>  5  15.2     8 275.8   180  3.07 3.780 18.00     0     0     3     3
#>  6  10.4     8 472.0   205  2.93 5.250 17.98     0     0     3     4
#>  7  10.4     8 460.0   215  3.00 5.424 17.82     0     0     3     4
#>  8  14.7     8 440.0   230  3.23 5.345 17.42     0     0     3     4
#>  9  15.5     8 318.0   150  2.76 3.520 16.87     0     0     3     2
#> 10  15.2     8 304.0   150  3.15 3.435 17.30     0     0     3     2
#> 11  13.3     8 350.0   245  3.73 3.840 15.41     0     0     3     4
#> 12  19.2     8 400.0   175  3.08 3.845 17.05     0     0     3     2
#> 13  15.8     8 351.0   264  4.22 3.170 14.50     0     1     5     4
#> 14  15.0     8 301.0   335  3.54 3.570 14.60     0     1     5     8
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
