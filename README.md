
<!-- README.md is generated from README.Rmd. Please edit that file -->

# llr

<!-- badges: start -->
<!-- badges: end -->

llr is a small, work in progress and just for fun clojure-like lisp on
top of R’s abstract syntax trees. Expressions are not interpreted, but
are translated to R’s AST and then interpreted by the R interpreter.

Most implementation details are sub-optimal, but the focus is on having
fun and producing results instead writing perfect code. There are also
many bugs and inconsistencies!

## Installation

``` r
remotes::install_github("dirkschumacher/llr")
```

## Intro

``` clojure
(->
  r/datasets::mtcars
  (r/dplyr::filter (> hp 100))
  (r/dplyr::summarise :count (r/dplyr::n) :mean_mpg (r/mean mpg))
  (r/tibble::as_tibble))
#> # A tibble: 1 x 2
#>   count mean_mpg
#>   <int>    <dbl>
#> 1    23     17.5
```

Or run it from R

``` r
library(llr)
interp <- llr_env$new()
interp$eval("(+ 1 1)")
#> 2
```

Also see some [Advent Of Code
solutions](https://github.com/dirkschumacher/aoc2020) in llr.

### REPL

It also has a (limited) REPL

``` r
interp <- llr_env$new()
interp$repl()
```

## Special forms

### Data Types

#### Lists

``` clojure
; this is a list
'(1 2 3 4 5 6)
; an unquoted list is a function call
(+ 1 2 3 4 5 6)
#> 21
```

#### Vectors

``` clojure
[1 2 3 4]
#> [1 2 3 4]
```

#### Maps

``` clojure
{:a 1 :b 2}
#> {:a 1 :b 2}
```

### Symbols

``` clojure
x
```

``` clojure
namespaced.variable/x
```

``` clojure
:keyword
```

``` clojure
"character"
```

``` clojure
10 ; integer
```

``` clojure
10.42 ; double
```

### Functions

``` clojure
(fn [a b] (+ a b))

(fn this
  ([] 0)
  ([a] a)
  ([a b] (+ a b))
  ([a b & more] (reduce + (conj [a b] more))))
```

### def

`def` defines a symbol in a namespace and assignes it a name.

``` clojure
(def x 1)
(def plus (fn [a b] (+ a b)))
(plus x x)
#> 2
```

### Meta-data

Symbols and values can hold meta-data. That meta-data needs to be a map
at the moment.

``` clojure
(def ^{:const true} x ^{:meta "hello"} [ 1 2 3])
(meta x)
#> {:meta hello}
```

Meta-data on symbols is currently only available to the reader.

### Macros

Macros are also supported. Macros are functions bound to a name with
meta data `{:macro true}`.

In a macro you can use syntax-quote `<backtick>` together with the
unquote `~` and unquote-splice `~@` operators.

``` clojure
(defmacro infix [operand1 operator operand2]
  `(~operator ~operand1 ~operand2))
(infix 1 + 1)
#> 2
```

### Recursion

Similar to Clojure llr uses `recur` to jump to a recursion point
currently only defined by `loop`.

``` clojure
(def is-even 
  (fn [number] 
    (loop [cnt number]
      (if (zero? cnt)
        true
        (if (< cnt 0) false (recur (- cnt 2)))))))
#> `is-even`
```

``` clojure
(is-even 5001)
#> [1] FALSE
```

``` clojure
(is-even 5000)
#> [1] TRUE
```

### Namespaces

Every top level definition is part of a namespace

``` clojure
(ns product.lib)
(defn compute [a b] (+ a b))
(ns user)
(product.lib/compute 10 32)
#> 42
```

### Reader Dispatch

The reader switches to a different set of interpretations of the next
symbol when reading the character `#`.

### `#_` ignores the next form

``` clojure
#_ (r/stop "error")
"Yay"
#> Yay
```

### R interop

All symbols starting with the namespace `r/` are treated slightly
differently. You can use that to refer to external R functions and
symbols. In addition keywords are interpreted as named arguments.

``` clojure
(r/set.seed 1)
(def rand-numbers (r/stats::rnorm :n 10))
(r/mean rand-numbers)
#> [1] 0.1322028
```

## Design Goals

-   Have fun, experiment and learn :)
-   Build a clojure-like language that supports R-interop using the `r/`
    namespace.
-   Thus the core language should feel like clojure and support some of
    clojures’s core functions, but still make it easy to work with R’s
    internal data structures.

## Contributing

-   Please read the code-of-conduct and also be aware that this a fun
    project, so things will break and progress is valued prefect code
    (at the moment).
-   However everyone is invited to play around with the language, learn
    together, extend it, document things, fix bugs and propose features.

## Code of Conduct

Please note that the llr project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
