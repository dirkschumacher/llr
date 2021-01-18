;; -*- clojure -*-
(ns llr.core)

(def ^{:macro true} defmacro
  (fn [name args body]
      `(with-meta (def ~name (fn ~args ~body)) {:macro true})))

(defmacro defn [name args body]
  ; define a function and assign it to a name
  `(def ~name (fn ~args ~body)))

(def =
  (fn [a b] (r/== a b)))

(defmacro use [name]
  `((r/$ *ns_manager* use) ~name))

(def +
  (fn plus
    ([] 0)
    ([a] a)
    ([a b] (r/base::`+` a b))
    ([a b & more] (reduce plus (conj [a b] more)))))

(def *
  (fn prod
    ([] 1)
    ([a] a)
    ([a b] (r/base::`*` a b))
    ([a b & more] (reduce prod (conj [a b] more)))))

(defmacro def-multi-logic [name fun]
  `(def ~name
    (fn rec
      ([a] a)
      ([a b] (~fun a b))
      ([a b & more] (reduce rec (conj [a b] more))))))

(def-multi-logic and r/base::`&&`)
(def-multi-logic or r/base::`||`)

(defn not [x] (r/! x))

(defmacro ->
  [start & values]
  (let
    [reducer-fun
      (fn [acc el]
          `(~(first el) ~acc ~@(rest el)))]
    (reduce reducer-fun start values)))

(defmacro ->>
  [start & values]
  (let
    [reducer-fun
      (fn [acc el]
          `(~(first el) ~@(rest el) ~acc ))]
    (reduce reducer-fun start values)))

(defmacro when [test & body]
  `(if ~test (do ~@body)))

(defn map? [x] (instance? ral_map x))
(defn vector? [x] (instance? ral_vector x))
(defn list? [x] (instance? ral_list x))

(defn zero? [x] (= x 0))
(defn dec [x] (- x 1))
(defn inc [x] (+ x 1))

(defmacro comment [x] nil)

(defn get [coll key]
  (if (map? coll) ((r/$ coll get) key) (r/[[ coll key)))

(defn contains? [coll key]
  ; TODO: support llr bools
  (if (map? coll) ((r/$ coll contains) key)
      ((and (>= key 1) (<= key (count coll))))))

(defn vals [map]
  ((r/$ map values)))

(defn mod [a b] (r/%% a b))

(defn int [x]
  ; TODO: handle NA, warnings
  (r/llr::ral_integer (r/as.integer x)))

(defn str [x]
  ; TODO: handle NA, warnings
  (r/llr::ral_string (r/as.character x)))

(def map (fn [f x] (r/purrr::map x f)))
(def filter (fn [f x] (r/purrr::keep x f)))
(def partial r/purrr::partial)
(def count (fn [x] (if (map? x) ((r/$ x length)) (r/length x))))
(def reduce
  (fn
    ([f coll] (r/Reduce f coll))
    ([f init coll] (r/Reduce f coll init))))

(def comp
  (fn
  ([f] f)
  ([f & more] (r/purrr::compose (r/!!! (conj [f] more))))))
