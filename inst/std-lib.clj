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

(def map (fn [f x] (r/purrr::map x f)))
(def filter (fn [f x] (r/purrr::keep x f)))
(def partial r/purrr::partial)
(def count r/length)
(def reduce r/Reduce)

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

(defmacro ->
  [start & values]
  (let
    [reducer-fun
      (fn [acc el]
          `(~(first el) ~acc ~@(rest el)))]
    (reduce reducer-fun values start)))

(defmacro ->>
  [start & values]
  (let
    [reducer-fun
      (fn [acc el]
          `(~(first el) ~@(rest el) ~acc ))]
    (reduce reducer-fun values start)))

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
