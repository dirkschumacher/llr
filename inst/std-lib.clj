;; -*- clojure -*-
(ns llr.core)

(def ^{:macro true} defmacro
  (fn [name args body]
      `(with-meta (def ~name (fn ~args ~body)) {:macro true})))

(defmacro defn [name args body]
  ; define a function and assign it to a name
  `(def ~name (fn ~args ~body)))

(defn = [a b] (boolean (r/== (hash a) (hash b))))
(defn > [a b] (boolean (r/base::`>` a b)))
(defn < [a b] (boolean (r/base::`<` a b)))
(defn >= [a b] (boolean (or (> a b) (= a b))))
(defn <= [a b] (boolean (or (< a b) (= a b))))

(defmacro use [name]
  `((r/$ *ns_manager* use) ~name))

(defmacro multi-arity4-arith [name identity binary-fun]
  `(def ~name
     (fn rec
      ([] ~identity)
      ([a] a)
      ([a b] (~binary-fun a b))
      ([a b & more] (reduce rec (concat [a b] more))))))

(multi-arity4-arith + 0 r/base::`+`)
(multi-arity4-arith * 1 r/base::`*`)

(defmacro multi-arity3-fun [name fun]
  `(def ~name
    (fn rec
      ([a] a)
      ([a b] (~fun a b))
      ([a b & more] (reduce rec (concat [a b] more))))))

(multi-arity3-fun - r/base::`-`)
(multi-arity3-fun / r/base::`/`)

(def and
  (fn rec
    ([] true)
    ([a] (if a true false))
    ([a b] (if a (if b true false) false))
    ([a b & more] (reduce rec (concat [a b] more)))))

(def or
  (fn rec
    ([a] (if a true false))
    ([a b] (if a true (if b true false)))
    ([a b & more] (reduce rec (concat [a b] more)))))

(defn not [x] (if x false true))

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

(defn nil? [x]
  ; TODO: not really sure how to implement nil
  (boolean (r/is.null x)))

(defn get [coll key]
  (if (map? coll)
    ((r/$ coll get) key)
    (let [between (fn [a b x] (and (>= x a) (<= x b)))]
      (if (between 0 (- (count coll) 1) key)
          (r/[[ coll (r/base::`+` key 1))))))


(defn first [coll] (get coll 0))
(defn last [coll] (get coll (- (count coll) 1)))
(defn rest [coll] (r/[ coll -1))

(defn contains? [coll key]
  ; TODO: support llr bools
  (if (map? coll) ((r/$ coll contains) key)
      ((and (>= key 1) (<= key (count coll))))))

(defn vals [map]
  ((r/$ map values)))

(defn keys [map]
  ((r/$ map keys)))

(defn mod [a b] (r/%% a b))

(defn int [x]
  ; TODO: handle NA, warnings
  (r/llr::ral_integer (r/as.integer x)))

(defn str [x]
  ; TODO: handle NA, warnings
  (r/llr::ral_string (r/as.character x)))

(defn boolean [x]
  (if x true false))

(def map
  (fn this
      ([f x] (r/purrr::modify x f))
      ([f x y] (r/do.call r/ral_list (r/purrr::map2 x y f)))))

(def flatten r/purrr::flatten)
(def filter r/Filter)
(def partial r/purrr::partial)
(def count
  (fn [x] (if (map? x) ((r/$ x length)) (r/length x))))
(def reduce
  (fn
    ([f coll] (r/Reduce f coll))
    ([f init coll] (r/Reduce f coll init))))

(def comp
  (fn
    ([f] f)
    ([f & more] (r/purrr::compose (r/!!! (concat [f] more))))))
