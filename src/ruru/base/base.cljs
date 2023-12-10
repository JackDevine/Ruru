(ns ruru.base.base
  (:require [clojure.set]
            [clojure.string :as str]
            [cljs.reader :as reader])
  (:require-macros [ruru.base.macros :refer [inline-resource]]))

(defn ignore-next-form-impl [tokens] (remove #(and (seq? %1) (= :#_ (first %1))) tokens))

(defn html? [x] (contains? x 'html))
(defn ruru-symbol? [exp] (keyword? exp))
(defn ruru-string? [exp] (and (seq? exp) (= (first exp) :#_string)))
(defn ruru-number? [exp] (number? exp))
(defn ruru-function? [t env]
  (cond
    (seq? t) (= :lambda (-> t first :value))
    (contains? env t) (= :function (-> t env :role))
    :else (= :function (:role t))))

(defn list-fn [& args] (if (empty? args)
                         {'array_dims [0 0] 'value []}
                         (let [val (into [] (apply list args))]
                           {'array_dims [(count val) 1] 'value val})))

(defn ones [dims] {'array_dims dims 'value (into [] (repeat (reduce * dims) 1.0))})

(defn shape-impl [arr] (get arr 'array_dims nil))

(defn array-length [arr]
  (count (arr 'value)))

(defn reshape-impl [arr dims]
  (if (= (array-length arr) (reduce * (dims 'value))) (assoc arr 'array_dims (dims 'value))
      (list 'error (str "Reshape error, dimensions " (dims 'value) " not compatible with element count " (array-length arr)))))

(defn range-impl [start end]
  {'array_dims [(+ 1 (- end start)) 1] 'value (into [] (range start (+ 1 end)))})

(defn extend-dim [arr dim]
  (let [old-dim (arr 'array_dims)
        dim-1-extend (inc (- (first dim) (first old-dim)))
        arr-value (arr 'value)
        dim-1-extended (into [] (apply interleave (repeat dim-1-extend arr-value)))
        dim-2-extend (inc (- (second dim) (second old-dim)))
        dim-2-extended (into [] (apply concat (repeat dim-2-extend arr-value)))]
    {'array_dims dim 'value (if (= 1 dim-2-extend) dim-1-extended dim-2-extended)}))

(defn mul-impl [x y xdims ydims]
  (let [broadcast-dim+iter (for [dim (range (max (count ydims) (count xdims)))]
                             (let [x-len (get xdims dim)
                                   y-len (get ydims dim)]
                               [(if (or (nil? x-len) (= 1 x-len)) y-len x-len)
                                (if (or (nil? y-len) (= 1 y-len)) x-len y-len)]))
        new-dim (mapv first broadcast-dim+iter)]
    (if (= xdims ydims) {'array_dims new-dim
                         'value (mapv * (x 'value) (y 'value))}
        (mul-impl (extend-dim x new-dim) (extend-dim y new-dim) new-dim new-dim))))

(defn mul [x y]
  (let [xdims (get x 'array_dims [])
        ydims (get y 'array_dims [])]
    (cond (not= 0 (reduce + (map empty? [xdims ydims]))) '(error "Not implemented")
          :else (mul-impl x y xdims ydims))))

(defn ruru-array? [x] (contains? x 'array_dims))

(defn ruru-index-linear [coll key]
  (cond
    (number? key) (get (coll 'value) (dec key))
    (= 'error (first key)) key
    :else (list 'error (str "Invalid key for linear indexing " (str key)))))

(defn inbounds [dims ind]
  (reduce #(and %1 %2) true (mapv >= dims ind)))

(defn ruru-linear-index [dims ind]
  (cond (not (inbounds dims ind)) (list 'error (str "Indeces " (str ind) " out of bounds for array of size " (str dims)))
        :else (reduce + (first ind) (map * (take (dec (count dims)) dims) (map dec (rest ind))))))

(defn ruru-get-index [coll key]
  (cond
    (and (ruru-array? coll) (number? key)) (ruru-index-linear coll key)
    (ruru-array? coll) (ruru-index-linear coll (ruru-linear-index (coll 'array_dims) (key 'value)))
    (ruru-string? coll) (list :#_string (apply str "\"" (get (second coll) key) "\""))
    (seq? coll) (nth coll (dec key))
    :else (get coll key)))

(defn ruru-concat [x y]
  (let [x (if (ruru-array? x) x {'array_dims [1 1] 'value [x]})
        y (if (ruru-array? y) y {'array_dims [1 1] 'value [y]})
        xdims (x 'array_dims)
        ydims (y 'array_dims)]
    (if (not= (take (- (count xdims) 1) xdims) (take (- (count xdims) 1) ydims))
      (list 'error (str "Dimensions " (apply str (interpose "x" xdims))
                        " and " (apply str (interpose "x" ydims))
                        " cant be concatenated"))
      {'array_dims (assoc xdims (dec (count xdims)) (+ (last xdims) (last ydims)))
       'value (into [] (concat (x 'value) (y 'value)))})))

(defn set-diff [a1 a2]
  (let [v1 (set (a1 'value))
        v2 (set (a2 'value))
        d (clojure.set/difference v1 v2)
        d (sort (into [] d))]
    {'array_dims [1 (count d)] 'value d}))

(defn ruru-quote? [exp] (and (map? exp) (= :quoted (:role exp))))

(def functions
  {:html (fn [x] {'html x})
   :set-diff set-diff
   :extend_dim extend-dim
   :t #(-> %1
           (assoc 'array_dims (into [] (reverse (get %1 'array_dims))))
           (assoc 'transpose (not (get %1 'transpose))))
   :mul mul
   :reshape reshape-impl
   :ones ones
   :upper_case #(list :#_string (str/upper-case (second %)))
   :lower_case #(list :#_string (str/lower-case (second %)))
   :square #(* %1 %1)
   :sqrt #(Math/sqrt %1)
   :sin Math/sin
   :cos Math/cos
   :tan Math/tan
   :+ +
   :* *
   :- -
   :/ /
   :# #(apply hash-map (% 'value))
   (keyword "@") ruru-get-index
   (keyword ":") range-impl
   :reduce (fn [coll op] (reduce op (coll 'value)))
   :filter (fn [coll pred] (if (not (fn? pred))
                             (list 'error (str pred " is not a function"))
                             (let [filtered (filterv pred (coll 'value))]
                               {'array_dims [(count filtered) 1] 'value filtered})))
   :map (fn [v f] {'array_dims (v 'array_dims) 'value (mapv f (v 'value))})
   :is_even even?
   :identity identity
   :set! :set!
   :set_into! :set_into!
   :=> :set-into!
   :list list-fn
   :first #(cond
             (ruru-array? %) (first (% 'value))
             (ruru-string? %) (list :#_string
                                    (apply str "\"" (first (subs (second %) 1)) "\""))
             :else (first %))
   :last #(cond
            (ruru-array? %) (last (% 'value))
            (ruru-string? %) (list :#_string (first (second %)))
            :else (last %))
   (keyword ";") ruru-concat
   :string #(list :#_string (str (subs (second %1) 0 (- (count (second %1)) 1))
                                 (subs (second %2) 1)))
   := =
   :! not
   :& #(and %1 %2)
   :| #(or %1 %2)
   :then (fn [pred exp] (if pred (first (exp 'value)) (second (exp 'value))))})

(def variables
  {:pi (.-PI js/Math)
   :true true
   :false false})

(def built-in-environment
  (let
   [function-context (into {} (for [[k v] functions] [k {:role :function :value v}]))
    variable-context (into {} (for [[k v] variables] [k {:role :variable :value v}]))]
    (merge function-context variable-context {:#!reader-macro {:ignore-next-form ignore-next-form-impl}})))

(def ruru-base (inline-resource "src/ruru/base/base.ruru"))
