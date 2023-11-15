(ns ruru.ruru-lang
  (:require
   [clojure.string :as str]
   [clojure.set]
   [clojure.core.match :refer [match]]
   [cljs.reader]))

(defn ignore-next-form-impl [tokens] (remove #(and (seq? %1) (= :#_ (first %1))) tokens))

(defn html? [x] (contains? x 'html))

(defn list-fn [& args] (let [val (into [] (apply list args))]
                         {'array_dims [(count val) 1] 'value val}))

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

(extend-dim {'array_dims [1 5] 'value (into [] (range 5))} [3 5])
(extend-dim {'array_dims [5 1] 'value (into [] (range 5))} [5 3])

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
        ydims (get y 'array_dims [])
        _ (println xdims)]
    (cond (not= 0 (reduce + (map empty? [xdims ydims]))) '(error "Not implemented")
          :else (mul-impl x y xdims ydims))))

(mul {'array_dims [3 3] 'value (into [] (range 9))} {'array_dims [3 3] 'value (into [] (range 9))})

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

(def default-environment
  {:html {:role :function :value (fn [x] {'html x})}
   :set_diff {:role :function :value set-diff}
   :extend_dim {:role :function :value extend-dim}
   :' {:role :function
       :value #(-> %1
                   (assoc 'array_dims (into [] (reverse (get %1 'array_dims))))
                   (assoc 'transpose (not (get %1 'transpose))))}
   :mul {:role :function
         :value mul}
   :reshape {:role :function :value reshape-impl}
   :ones {:role :function :value ones}
   :upper_case {:role :function :arity 1 :value str/upper-case}
   :lower_case {:role :function :arity 1 :value str/lower-case}
   :& {:role :function :arity 1 :value str}
   :square {:role :function :arity 1 :value #(* %1 %1)}
   :sqrt {:role :function :arity 1 :value #(Math/sqrt %1)}
   :sin {:role :function :arity 1 :value Math/sin}
   :cos {:role :function :arity 1 :value Math/cos}
   :tan {:role :function :arity 1 :value Math/tan}
   :pi {:role :variable :value (.-PI js/Math)}
   :+ {:role :function :arity 2 :value +}
   :* {:role :function :arity 2 :value *}
   :- {:role :function :arity 2 :value -}
   :/ {:role :function :arity 2 :value /}
   :# {:role :function :arity 1 :value #(apply hash-map (% 'value))}
   (keyword "@") {:role :function
                  :arity 2
                  :value ruru-get-index}
   (keyword ":") {:role :function
                  :arity 2
                  :value range-impl}
   :reduce {:role :function :arity 2 :value (fn [coll op] (reduce op (coll 'value)))}
   :filter {:role :function
            :arity 2
            :value (fn [coll pred] (let [filtered (filterv pred (coll 'value))]
                                     {'array_dims [(count filtered) 1] 'value filtered}))}
   :map {:role :function :arity 2 :value (fn [v f] {'array_dims (v 'array_dims) 'value (mapv f (v 'value))})}
   :is_even {:role :function :arity 1 :value even?}
   :identity {:role :function :arity 1 :value identity}
   :set! {:role :function :arity 2 :value :set!}
   :set_into! {:role :function :arity 2 :value :set-into!}
   :=> {:role :function :arity 2 :value :set-into!}
   :list {:role :function :value list-fn}
   :first {:role :function :value #(if (ruru-array? %) (first (% 'value)) (first %))}
   :last {:role :function :value #(last (% 'value))}
   (keyword "|") {:role :function
                  :arity 2
                  :value ruru-concat}
   :string {:role :function :arity 2 :value #(str %1 %2)}
   :#!reader-macro {:ignore-next-form ignore-next-form-impl}})

(defn ruru-symbol? [exp] (keyword? exp))
(defn ruru-string? [exp] (string? exp))
(defn ruru-number? [exp] (number? exp))
(defn assignment? [exp] (= exp :set!))
(defn assignment-into? [exp] (contains? #{:set_into! :=>} exp))

(defn self-evaluating? [exp]
  (or (ruru-string? exp)
      (and (seq? exp) (= 1 (count exp)))
      (and (seq? exp) (= :lambda (first exp)))
      (ruru-number? exp)))

(declare ruru-eval)

(defn ruru-set [x exp env]
  (let
   [result (first (ruru-eval (if (= 1 (count exp)) (first exp) (ruru-eval exp env)) env))]
    [result (assoc env (keyword x) {:role :variable :value result})]))

(defn lambda? [f env]
  (cond
    (contains? #{:set! :set-into! :=>} f) false
    (seq? f) (or (= :lambda (first f)) (and (seq? (first f)) (= 1 (count f)) (= :lambda (first (first f)))))
    (contains? env f) (lambda? (get-in env [f :value] f) env)
    :else false))

(defn ruru-set-into [exp x env]
  (let [result (first (ruru-eval (if (= 1 (count exp)) (first exp) (ruru-eval exp env)) env))
        role (if (lambda? exp env) :function :variable)]
    [result (assoc env (keyword x) {:role role :value result})]))

(defn add-variables-to-env [args data env]
  (cond
    (empty? args) env
    :else (add-variables-to-env (rest args) (rest data)
                                (assoc env (first args)
                                       {:role :variable :value (first data)}))))

(defn lambda-apply [lambda data env]
  (cond
    (empty? data) [data env]
    :else (let [lambda (cond (contains? env lambda) ((env lambda) :value) :else lambda)
                arguments (second lambda)
                body (nth lambda 2)
                lambda-env (add-variables-to-env arguments data env)
                ; TODO evaluate more than one expression and return the result of the last expression
                lambda-return (first (ruru-eval (first body) lambda-env))]
            [lambda-return env])))

(defn ruru-apply [f args env]
  (cond
    (lambda? f env) (lambda-apply f (map #(first (ruru-eval %1 env)) args) env)
    (assignment? f) (ruru-set (first args) (rest args) env)
    (assignment-into? f) (ruru-set-into (take (- (count args) 1) args) (last args) env)
    :else [(apply ((get env f {:value f}) :value) (map #(first (ruru-eval %1 env)) args)) env]))

(defn ruru-eval [exp env]
  (cond
    (self-evaluating? exp) [exp env]
    (ruru-symbol? exp) [((get env exp {:value exp}) :value) env]
    (empty? exp) [exp env]
    :else (ruru-apply (first exp) (rest exp) env)))

(def delimit-tokens #{"‿" "|" "~" " " "\n" "\n\t" "\n    " "\n  " "," "[" "]" ":" ":=" "=" "=>" "+" "*" "-" "/" "(" ")" "{" "}" ";" ";=" "\"" "@" "#" "%" "!" "'"})

(defn begins-with-delimeter? [s] (some #(str/starts-with? s %1) delimit-tokens))

(defn first-delimeter [s]
  (let [candidate-delimiters (filter #(str/starts-with? s %1) delimit-tokens)
        max-length (apply max (map count candidate-delimiters))]
    (subs s 0 max-length)))

(defn begins-with-comment? [s] (or (str/starts-with? s "%") (str/starts-with? s "%{")))

(declare get-tokens)

(defn get-comment-block-tokens [s]
  (let [balancing-token+length (if (str/starts-with? s "%{") ["}%" 2] ["\n" 1])
        balancing-token (first balancing-token+length)
        token-length (second balancing-token+length)
        comment-end (str/index-of s balancing-token)
        comment-end (cond
                      (nil? comment-end) (count s)
                      (= "\n" balancing-token) comment-end
                      :else (+ token-length comment-end))]
    (conj (get-tokens (subs s comment-end)) (list :#_ (subs s 0 comment-end)))))

(defn get-tokens [s]
  (cond (empty? s) '()
        (begins-with-comment? s) (get-comment-block-tokens s)
        (begins-with-delimeter? s) (concat [(first-delimeter s)] (get-tokens (apply str (nthrest s (count (first-delimeter s))))))
        :else (let [front-chars (take-while #(not (contains? delimit-tokens (str %1))) s)
                    front-str (apply str front-chars)
                    rest-str (apply str (take-last (- (count s) (count front-str)) s))]
                (concat (list front-str) (get-tokens rest-str)))))

(defn get-comment-block-tokens-first-exp [s]
  (let [balancing-token (if (str/starts-with? s "%{") "}%" "\n")
        comment-end (str/index-of s balancing-token)
        comment-end (cond
                      (nil? comment-end) (count s)
                      (= "\n" balancing-token) comment-end
                      :else (+ 2 comment-end))]
    [(list :#_ (subs s 0 comment-end)) (subs s comment-end)]))

(defn begins-with-string-quote? [s]
  (= \" (first s)))

(defn get-first-string [s]
  (let [[first-str rest-str] (str/split (subs s 1) #"\"" 2)]
    [(list :#_string first-str) (if (nil? rest-str) "" rest-str)]))

(defn begins-with-number? [s]
  (number? (cljs.reader/read-string (first s))))

(defn get-first-number [s]
  (let [num (apply str (take-while #(contains? #{"0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "."} %) s))]
    [num (subs s (count num))]))

(defn begins-with-role-change? [s]
  (= "~" (first s)))

(declare get-first-token)

(defn get-first-role-change [s]
  (let [[t rest-s] (get-first-token (subs s 1))]
    [(list :#_variable t) rest-s]))

(defn get-first-token [s]
  (cond (empty? s) ['() ""]
        (begins-with-string-quote? s) (get-first-string s)
        (begins-with-comment? s) (get-comment-block-tokens-first-exp s)
        (begins-with-role-change? s) (get-first-role-change s)
        (begins-with-delimeter? s) [(first-delimeter s) (subs s (count (first-delimeter s)))]
        (begins-with-number? s) (get-first-number s)
        :else (let [front-chars (take-while #(not (contains? delimit-tokens (str %1))) s)
                    front-str (apply str front-chars)
                    rest-str (subs s (count front-str))]
                [front-str rest-str])))

(defn update-paren-count [paren-count-vec ft]
  (let [v paren-count-vec]
    (cond (= ft "(") (concat [(+ (first v) 1)] (rest v))
          (= ft ")") (concat [(- (first v) 1)] (rest v))
          (= ft "[") [(first v) (+ (second v) 1) (last v)]
          (= ft "]") [(first v) (- (second v) 1) (last v)]
          (= ft "{") (concat (take 2 v) [(+ (last v) 1)])
          (= ft "}") (concat (take 2 v) [(- (last v) 1)])
          :else v)))

(defn get-first-exp [s exp paren-count-vec]
  (let [ft+rest-s (get-first-token s)
        ft (first ft+rest-s)
        rest-s (second ft+rest-s)
        paren-count (update-paren-count paren-count-vec ft)]
    (cond
      (= "" rest-s) [rest-s (concat exp (list ft)) paren-count]
      (and (= "\n" ft) (every? #(= 0 %1) paren-count)) [rest-s exp paren-count]
      (any? (map #(not= 0 %1) paren-count)) (get-first-exp rest-s (concat exp (list ft)) paren-count))))

(defn comment? [t] (and (seq? t) (= :#_ (first t))))

(defn token-string? [t] (and (seq? t) (= :#_string (first t))))

(defn token-role-change? [t] (and (seq? t) (= :#_variable (first t))))

(defn strand? [t] (str/starts-with? t "‿"))

(defn get-variable-role [t]
  (cond (= (first t) (str/upper-case (first t))) :function
        :else :variable))

(defn token-value [t]
  (cond
    (contains? #{" " "\n" "\n\t" "\n    " "\n  "} t) {:role :whitespace :value t}
    (= t "(") {:role :open-paren :value t}
    (= t ")") {:role :close-paren :value t}
    (= t "[") {:role :open-square-brace :value t}
    (= t "]") {:role :close-square-brace :value t}
    (= t "{") {:role :open-curly-brace :value t}
    (= t "}") {:role :close-curly-brace :value t}
    (strand? t) {:role :strand :value t :name (keyword t)}

    (token-string? t) {:role :variable :value t}
    (token-role-change? t) {:name (keyword (second t)) :value (second t) :role :variable :role-changed true}
    (comment? t) {:role :comment :value t}
    (contains? delimit-tokens t) {:role :function :value t :name (keyword t)}

    (number? (cljs.reader/read-string t)) {:role :number :value (cljs.reader/read-string t)}
    :else {:role (get-variable-role t) :name (keyword (str/lower-case t)) :value t}))

(defn add-start-end-impl [ts start n]
  (let [token-length (count (str (:value (get ts n))))
        new-start (+ start token-length)]
    (cond (< n (count ts)) (add-start-end-impl
                            (-> ts
                                (assoc-in [n :start] start)
                                (assoc-in [n :end] (dec new-start)))
                            new-start (inc n))
          :else ts)))

(defn add-start-end [ts]
  (add-start-end-impl ts 0 0))

(defn tokenize [ts] (add-start-end (into [] (map token-value ts))))

(defn run-reader-macros-impl [ast macros]
  (cond (empty? macros) ast
        :else (run-reader-macros-impl ((first macros) ast) (rest macros))))

(defn run-reader-macros [ast env]
  (let [reader-macros (vals (env :#!reader-macro))]
    (run-reader-macros-impl ast reader-macros)))

(defn ruru-function? [t env]
  (cond
    (seq? t) (= :lambda (-> t first :value))
    (contains? env t) (= :function (-> t env :role))
    :else (= :function (:role t))))

(defn get-ast-3-tokens [tokens env]
  (let [lt (last tokens)
        st (second tokens)
        ft (first tokens)]
    (cond
      (every? #(ruru-function? %1 env) tokens) `(:lambda (:x) ((~st (~ft :x) (~lt :x))))
      (ruru-function? lt env) (list lt (list st ft))
      :else (list st ft lt))))

(defn depth-adder [tt bt]
  (fn [s p] (+ s (cond (= p tt) 1 (= p bt) -1 :else 0))))

(defn get-paren-depth [tokens]
  (let [ft (first tokens)
        tt (:role ft)
        bt (cond
             (= tt :open-paren) :close-paren
             (= tt :open-square-brace) :close-square-brace
             (= tt :open-curly-brace) :close-curly-brace
             :else (str "Unknown paren type " tt))
        token-roles (map #(:role %1) tokens)
        paren-depth (reductions (depth-adder tt bt) 0 token-roles)]
    paren-depth))

(defn balancing-paren
  [tokens]
  (let [paren-depth (get-paren-depth tokens)]
    (count (take-while #(not= 0 %1) (rest paren-depth)))))

(defn starts-with-monad [tokens env]
  (match (into [] (map #(ruru-function? %1 env) (take 3 tokens)))
    [true true true] false
    [_ true true] true
    :else false))

(declare nest-parens)

(defn array-expressions [tokens]
  (let [expressions (take-nth 2 (partition-by #(= "," (:value %)) tokens))]
    (map (fn [exp] {:role :expr :value (nest-parens exp)}) expressions)))

(defn nest-parens [tokens]
  (cond
    (empty? tokens) tokens
    (contains? #{:open-paren :open-square-brace :open-curly-brace} (:role (first tokens)))
    (let [closing-paren-ind (balancing-paren tokens)
          tokens-in-paren (take (- closing-paren-ind 1) (rest tokens))
          tokens-outside-paren (nthrest tokens (+ closing-paren-ind 1))
          paren-type (:role (first tokens))]
      (cond
        (= paren-type :open-paren)
        (conj (nest-parens tokens-outside-paren)
              {:role :expr :value (nest-parens tokens-in-paren)})
        (= paren-type :open-curly-brace) (conj (nest-parens tokens-outside-paren)
                                               (concat (list {:role :function :value :lambda :name :lambda} (list {:role :argument :value :x} {:role :argument :value :y})) (list (list {:role :expr :value (nest-parens tokens-in-paren)}))))
        (= paren-type :open-square-brace)
        (conj (nest-parens tokens-outside-paren)
              (concat [{:role :list :value :list}] (array-expressions (nest-parens tokens-in-paren))))
        :else (str "Unknown parent type" paren-type)))
    :else (concat (list (first tokens)) (nest-parens (rest tokens)))))

(defn deep-map [f seq1]
  (cond
    (empty? seq1) nil
    (sequential? (first seq1)) (cons (deep-map f (first seq1)) (deep-map f (rest seq1)))
    :else (cons (f (first seq1)) (deep-map f (rest seq1)))))

(defn deep-mapv [f seq1]
  (cond
    (empty? seq1) nil
    (sequential? (first seq1)) (into [] (cons (deep-map f (first seq1)) (deep-map f (rest seq1))))
    :else (cons (f (first seq1)) (into [] (deep-map f (rest seq1))))))

(declare get-ast)

(defn unnest-exprs [tokens env]
  (let [inner-ast (if (= :expr (:role tokens)) (get-ast (:value tokens) env) tokens)]
    (cond
      (and (seq? inner-ast) (= 1 (count inner-ast))) (first inner-ast)
      :else inner-ast)))

(defn get-ast-2-tokens [tokens env]
  (let [ft (first tokens)
        st (second tokens)]
    (cond
      (every? #(ruru-function? %1 env) tokens) `(:lambda (:x) ((~st (~ft :x))))
      :else (list st ft))))

(defn get-ast [tokens env]
  (if
   (empty? tokens) '()
   (let [nt (count tokens)
         tokens (deep-map #(unnest-exprs %1 env) tokens)]
     (cond
       (= nt 1) tokens
       (= nt 2) (get-ast-2-tokens tokens env)
       (starts-with-monad tokens env) (let [ast-leaf (list (second tokens) (first tokens))
                                            ast-head (nthrest tokens 2)]
                                        (get-ast (conj ast-head ast-leaf) env))
       (= nt 3) (get-ast-3-tokens tokens env)
       :else (let [ast-leaf (get-ast-3-tokens (take 3 tokens) env)
                   ast-head (nthrest tokens 3)]
               (get-ast (conj ast-head ast-leaf) env))))))

(defn token->symbol [token]
  (cond
    (seq? token) (map token->symbol token)
    (not (map? token)) token
    (token-string? (:value token)) (second (:value token))
    (or (= :variable (:role token)) (= :function (:role token))) (:name token)
    (= :list (:role token)) (:value token)
    :else (:value token)))

(defn normalize-ast [ast]
  (let [symbols (deep-map token->symbol ast)]
    (if (and (= 1 (count symbols)) (seq? (first symbols)))
      (first symbols)
      symbols)))

(defn remove-whitespace [tokens]
  (filter #(not= :whitespace (:role %1)) tokens))

(defn expression-list-impl [s l]
  (if (= s "") [s l]
      (let [rest-s+first-exp+paren-count (get-first-exp s '() [0 0 0])
            rest-s (first rest-s+first-exp+paren-count)
            first-exp (second rest-s+first-exp+paren-count)]
        (expression-list-impl rest-s (concat l [first-exp])))))

(defn expression-list [s] (second (expression-list-impl s '())))

(defn first-strand-block-count [ts]
  (* 2 (count (take-while #(= :strand (get % :role)) (take-nth 2 (rest ts))))))

(defn bind-strand-block [ts strand-block-count]
  (into [] (concat [{:role :list :value :list}] (into [] (take-nth 2 (take strand-block-count ts))))))

(defn bind-strands [ts]
  (cond
    (<= (count ts) 1) ts
    (= :strand (get (second ts) :role)) (let [strand-block-count (first-strand-block-count ts)]
                                          (into
                                           []
                                           (concat
                                            [(bind-strand-block ts (inc strand-block-count))]
                                            (bind-strands
                                             (into [] (nthrest ts (inc strand-block-count)))))))
    :else (into [] (concat [(first ts)] (bind-strands (rest ts))))))

(defn highlight-selection-impl [s selection]
  [:span (subs s 0 selection)
   [:span {:style {:background-color "lightgrey" "border-left" "1px solid black"}} (get s selection)] [:span (subs s (+ 1 selection))]])

(defn selection-in-string? [s selection]
  (and (>= selection 0) (< selection (count s))))

(defn highlight-selection [s selection]
  (cond
    (and (= 1 (count s)) (= 0 selection)) [:span {:style {:background-color "lightgrey" "border-left" "1px solid black"}} s]
    (selection-in-string? s selection) (highlight-selection-impl s selection)
    :else [:span s]))

(def cell-input-style
  {:background-color "white"
   :font-family "monospace"
   :autocomplete "off"
   :autocorrect "off"
   :autocapitalize "off"
   :spellcheck "false"})

(defn token->hiccup [t selection]
  (let [selection (+ 0 (- selection (:start t)))
        val (:value t)]
    (cond
      (and (seq? val) (= :#_string (first val))) [:span {:style {:color "red"}} (highlight-selection (str "\"" (second val) "\"") selection)]
      (= "\n" (:value t)) [:br]
      (contains? #{"\n\t" "\n  " "\n    "} (:value t)) [:span "\n" [:span {:style {"border-left" "1px solid pink"}} (highlight-selection (subs (:value t) 1) selection)]]
      (= :whitespace (:role t)) [:span (highlight-selection (:value t) selection)]
      (= :function (:role t)) [:span {:style {:color "blue" :font-weight "bold"}} (highlight-selection (:value t) selection)]
      (and (:role-changed t) (= :variable (:role t))) [:span (highlight-selection (str "~" (subs (str (:name t)) 1)) selection)]
      (= :variable (:role t)) [:span (highlight-selection (subs (str (:name t)) 1) selection)]
      (= :number (:role t)) [:span {:style {:color "grey"}} (highlight-selection (str (:value t)) selection)]
      (= :comment (:role t)) [:span {:style {:font-style "italic" :color "green"}} (highlight-selection (second (:value t)) selection)]
      :else (highlight-selection (str (:value t)) selection))))


(defn get-hiccup-exp [exp selection]
  (let [tokens (into [] (tokenize exp))]
    (cond (empty? tokens) [""]
          :else (mapv #(token->hiccup %1 selection) tokens))))

(defn get-hiccup [s selection]
  (let [exp-list (if (string? s) (expression-list s) s)
        token-list (apply concat (interpose '("\n") exp-list))
        n-chars (reduce #(+ %1 (count %2)) 0 token-list)
        hiccup-val (get-hiccup-exp token-list selection)]
    (cond (< selection n-chars) hiccup-val
          :else (into [] (concat hiccup-val [[:span {:style {:background-color "lightgrey" "border-left" "1px solid black"}} " "]])))))

(defn interpret-tokens [t env]
  (-> t
      (run-reader-macros env)
      tokenize
      remove-whitespace
      bind-strands
      nest-parens
      (get-ast env)
      normalize-ast
      (ruru-eval env)))

(defn interpret-recur-impl [l result env]
  (if (empty? l) [l result env] (let [r-env (interpret-tokens (first l) env)
                                      result (first r-env)
                                      new-env (second r-env)]
                                  (interpret-recur-impl (rest l) result new-env))))

(defn interpret-recur [s env]
  (let [exp-list (expression-list s)]
    (rest (interpret-recur-impl exp-list nil env))))

(defn interpret-exp-list [exp-list env]
  (reduce #(interpret-tokens %2 (second %1)) [nil env] exp-list))

(defn interpret [s env]
  (cond (string? s) (interpret-recur s env)
        :else (interpret-exp-list s env)))
