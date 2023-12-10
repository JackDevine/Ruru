(ns ruru.ruru-lang
  (:require
   [clojure.string :as str]
   [clojure.core.match :refer [match]]
   [cljs.reader]
   [ruru.base.base :as base]))

(defn assignment? [exp] (= exp :set!))
(defn assignment-into? [exp] (contains? #{:set_into! :=>} exp))

(defn self-evaluating? [exp]
  (or (base/ruru-string? exp)
      (and (seq? exp) (= 1 (count exp)))
      (and (seq? exp) (= :lambda (first exp)))
      (base/ruru-number? exp)))

(defn variable-id [x]
  (let [kx (keyword x)]
    (if (not (nil? kx)) kx x)))

(declare ruru-eval)

(defn eval-args [evaled-args args env]
  (cond (empty? args) [evaled-args env]
        :else (let [[evaled-arg evaled-env] (ruru-eval (first args) env)]
                (eval-args (concat evaled-args [evaled-arg]) (rest args) evaled-env))))

(defn ruru-set [x exp exp-role env]
  [exp (assoc env (variable-id x) {:role exp-role :value exp})])

(defn lambda? [f env]
  (cond
    (contains? #{:set! :set-into! :=>} f) false
    (seq? f) (or (= :lambda (first f)) (and (seq? (first f)) (= 1 (count f)) (= :lambda (first (first f)))))
    (contains? env f) (lambda? (get-in env [f :value] f) env)
    :else false))

(defn ruru-set-into [exp exp-role x env]
  [exp (assoc env (variable-id x) {:role exp-role :value exp})])

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

(defn exp-value [exp]
  (cond (base/ruru-quote? exp) exp
        (and (map? exp) (contains? exp :name)) (exp :name)
        (map? exp) (exp :value)
        (base/ruru-string? exp) exp
        (seq? exp) (map exp-value exp)
        :else exp))

(declare ruru-eval-quote)

(defn extract-expr [exp]
  (let [ret (map #(ruru-eval-quote %) (get exp :value exp))]
    (if (= 1 (count ret)) (first ret)
        ret)))

(defn ruru-eval-quote [exp]
  (cond (base/ruru-string? (get exp :value)) (get exp :value)
        (seq? exp) {'array_dims [(dec (count exp)) 1]
                    'value (mapv extract-expr (rest exp))}
        (and (map? exp) (= :variable (:role exp))) (ruru-eval-quote (exp :name))
        (and (map? exp) (= :function (:role exp))) (ruru-eval-quote (exp :name))
        (and (map? exp) (= :expr (:role exp))) (extract-expr exp)
        (map? exp) (ruru-eval-quote (exp :value))
        :else exp))

(defn ruru-apply [f args env]
  (cond
    (lambda? f env) (lambda-apply f (map #(first (ruru-eval %1 env)) args) env)
    (assignment? f) (let
                     [assignment-exp (rest args)
                      assignment-var (first args)
                      [evaled-exp evaled-env] (eval-args '() assignment-exp env)
                      exp-role (if (lambda? assignment-exp env) :function :variable)]
                      (ruru-set assignment-var (first evaled-exp) exp-role evaled-env))
    (assignment-into? f) (let
                          [assignment-exp (take (- (count args) 1) args)
                           assignment-var (last args)
                           [evaled-exp evaled-env] (eval-args '() assignment-exp env)
                           exp-role (if (lambda? assignment-exp env) :function :variable)]
                           (ruru-set-into (first evaled-exp) exp-role assignment-var evaled-env))
    :else (let [f-value (get-in env [f :value] (list 'error (str "Undefined function " f)))
                is-error (and (seq? f-value) (= 'error (first f-value)))
                [evaled-args evaled-env] (eval-args '() args env)]
            [(if is-error f-value (apply f-value evaled-args)) evaled-env])))

(defn ruru-eval [exp env]
  (let [exp (if (seq? exp)
              (map exp-value exp)
              (exp-value exp))]
    (cond
      (and (seq? exp) (= 1 (count exp)) (base/ruru-function? (first exp) env)) [(first exp) env]
      (and (seq? exp) (= 1 (count exp))) (ruru-eval (first exp) env)
      (base/ruru-string? exp) [exp env]
      (base/ruru-symbol? exp) [(get-in env [exp :value] (list 'error (str "Undefined symbol " exp))) env]
      (self-evaluating? exp) [exp env]
      (empty? exp) [exp env]
      (base/ruru-quote? exp) [(ruru-eval-quote exp) env]
      :else (ruru-apply (first exp) (rest exp) env))))

(def delimit-tokens #{"‿" "|" "&" "~" " " "\n" "\n\t" "\n    " "\n  " "," "[" "]" ":" ":=" "=" "=>" "+" "*" "-" "/" "(" ")" "{" "}" ";" ";=" "\"" "@" "#" "%" "!" "'"})

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
    (if (nil? rest-str)
      [(list :#_string (str "\"" first-str)) ""]
      [(list :#_string (str "\"" first-str "\"")) rest-str])))

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
    (= t "'") {:role :quote :value t}
    (strand? t) {:role :strand :value t :name (keyword t)}
    (= t ":=") {:role :assignment :value t}

    (token-string? t) {:role :variable :value t}
    (token-role-change? t) {:name (keyword (second t)) :value (second t) :role :variable :role-changed true}
    (comment? t) {:role :comment :value t}
    (contains? delimit-tokens t) {:role :function :value t :name (keyword t)}

    (number? (cljs.reader/read-string t)) {:role :number :value (cljs.reader/read-string t)}
    :else {:role (get-variable-role t) :name (keyword (str/lower-case t)) :value t}))

(defn add-start-end-impl [ts start n]
  (let [token (get ts n)
        token-value (:value token)
        token-length (cond (base/ruru-string? token-value) (count (second token-value))
                           (comment? token-value) (count (second token-value))
                           (and (:role-changed token) (= :variable (:role token))) (+ (count (str token-value)) 1) 
                           :else (count (str token-value)))
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

(defn get-ast-3-tokens [tokens env]
  (let [lt (last tokens)
        st (second tokens)
        ft (first tokens)]
    (cond
      (every? #(base/ruru-function? %1 env) tokens) `(:lambda (:x) ((~st (~ft :x) (~lt :x))))
      (base/ruru-function? lt env) (list lt (list st ft))
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
  (match (into [] (map #(base/ruru-function? %1 env) (take 3 tokens)))
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
      (every? #(base/ruru-function? %1 env) tokens) `(:lambda (:x) ((~st (~ft :x))))
      :else (list st ft))))

(defn token->symbol [token]
  (cond
    (seq? token) (deep-map token->symbol token)
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

(defn remove-whitespace [tokens]
  (filterv #(not= :whitespace (:role %1)) tokens))

(defn bind-quotes [ts]
  (cond
    (empty? ts) ts
    (= :expr (get (first ts) :role)) (concat [(assoc (first ts) :value (bind-quotes ((first ts) :value)))]
                                             (bind-quotes (rest ts)))
    (seq? (first ts)) (concat [(bind-quotes (first ts))] (bind-quotes (rest ts)))
    (= :quote (get (first ts) :role)) (concat [{:role :quoted :value (second ts)}]
                                              (bind-quotes (nthrest ts 2)))
    :else (concat [(first ts)] (bind-quotes (rest ts)))))

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

(defn assignment-token? [token]
  (= :assignment (:role token)))

(defn get-assignment [tokens]
  (let
   [split-tokens (partition-by assignment-token? tokens)
    [assign-sym assign-exp] (cond (= 1 (count split-tokens)) [() (first split-tokens)]
                                  :else [(first split-tokens) (last split-tokens)])]
    (cond (> (count assign-sym) 2) [(list 'error (str "Cannot assign into " assign-sym)) assign-exp]
          :else [(first assign-sym) assign-exp])))

(defn assignment+ast [[assign-sym assign-exp] env]
  (cond (empty? assign-sym) (get-ast assign-exp env)
        :else (list :set! assign-sym (get-ast assign-exp env))))

(defn interpret-tokens [t env]
  (-> t
      (run-reader-macros env)
      tokenize
      remove-whitespace
      bind-strands
      nest-parens
      bind-quotes
      get-assignment
      (assignment+ast env)
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

(def default-environment
  (second (interpret base/ruru-base base/built-in-environment)))
