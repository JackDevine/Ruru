(ns ruru.ruru-lang
  (:require
   [ruru.base.base :as base]
   [ruru.parser :as parser]
   [clojure.pprint :as pp]))

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

(declare ruru-unquote-quote)

(defn extract-expr [exp]
  (let [ret (map #(ruru-unquote-quote %) (get exp :value exp))]
    (if (= 1 (count ret)) (first ret)
        ret)))

(defn ruru-unquote-quote [exp]
  (cond (base/ruru-string? (get exp :value)) (get exp :value)
        (seq? exp) {'array_dims [(dec (count exp)) 1]
                    'value (mapv extract-expr (rest exp))}
        (and (map? exp) (= :variable (:role exp))) (ruru-unquote-quote (exp :name))
        (and (map? exp) (= :function (:role exp))) (ruru-unquote-quote (exp :name))
        (and (map? exp) (= :expr (:role exp))) (extract-expr exp)
        (map? exp) (ruru-unquote-quote (exp :value))
        :else exp))

(defn ruru-eval-function? [f] (= :eval f))

(defn extract-list [v]
  (mapv #(if (base/ruru-array? %) (extract-list (% 'value)) %) v))

(defn get-list-elements [l]
  (cond (and (seq? l) (= :list (first l))) (get-list-elements (rest l))
        (seq? l) (map get-list-elements l)
        :else l))

(defn extract-string-scalar [x]
  (cond (base/ruru-string? x) (subs (second x) 1 (dec (count (second x))))
        (map? x) (into {} (for [[k v] x] [(extract-string-scalar k) (extract-string-scalar v)]))
        :else x))

(defn extract-string [v]
  (mapv #(cond
           (vector? %) (extract-string %)
           :else (extract-string-scalar %)) v))

(defn ruru-expression? [exp]
  (= :expr (:role exp)))

(defn ruru-eval-exp [exp env]
  (cond (base/ruru-quote? exp) (let [unquoted (ruru-unquote-quote exp)]
                                 (ruru-eval-exp unquoted env))
        (base/ruru-array? exp) (ruru-eval-exp
                                (first (extract-list [exp]))
                                env)
        (and (sequential? exp) (= :list (first exp))) (ruru-eval (get-list-elements exp) env)
        (and (sequential? exp) (map? (first exp))) [(map #(first (ruru-eval-exp % env)) exp) env]
        (and (sequential? exp) (sequential? (first exp))) [(map #(first (ruru-eval-exp % env)) exp) env]
        (seq? exp) (let [assignment+exp (parser/get-assignment exp)
                         ast (parser/assignment+ast assignment+exp env)]
                            (ruru-eval ast env))
        :else (ruru-eval exp env)))

(defn ruru-apply [f args env]
  (cond
    (fn? f) (let [[evaled-args evaled-env] (eval-args '() args env)]
              [(apply f evaled-args) evaled-env]) 
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
  (let [exp (if (sequential? exp)
              (map exp-value exp)
              (exp-value exp))]
    (cond
      (= :environment exp) [env env]
      (and (sequential? exp) (= 1 (count exp)) (base/ruru-function? (first exp) env)) [(first exp) env]
      (and (sequential? exp) (= 1 (count exp))) (ruru-eval (first exp) env)
      (base/ruru-string? exp) [exp env]
      (base/ruru-symbol? exp) [(get-in env [exp :value] (list 'error (str "Undefined symbol " exp))) env]
      (self-evaluating? exp) [exp env]
      (empty? exp) [exp env]
      (base/ruru-quote? exp) [(ruru-unquote-quote exp) env]
      :else (ruru-apply (first exp) (rest exp) env))))

(defn filter-empty [l]
  (filter #(not (empty? %)) l))

(defn interpret-tokens [t env]
  (-> t
      (parser/run-reader-macros env)
      parser/tokenize
      parser/remove-whitespace
      parser/bind-strands
      parser/nest-parens
      parser/bind-quotes
      parser/get-assignment
      (parser/assignment+ast env)
      (ruru-eval env)))

(defn interpret-recur-impl [l result env]
  (if (empty? l) [l result env] (let [r-env (interpret-tokens (first l) env)
                                      result (first r-env)
                                      new-env (second r-env)]
                                  (interpret-recur-impl (rest l) result new-env))))

(defn interpret-recur [s env]
  (let [exp-list (parser/expression-list s)]
    (rest (interpret-recur-impl exp-list nil env))))

(defn interpret-exp-list [exp-list env]
  (reduce #(interpret-tokens %2 (second %1)) [nil env] exp-list))

(defn interpret [s env]
  (cond (string? s) (interpret-recur s env)
        :else (interpret-exp-list s env)))

(defn list->ruru-array [l]
  {'array_dims [(count l) 1]
   'value (mapv
           #(cond (sequential? %) (list->ruru-array %)
                  (string? %) (list :#_string (str "\"" % "\""))
                  :else %)
           l)})

(defn eval-expression-list
  [ruru-string]
  (let [s (subs (second ruru-string) 1 (dec (count (second ruru-string))))
        exp-list (parser/expression-list s)]
    (list->ruru-array exp-list)))

(def evaluation-functions
  {:get_first_exp #(parser/get-first-exp (second %) '() [0 0 0])
   :expression_list eval-expression-list
   :get_tokens parser/get-tokens
   :tokenize (fn [tokens] (list->ruru-array (parser/tokenize (ffirst (extract-string (extract-list [tokens]))))))
   :remove_whitespace (fn [tokens] (list->ruru-array (parser/remove-whitespace (first (extract-string (extract-list [tokens]))))))
   :bind_strands (fn [tokens] (list->ruru-array (parser/bind-strands (first (extract-string (extract-list [tokens]))))))
   :nest_parens (fn [tokens] (list->ruru-array (parser/nest-parens (first (extract-string (extract-list [tokens]))))))
   :get_ast (fn [tokens env] (list->ruru-array (parser/get-ast
                                                (first (extract-string (extract-list [tokens])))
                                                env)))
   :extract_expr (fn [tokens] (list->ruru-array (extract-expr (first (extract-string (extract-list [tokens]))))))
   :eval (fn [tokens env] (first (ruru-eval
                                  (first (extract-string (extract-list [tokens])))
                                  env)))
   :repr (fn [x] (with-out-str (pp/pprint (first (extract-string (extract-list [x]))))))})

(def default-environment
  (merge (into {} (for [[k v] evaluation-functions] [k {:role :function :value v}]))
         (second (interpret base/ruru-base base/built-in-environment))))
