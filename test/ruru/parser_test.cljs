(ns ruru.parser-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [ruru.parser :as parser]
            [ruru.ruru-lang :as ruru]
            [ruru.base.base :as base]))

(def env ruru/default-environment)

(deftest get-tokens-test
  (testing "Tokenize test"
    (is (=
         (parser/get-tokens " [3,4]")
         '(" " "[" "3" "," "4" "]")))
    (is (=
         (parser/get-tokens "x=1+2")
         '("x" "=" "1" "+" "2")))
    (is (=
         (parser/get-tokens " 1  +  2 ")
         '(" " "1" " " " " "+" " " " " "2" " ")))
    (is (=
         (parser/get-tokens "x:=1+2")
         '("x" ":=" "1" "+" "2")))
    (is (= (parser/get-tokens "2+%{a comment}%3%a line comment\n")
           '("2" "+" (:#_ "%{a comment}%") "3" (:#_ "%a line comment") "\n")))
    (is (= (-> "2+2%{a comment}%"
               parser/get-tokens
               (parser/run-reader-macros env))
           '("2" "+" "2")))
    (is (= (-> "2+\n\t2"
               parser/get-tokens
               (parser/run-reader-macros env))
           '("2" "+" "\n\t" "2")))
    (is (= (-> "2+\n    2"
               parser/get-tokens
               (parser/run-reader-macros env))
           '("2" "+" "\n    " "2")))
    (is (= (-> "2+\n  2"
               parser/get-tokens
               (parser/run-reader-macros env))
           '("2" "+" "\n  " "2")))))

(deftest token-value-test
  (testing "Whitespace id"
    (is (= :whitespace (:role (parser/token-value " "))))
    (is (= :whitespace (:role (parser/token-value "\n"))))
    (is (= :whitespace (:role (parser/token-value "\n\t"))))
    (is (= :whitespace (:role (parser/token-value "\n    "))))
    (is (= :whitespace (:role (parser/token-value "\n  "))))))

(deftest get-first-exp-test
  (testing "Get first expression"
    (is (= (second (parser/get-first-exp "2+2" '() [0 0 0])) '("2" "+" "2")))
    (is (= (second (parser/get-first-exp "2+2\n\t3+4" '() [0 0 0])) '("2" "+" "2" "\n\t" "3" "+" "4")))
    (is (= (second (parser/get-first-exp "2+(2\n3+4)+2\n4-5" '() [0 0 0]))
           '("2" "+" "(" "2" "\n" "3" "+" "4" ")" "+" "2")))
    (is (= (second (parser/get-first-exp "2+2+[3 4\n5 6]" '() [0 0 0]))
           '("2" "+" "2" "+" "[" "3" " " "4" "\n" "5" " " "6" "]")))
    (is (= (second (parser/get-first-exp "2{x+y\nx*y}4" '() [0 0 0]))
           '("2" "{" "x" "+" "y" "\n" "x" "*" "y" "}" "4")))
    (is (= (second (parser/get-first-exp "2+%{a multi\nline\ncomment}%4" '() [0 0 0]))
           '("2" "+" (:#_ "%{a multi\nline\ncomment}%") "4")))
    (is (= (second (parser/get-first-exp "['span,\"hallo\"]" '() [0 0 0]))
           '("[" "'" "span" "," (:#_string "\"hallo\"") "]")))))

(deftest first-delimeter-test
  (testing "First delimeter test"
    (is (= (parser/first-delimeter ":=3")
           ":="))
    (is (= (parser/first-delimeter " [")
           " "))))
(deftest balancing-paren-test
  (testing "Paren balancing"
    (is (= 8
           (-> "(3*(4-5))"
               parser/get-tokens
               (parser/tokenize)
               parser/balancing-paren)))
    (is (= 10
           (-> "(3*(4-5)+2)"
               parser/get-tokens
               (parser/tokenize)
               parser/balancing-paren)))
    (is (= 4
           (parser/balancing-paren (parser/tokenize (parser/get-tokens "(2+3)+2")))))
    (is (= 4
           (-> "(3 Square)"
               parser/get-tokens
               (parser/tokenize)
               parser/balancing-paren)))))

(deftest strand-test
  (testing "Strand operator"
    (is (= (parser/bind-strands [{:role :number :value 1}
                               {:role :strand :value "‿"}
                               {:role :number :value 2}
                               {:role :function :value :+}
                               {:role :number :value 3}])
           [[{:role :list :value :list}
             {:role :number :value 1}
             {:role :number :value 2}]
            {:role :function :value :+}
            {:role :number :value 3}]))))

(deftest expression-list-test
  (testing "Expression list"
    (is (= (parser/expression-list "2+2")
           [["2" "+" "2"]]))
    (is (= (parser/expression-list "2+2\n3+4")
           [["2" "+" "2"] ["3" "+" "4"]]))
    (is (= (parser/expression-list "2+2\n3+4\n5+6")
           [["2" "+" "2"] ["3" "+" "4"] ["5" "+" "6"]]))
    (is (= (parser/expression-list "g:=~(Sqrt‿2‿3 First)+\n2")
           '(("g" ":=" (:#_variable "(") "Sqrt" "‿" "2" "‿" "3" " " "First" ")" "+")
             ("2"))))))


(comment
  (parser/expression-list "g:=~(Sqrt‿2‿3 First)+\n2")
  (-> "g:=~(Sqrt‿2‿3 First)+\n2"
      (ruru/interpret ruru/default-environment)
      second
      :g)
  )

(deftest role-change-test
  (testing "Parsing expressions that have role changes"
    (is (= (-> "4(~(Sqrt‿2‿3 First)+)"
               parser/expression-list
               first
               parser/tokenize
               parser/remove-whitespace
               parser/bind-strands
               parser/nest-parens
               parser/get-assignment
               (parser/assignment+ast env)
               (ruru/ruru-eval env)
               first)
           2))))

(deftest tacit-functions-test
  (testing "Tacit function creation"
    (is (= (parser/one-arg-tacit-function '(:f :g :h :i :j) :x)
           '(:j (:i (:h (:g (:f :x)))))))
    (is (= (parser/two-arg-tacit-function '(:f) '(:x :y))
           '(:f :x :y)))
    (is (= (parser/two-arg-tacit-function '(:f :g) '(:x :y))
           '(:g (:f :x) :y)))
    (is (= (parser/two-arg-tacit-function '(:f :g :h) '(:x :y))
           '(:h (:g (:f :x)) :y)))))

(deftest tacit-programming-test
  (testing "Tacit programming"
    (is (= (-> "Square Square"
               parser/expression-list
               first
               parser/tokenize
               parser/remove-whitespace
               parser/get-assignment
               (parser/assignment+ast env)
               ruru/exp-value)
           '(:lambda (:x) ((:square (:square :x)))
                     (:x :y) ((:square (:square :x) :y)))))
    (is (= (-> "Square Sqrt +"
               parser/expression-list
               first
               parser/tokenize
               parser/remove-whitespace
               parser/get-assignment
               (parser/assignment+ast env)
               ruru/exp-value)
           '(:lambda (:x) ((:+ (:sqrt (:square :x))))
                     (:x :y) ((:+ (:sqrt (:square :x)) :y)))))
    (is (= (-> "Square Sqrt Cos"
               parser/expression-list
               first
               parser/tokenize
               parser/remove-whitespace
               parser/get-assignment
               (parser/assignment+ast env)
               ruru/exp-value)
           '(:lambda (:x) ((:cos (:sqrt (:square :x))))
                     (:x :y) ((:cos (:sqrt (:square :x)) :y)))))
    (is (= (-> "Square Sqrt Cos Sin"
               parser/expression-list
               first
               parser/tokenize
               parser/remove-whitespace
               parser/get-assignment
               (parser/assignment+ast env)
               ruru/exp-value)
           '(:lambda (:x) ((:sin (:cos (:sqrt (:square :x)))))
                     (:x :y) ((:sin (:cos (:sqrt (:square :x))) :y)))))))
