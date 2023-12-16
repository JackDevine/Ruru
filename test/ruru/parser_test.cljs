(ns ruru.parser-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [ruru.parser :as parser]
            [ruru.ruru-lang :as ruru]))

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