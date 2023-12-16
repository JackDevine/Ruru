(ns ruru.ruru-lang-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [ruru.ruru-lang :as ruru]))

(def env ruru/default-environment)

(deftest ruru-eval-test
  (testing "ruru-apply"
    (is
     (and
      (= 0 (first (ruru/ruru-eval [:- 1 1] env)))
      (= 5 (first (ruru/ruru-eval [:+ 2 3] env)))))
    (is (= (first (ruru/ruru-eval '((:lambda (:x) ((:square :x))) 2) env))
           4))
    (is (= (first (ruru/ruru-eval '((:lambda (:x :arg2) ((:+ :arg2 :x))) 2 3) env))
           5))))

(deftest set!-test
  (testing "set! function"
    (is
     (= 5
        (:value ((second (ruru/ruru-eval '(:set! :y (:+ 2 3)) env)) :y))))))

(deftest interpret-test
  (testing "Interpret expressions"
    (is (= (-> (ruru/interpret "[1‿2,3]" ruru/default-environment) first)
           {'array_dims [2 1] 'value [{'array_dims [2 1] 'value [1 2]} 3]}))
    (is (= (first (ruru/interpret "2+2" env))
           4))
    (is (= (first (ruru/interpret "2 Square" env))
           4))
    (is (= (first (ruru/interpret "1+1 Square Square" env))
           16))
    (is (= (first (ruru/interpret "3 Square + 2" env))
           11))
    (is (= (first (ruru/interpret "1 + 2 Square" env))
           9))
    (is (= (first (ruru/interpret "1+(2*3)" env))
           7))
    (is (= (first (ruru/interpret "2+(4 Square)" env))
           18))
    (is (= (first (ruru/interpret "(2*3)" env))
           6))
    (is (= (first (ruru/interpret "(3 Square Square)" env))
           81))
    (is (= (first (ruru/interpret "(2*3-(4+6))" env))
           -4))
    (is (= (first (ruru/interpret "(2*3-(4+6))+2" env))
           -2))
    (is (= (first (ruru/interpret "[1,2,3]" env))
           {'array_dims [3 1] 'value [1 2 3]}))
    (is (= (first (ruru/interpret "[1,2,3] => arr\narr;[4,5,6]" env))
           {'array_dims [3 2] 'value [1 2 3 4 5 6]}))
    (is (= (first (ruru/interpret "[1,2,3] => arr\narr;[4,5,6]First" env))
           1))
    (is (= (first (ruru/interpret "[1,[2,4],3]" env))
           {'array_dims [3 1] 'value [1 {'array_dims [2 1] 'value [2 4]} 3]}))
    (is (= (first (ruru/interpret "[1,[2,(4+2)],3]" env))
           {'array_dims [3 1] 'value [1 {'array_dims [2 1] 'value [2 6]} 3]}))
    (is (= (first (ruru/interpret "a + 2" (assoc env :a {:role :variable :value 2})))
           4))
    (is (= (first (ruru/interpret "a + a" (assoc env :a {:role :variable :value 2})))
           4))
    (is (= (first (ruru/interpret "2{x-y*2}3" env))
           -2))
    (is (= (first (ruru/interpret "2+2 => x\n2+x" env))
           6))
    (is (= (first (ruru/interpret "2+2 => x\nx+x" env))
           8))
    (is (= (first (ruru/interpret "2+2 {x Square}" env))
           16))
    (is (= (first (ruru/interpret "{x Square}" env))
           '(:lambda (:x :y) ((:square :x)))))
    (is (= (first (ruru/interpret "{x Square} => f\n2 F" env))
           4))
    (is (= (first (ruru/interpret "2 (Square+Square)" env))
           8))
    (is (= (first (ruru/interpret "(3 Square)+(4 Square)" env))
           25))
    (is (= (first (ruru/interpret "(3 Square)+(4 Square) Sqrt" env))
           5.0))
    (is (= (first (ruru/interpret "3{(x Square)+(y Square)}4" env))
           25))
    (is (= (first (ruru/interpret "3{(x Square)+(y Square) sqrt}4" env))
           5.0))
    (is (= (first (ruru/interpret "{x Square} => f\n2 F F" env))
           16))
    (is (= (first (ruru/interpret "{x Square} => f\n2 F + 3" env))
           7))
    (is (= (first (ruru/interpret "{x Square} => f\n2 F => res" env))
           4))
    (is (= (-> (ruru/interpret "2 => x\nx+2 => var_name\nvar_name * x => foo\nfoo/(x+(var_name*3)) => bar"
                              env)
               second
               :bar
               :value)
           (/ 4 7)))
    (is (= (-> (ruru/interpret "2+2\n\tSquare => x" env) second :x :value)
           16))
    (is (= (-> (ruru/interpret "2+2\n\tSquare" env) first) 16))
    (is (= (-> (ruru/interpret "2 Square => w" env) first) 4))
    (is (= (-> (ruru/interpret "2 {x*2} => w" env) first) 4))
    (is (= (-> (ruru/interpret "2+(2\nSquare)" env) first) 6))
    (is (= (-> (ruru/interpret "[1, (2-3),\n (3+2),  4]" env) first) {'array_dims [4 1] 'value [1 -1 5 4]}))
    (is (= (-> (ruru/interpret "\"hallo\"string\" world\"" env) first) '(:#_string "\"hallo world\"")))
    (is (= (-> (ruru/interpret "1:6Reshape[2, 3]" ruru/default-environment) first) {'array_dims [2 3] 'value [1 2 3 4 5 6]}))
    (is (= (-> (ruru/interpret "1:6Reshape[3,2]@[2,2]" ruru/default-environment) first) 5))
    (is (= (-> (ruru/interpret "1:6Reshape[3,2]@6" ruru/default-environment) first) 6))
    (is (= (-> (ruru/interpret "1:6Reshape[3,2]@[4,2]" ruru/default-environment) first)
           '(error "Indeces [4 2] out of bounds for array of size [3 2]")))
    (is (= (-> (ruru/interpret "\"a string\"First" ruru/default-environment) first)
           '(:#_string "\"a\"")))
    (is (= (-> (ruru/interpret "\"a string\"Upper_case" ruru/default-environment) first)
           '(:#_string "\"A STRING\"")))
    (is (= (-> (ruru/interpret "\"A StrIng\"Lower_case" ruru/default-environment) first)
           '(:#_string "\"a string\"")))
    (is (= (-> (ruru/interpret "\"A StrIng\"First" ruru/default-environment) first)
           '(:#_string "\"A\"")))
    (is (= (-> (ruru/interpret "\"A StrIng\"@3" ruru/default-environment) first)
           '(:#_string "\"S\"")))
    (is (= (-> (ruru/interpret "'[1,2,3]" ruru/default-environment) first)
           {'array_dims [3 1] 'value [1 2 3]}))
    (is (= (-> (ruru/interpret "'pi" ruru/default-environment) first)
           :pi))
    (is (= (-> (ruru/interpret "'2" ruru/default-environment) first)
           2))
    (is (= (-> (ruru/interpret "'[2+2,3]" ruru/default-environment) first)
           {'array_dims [2 1] 'value ['(2 :+ 2) 3]}))
    (is (= (-> (ruru/interpret "'[q, [3, 5]]" ruru/default-environment) first)
           {'array_dims [2 1] 'value [:q {'array_dims [2 1] 'value [3 5]}]}))
    (is (= (-> (ruru/interpret "'(2+2)" ruru/default-environment) first)
           '(2 :+ 2)))
    (is (= (-> (ruru/interpret "'((2*3)+2)First" ruru/default-environment) first)
           '(2 :* 3)))
    (is (= (-> (ruru/interpret "'((2*3)+2)Last" ruru/default-environment) first)
           2))
    (is (= (-> (ruru/interpret "'((2*3)+2)@1" ruru/default-environment) first)
           '(2 :* 3)))
    (is (= (-> (ruru/interpret "['span,\"hallo\"]" ruru/default-environment) first)
           {'array_dims [2 1] 'value [:span '(:#_string "\"hallo\"")]}))
    (is (= (-> (ruru/interpret "('q)" ruru/default-environment) first)
           :q))
    (is (= (-> (ruru/interpret "1:6Filter is_even" ruru/default-environment) first)
           {'array_dims [3 1] 'value [2 4 6]}))
    (is (= (-> (ruru/interpret "1:20Filter is_even Map square Reduce~+" ruru/default-environment) first)
           1540))
    (is (= (-> (ruru/interpret "2 S" ruru/default-environment) first)
           '(error "Undefined function :s")))
    (is (= (-> (ruru/interpret "1:10Filter is_" ruru/default-environment) first first)
           'error))
;;     (is (= (-> (ruru/interpret "1:10Map sq" ruru/default-environment) first first)
;;            'error))
    (is (= (-> (ruru/interpret "x:=3" ruru/default-environment) first)
           3))
    (is (= (-> (ruru/interpret "x := 3+5" ruru/default-environment) second :x :value)
           8))
    (is (= (let [error-env (-> (ruru/interpret "x+y := 3+5" ruru/default-environment) second)
                 error-entry (apply (partial dissoc error-env) (keys ruru/default-environment))]
             (ffirst (keys error-entry)))
           'error))
    (is (= (-> (ruru/interpret "x := (3+5 - (2*2))" ruru/default-environment) second :x :value)
           4))
    (is (= (-> (ruru/interpret "2 => w Square => w2" ruru/default-environment) second :w :value)
           2))
    (is (= (-> (ruru/interpret "2 => w Square => w2" ruru/default-environment) second :w2 :value)
           4))
    (is (= (let
            [new-env (-> (ruru/interpret "x := 2 => w Square => w2" ruru/default-environment) second)]
             (map #(:value (second %)) (select-keys new-env [:x :w :w2])))
           [4 2 4]))
    (is (= (let
            [new-env (-> (ruru/interpret "x %{a comment}% := 2 => w Square => w2" ruru/default-environment) second)]
             (map #(:value (second %)) (select-keys new-env [:x :w :w2])))
           [4 2 4]))
    (is (= (-> (ruru/interpret "2+2=4" ruru/default-environment) first)
           true))
    (is (= (-> (ruru/interpret "1:3 Sum" ruru/default-environment) first)
           6))
    (is (= (-> (ruru/interpret "1:3 Prod" ruru/default-environment) first)
           6))
    (is (= (-> (ruru/interpret "1:3 Reverse" ruru/default-environment) first)
           {'array_dims [3 1] 'value [3 2 1]}))
    (is (= (-> (ruru/interpret "2:5 Reverse" ruru/default-environment) first)
           {'array_dims [4 1] 'value [5 4 3 2]}))
    (is (= (-> (ruru/interpret "1005:1010 Reverse" ruru/default-environment) first)
           {'array_dims [6 1] 'value [1010 1009 1008 1007 1006 1005]}))
    (is (= (-> (ruru/interpret "\"a string\" Reverse" ruru/default-environment) first)
           '(:#_string "\"gnirts a\"")))
    (is (= (-> (ruru/interpret "[+,2,2]Eval environment" ruru/default-environment) first)
           4))
    (is (= (-> (ruru/interpret "'[+,2,2]Eval environment" ruru/default-environment) first)
           4))
    (is (= (-> (ruru/interpret "[+,2,[-,3,2]]Eval environment" ruru/default-environment) first)
           3))
    (is (= (-> (ruru/interpret "[+,2+2,[-,3,2]]Eval environment" ruru/default-environment) first)
           5))
;;     (is (= (-> (ruru/interpret "'[+,2+2,[-,3,2]]Eval" ruru/default-environment) first)
;;            5))
    (is (= (-> (ruru/interpret "'[+,2,[-,3,2]]Eval environment" ruru/default-environment) first)
           3))
    (is (= (-> (ruru/interpret "'pi Eval environment" ruru/default-environment) first)
           (.-PI js/Math)))
;;     (is (= (-> (ruru/interpret "'(2+2) Eval environment" ruru/default-environment) first)
;;            4))
;;     (is (= (-> (ruru/interpret "'(2*pi) Eval" ruru/default-environment) first)
;;            (* 2 (.-PI js/Math))))
    ))

(deftest parser-evaluation-test
  (testing "Parser and evaluation functions"
    (is (= (-> (ruru/interpret "\"2 Square % comment\"Expression_list" ruru/default-environment) first)
           {'array_dims [1 1]
            'value [{'array_dims [5 1]
                     'value ['(:#_string "\"2\"") '(:#_string "\" \"")
                             '(:#_string "\"Square\"") '(:#_string "\" \"")
                             '(:#_ "% comment")]}]}))
    (is (= (-> (ruru/interpret "\"1:10Reduce~+\"Expression_list" ruru/default-environment) first)
           {'array_dims [1 1]
            'value [{'array_dims [5 1]
                     'value ['(:#_string "\"1\"") '(:#_string "\":\"")
                             '(:#_string "\"10\"") '(:#_string "\"Reduce\"")
                             {'array_dims [2 1]
                              'value [:#_variable '(:#_string "\"+\"")]}]}]}))
    (is (= (-> (ruru/interpret "\"2 Square % comment\"Expression_list Run_reader_macros environment" ruru/default-environment) first)
           {'array_dims [1 1]
            'value [{'array_dims [4 1]
                     'value ['(:#_string "\"2\"") '(:#_string "\" \"")
                             '(:#_string "\"Square\"") '(:#_string "\" \"")]}]}))
    (is (= (-> (ruru/interpret
                "\"2 Square\"Expression_list Tokenize"
                ruru/default-environment) first)
           {'array_dims [3 1]
            'value [{:role :number, :value 2, :start 0, :end 0} {:role :whitespace, :value " ", :start 1, :end 1} {:role :function, :name :square, :value "Square", :start 2, :end 7}]}))
    (is (= (-> (ruru/interpret
                "\"35 Square\"Expression_list Tokenize Remove_whitespace"
                ruru/default-environment) first)
           {'array_dims [2 1]
            'value [{:role :number, :value 35, :start 0, :end 1}
                    {:role :function, :name :square, :value "Square", :start 3, :end 8}]}))
    (is (= (-> (ruru/interpret
                "\"1‿2\"Expression_list Tokenize Remove_whitespace Bind_strands"
                ruru/default-environment) first)
           {'array_dims [1 1]
            'value [{'array_dims [3 1]
                     'value [{:role :list, :value :list}
                             {:role :number, :value 1, :start 0, :end 0}
                             {:role :number, :value 2, :start 2, :end 2}]}]}))
    (is (= (-> (ruru/interpret
                "\"1‿2\"Expression_list Tokenize Remove_whitespace Bind_strands"
                ruru/default-environment) first)
           {'array_dims [1 1]
            'value [{'array_dims [3 1]
                     'value [{:role :list, :value :list}
                             {:role :number, :value 1, :start 0, :end 0}
                             {:role :number, :value 2, :start 2, :end 2}]}]}))
    (is (= (-> (ruru/interpret
                "\"2 + (3 + 4)\"Expression_list Tokenize Remove_whitespace Bind_strands Nest_parens"
                ruru/default-environment) first)
           {'array_dims [3 1]
            'value [{:role :number, :value 2, :start 0, :end 0}
                    {:role :function, :value "+", :name :+, :start 2, :end 2}
                    {:role :expr, :value '({:role :number, :value 3, :start 5, :end 5} {:role :function, :value "+", :name :+, :start 7, :end 7} {:role :number, :value 4, :start 9, :end 9})}]}))
    (is (= (-> (ruru/interpret
                "\"2+3-4\"Expression_list Tokenize Remove_whitespace Bind_strands Nest_parens Get_ast"
                ruru/default-environment) first)
           {'array_dims [3 1]
            'value [{:role :function, :value "-", :name :-, :start 3, :end 3}
                    {'array_dims [3 1]
                     'value [{:role :function, :value "+", :name :+, :start 1, :end 1}
                             {:role :number, :value 2, :start 0, :end 0}
                             {:role :number, :value 3, :start 2, :end 2}]}
                    {:role :number, :value 4, :start 4, :end 4}]}))
    (is (= (-> "\"1:10Filter is_even Reduce~+\"Expression_list Tokenize Remove_whitespace Bind_strands Nest_parens Get_ast Eval environment"
               (ruru/interpret ruru/default-environment)
               first)
           30))))

(run-tests ruru.ruru-lang-test)