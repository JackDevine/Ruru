(ns ruru.format-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [ruru.format :as format]
            [ruru.ruru-lang :as ruru]
            [ruru.parser :as parser]))

(def find-token-test-data (-> "2 %{a comment}% Square"
                              (parser/get-first-exp '() [0 0 0])
                              second
                              parser/tokenize))

(deftest find-selected-token
  (testing "Find the selected token with binary search"
    (is (= (format/find-selected-token find-token-test-data 0 [0 (dec (count find-token-test-data))])
           0))
    (is (= (format/find-selected-token find-token-test-data 1 [0 (dec (count find-token-test-data))])
           1))
    (is (= (format/find-selected-token find-token-test-data 2 [0 (dec (count find-token-test-data))])
           2))
    (is (= (format/find-selected-token find-token-test-data 3 [0 (dec (count find-token-test-data))])
           2))
    (is (= (format/find-selected-token find-token-test-data 15 [0 (dec (count find-token-test-data))])
           3))
    (is (= (format/find-selected-token find-token-test-data 21 [0 (dec (count find-token-test-data))])
           4))
    (is (= (format/find-selected-token find-token-test-data 22 [0 (dec (count find-token-test-data))])
           nil))
    (is (= (format/find-selected-token find-token-test-data -1 [0 (dec (count find-token-test-data))])
           nil))))