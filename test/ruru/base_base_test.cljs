(ns ruru.base-base-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [ruru.base.base :as base]))

(deftest linear-index-test
  (testing "Get linear indeces"
    (is (= (base/ruru-linear-index [2 3] [2 2]) 4))
    (is (= (base/ruru-linear-index [4 3] [3 1]) 3))
    (is (= (base/ruru-linear-index [4 3] [1 2]) 5))
    (is (= (base/ruru-linear-index [2 3] [2 3]) 6))
    (is (= (base/ruru-linear-index [2 3] [3 3]) (list 'error "Indeces [3 3] out of bounds for array of size [2 3]")))))

(deftest get-index-test
  (testing "Get index"
    (is (= (base/ruru-get-index {'array_dims [3 2] 'value [1 2 3 4 5 6]}
                                {'array_dims [2 1] 'value [2 1]})
           2))
    (is (= (base/ruru-get-index {'array_dims [3 2] 'value [1 2 3 4 5 6]}
                                {'array_dims [2 1] 'value [4 1]})
           '(error "Indeces [4 1] out of bounds for array of size [3 2]")))
    (is (= (base/ruru-get-index {'array_dims [3 2] 'value [1 2 3 4 5 6]} 4) 4))))
