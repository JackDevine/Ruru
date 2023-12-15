;; This test runner is intended to be run from the command line
(ns ruru.test-runner
  (:require
    ;; require all the namespaces that you want to test
    [ruru.core-test]
    [ruru.ruru-lang-test]
    [ruru.base-base-test]
    [ruru.parser-test]
    [figwheel.main.testing :refer [run-tests-async]]))

(defn -main [& args]
  (run-tests-async 5000))
