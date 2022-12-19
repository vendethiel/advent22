(uiop:define-package #:day8/tests/day8
    (:use #:cl
          #:rove
          #:day8))
(in-package #:day8/tests/day8)

(deftest parse-file
  (testing "parsing a simple file"
           (ok 1)))
