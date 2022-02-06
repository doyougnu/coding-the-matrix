(defpackage ./tests/main
  (:use :cl
        :.
        :rove))
(in-package :./tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :.)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
