(defpackage cl-sudoku/tests/main
  (:use :cl
        :cl-sudoku
        :rove))
(in-package :cl-sudoku/tests/main)

(deftest test-target-1
  (testing "main return 1"
    (ok (= (main) 1))))
