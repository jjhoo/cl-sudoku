(defpackage cl-sudoku/tests/main
  (:use :cl
        :cl-sudoku
        :rove))
(in-package :cl-sudoku/tests/main)

;; (deftest test-target-1
;;  (testing "main return 1"
;;    (ok (= (main) 1))))

(deftest groupby-empty
  (testing
   "groupby empty list"
   (ok (null (groupby nil)))))

(deftest groupby-single
  (testing
   "groupby single item list"
   (let* ((lst (list (cons 1 2)))
	  (res (groupby lst :keyf #'car :valuef #'cdr)))
     (ok (equal res '((1 2)))))))

(deftest groupby
  (testing
   "groupby item list"
   (let* ((lst '((1 . 1) (1 . 2) (1 . 3) (2 . 4) (2 . 5) (2 . 6)))
	  (res (groupby lst :keyf #'car :valuef #'cdr)))
     (ok (equal res '((1 1 2 3) (2 4 5 6)))))))
