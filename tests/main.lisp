(defpackage cl-sudoku/tests/main
  (:import-from :alexandria-2 :copy-array)
  (:use :cl
	:cl-sudoku
	:rove))
(in-package :cl-sudoku/tests/main)

(defun in-solved (grid expected)
  (every (lambda (val)
	   (let ((row (nth 0 val))
		 (col (nth 1 val))
		 (value (nth 2 val)))
	     (= value (aref grid (1- row) (1- col)))))
	 expected))

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

(deftest solve-singles
  (testing
   "solve hidden singles"
   (let* ((grid "014600300050000007090840100000400800600050009007009000008016030300000010009008570")
	  (solver (solver-from-string grid))
	  (nsolver (solver-solve solver))
	  (nsolved (solver-solved nsolver))
	  (expected '((4 1 9) (5 2 8) (6 5 8) (8 6 4) (5 7 7) (9 1 1) (8 9 8))))
     (ok (in-solved nsolved expected)))))
