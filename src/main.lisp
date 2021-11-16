;; Copyright (c) 2021 Jani J. Hakala <jjhakala@gmail.com>, Finland
;;
;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, version 2 of the License.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU Affero General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
(defpackage :cl-sudoku
  (:import-from :iterate :collect :finally :for :in :iter :reducing)
  (:use :cl :alexandria-2)
  (:export main print-grid))

(in-package :cl-sudoku)

(define-condition invalid-value (error)
  ((value :initarg :value
          :initform nil
          :reader valued)))

(make-condition 'invalid-value)

(defstruct box row col)

(defun box-number-to-box (num)
  (let ((q (truncate (1- num) 3))
        (r (rem (1- num) 3)))
    (make-box :row (1+ q) :col (1+ r))))

(defun num-to-box-number (num)
  (1+ (truncate (1- num) 3)))

(defun cell-box-calc (row col)
  (make-box :row (num-to-box-number row) :col (num-to-box-number col)))

(defstruct pos row col box)

(defun pos-same-row-p (pos1 pos2)
  (= (pos-row pos1) (pos-row pos2)))

(defun pos-same-col-p (pos1 pos2)
  (= (pos-col pos1) (pos-col pos2)))

(defun pos-same-box-p (pos1 pos2)
  (equalp (pos-box pos1) (pos-box pos2)))

(defun pos-sees (pos1 pos2)
  (or (pos-same-row-p pos1 pos2)
      (pos-same-col-p pos1 pos2)
      (pos-same-box-p pos1 pos2)))

(defstruct cell pos value)

(defun create-cell (row col value)
  (make-cell :pos (make-pos :row row :col col :box (cell-box-calc row col))
             :value value))

(defun candidates-get-box (box cands)
  (typecase box
    (integer
     (remove-if-not (lambda (cell)
                      (equalp (box-number-to-box box) (pos-box (cell-pos cell)))) cands))
    (box
     (remove-if-not (lambda (cell)
                      (equalp box (pos-box (cell-pos cell)))) cands))))

(defun candidates-get-col (col cands)
  (remove-if-not (lambda (cell) (= col (pos-col (cell-pos cell)))) cands))

(defun candidates-get-row (row cands)
  (remove-if-not (lambda (cell) (= row (pos-row (cell-pos cell)))) cands))

(defun string-to-cells (string)
  (if (not (= (length string) 81))
      (error 'invalid-value :message "Grid string must be 81 chars long"))
  (let ((i 1) (j 1))
    (iter (for c in-string string)
      (let ((d (digit-char-p c)))
        (if (not d)
            (error 'invalid-value :message "Grid must contain only decimal digits" :value c))
        (collect (create-cell i j d))
        (if (= j 9)
            (progn
              (setq j 1)
              (incf i))
            (incf j))))))

(defun create-candidates ()
  (iter outer (for i in (iota 9 :start 1))
    (iter (for j in (iota 9 :start 1))
      (iter (for n in (iota 9 :start 1))
        (in outer (collect (create-cell i j n)))))))

(defun remove-candidates (candidates solved)
  (iter (for cell in solved)
    (reducing cell by (lambda (acc cell1)
                        (if (= 0 (cell-value cell1))
                            acc
                            (remove-if (lambda (cell2)
                                         (or (equalp (cell-pos cell1) (cell-pos cell2))
                                             (and (pos-sees (cell-pos cell1)
                                                            (cell-pos cell2))
                                                  (= (cell-value cell1)
                                                     (cell-value cell2)))))
                                       acc)))
              initial-value candidates)))

(defun print-grid (grid)
  (write-line "+-------+-------+-------+")
  (flet ((value-char (value) (if (= 0 value) #\. (digit-char value))))
    (iter (for cell in grid)
      (for i first 1 then (1+ i))
      (let ((val (cell-value cell)))
        (case (rem i 3)
          (0 (progn
               (write-char #\SPACE)
               (write-char (value-char val))
               (write-char #\SPACE)))
          (1 (progn
               (write-char #\|)
               (write-char #\SPACE)
               (write-char (value-char val))))
          (otherwise (progn (write-char #\SPACE)
                            (write-char (value-char val)))))
        (if (= 0 (rem i 9))
            (progn
              (write-char #\|)
              (terpri)))
        (if (= 0 (rem i 27))
            (write-line "+-------+-------+-------+"))
        ))))

(defun main ()
  (let ((grid "400000938032094100095300240370609004529001673604703090957008300003900400240030709"))
    (print grid)
    ;; (with-input-from-string (s grid)
    ;;  (print (read-char s)))
    ;;(loop for c across grid do (princ c))))
    (let* ((solved (string-to-cells grid))
           (x (first solved))
           (y (second solved))
           (z (third solved))
           (candidates (create-candidates))
           (ncandidates (remove-candidates candidates solved)))
      ;; (print solved)
      (print candidates)
      (print (list (cell-pos x) (cell-pos y)))
      (print (pos-same-row-p (cell-pos x) (cell-pos y)))
      (print (pos-same-col-p (cell-pos x) (cell-pos y)))
      (print (pos-same-box-p (cell-pos x) (cell-pos y)))
      (print (pos-same-box-p (cell-pos y) (cell-pos z)))
      (print ncandidates)
      (print (length ncandidates))
      (terpri)
      (print-grid solved)
      (print (candidates-get-box 9 ncandidates))
      1)))

;; (main)
