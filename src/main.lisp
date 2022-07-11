;; Copyright (c) 2021-2022 Jani J. Hakala <jjhakala@gmail.com>, Finland
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
  (:import-from :alexandria-2 :iota :remove-if)
  (:import-from :iterate :adjoining :appending :collect :finally :finish :first-iteration-p
   :for :in :iter :next-iteration :reducing :unioning)
  (:import-from :snakes :defgenerator :do-generator :generator->list :list->generator :product)
  (:use :cl)
  (:export groupby
	   main
	   print-grid))

(in-package :cl-sudoku)

(define-condition invalid-value (error)
  ((value :initarg :value
          :initform nil
          :reader valued)))

(make-condition 'invalid-value)

(defun group-cells-by-pos (cells)
  (groupby cells :keyf #'cell-pos :compf #'eqpos :valuef #'cell-value))

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

(defun eqpos (pos1 pos2)
  (declare (type pos pos1 pos2))
  (and
   (= (pos-row pos1) (pos-row pos2))
   (= (pos-col pos1) (pos-col pos2))))

(defun pos-same-row-p (pos1 pos2)
  (declare (type pos pos1 pos2))
  (= (pos-row pos1) (pos-row pos2)))

(defun pos-same-col-p (pos1 pos2)
  (declare (type pos pos1) (type pos pos2))
  (= (pos-col pos1) (pos-col pos2)))

(defun pos-same-box-p (pos1 pos2)
  (declare (type pos pos1 pos2))
  (equalp (pos-box pos1) (pos-box pos2)))

(defun pos-sees (pos1 pos2)
  (declare (type pos pos1 pos2))
  (or (pos-same-row-p pos1 pos2)
      (pos-same-col-p pos1 pos2)
      (pos-same-box-p pos1 pos2)))

(defstruct cell pos value)

(defun create-cell (row col value)
  (declare (type integer row col value))
  (make-cell :pos (make-pos :row row :col col :box (cell-box-calc row col))
             :value value))

(defun unique-cell-positions (cells)
  (remove-duplicates (mapcar #'cell-pos cells) :test #'eqpos))


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

(defun string-to-grid (string)
  (if (not (= (length string) 81))
      (error 'invalid-value :message "Grid string must be 81 chars long"))
  (let ((i 0)
        (j 0)
        (grid (make-array '(9 9) :element-type '(unsigned-byte 4)
                                 :initial-element 0)))
    (iter (for c in-string string)
      (let ((d (digit-char-p c)))
        (if (not d)
            (error 'invalid-value :message "Grid must contain only decimal digits" :value c))
        (setf (aref grid i j) d)
        (if (= j 8)
            (progn
              (setq j 0)
              (incf i))
            (incf j))))
    grid))

(defun create-candidates ()
  (iter outer (for i in (iota 9 :start 1))
    (iter (for j in (iota 9 :start 1))
      (iter (for n in (iota 9 :start 1))
        (in outer (collect (create-cell i j n)))))))

(defun remove-eliminated-candidates (candidates eliminated)
  (iter (for cell in eliminated)
    (reducing cell by (lambda (acc cell1)
                        (if (= 0 (cell-value cell1))
                            acc
                            (remove-if (lambda (cell2)
                                         (and (eqpos (cell-pos cell1)
						     (cell-pos cell2))
                                              (= (cell-value cell1)
                                                 (cell-value cell2))))
                                       acc)))
              initial-value candidates)))

(defun remove-solved-candidates (candidates solved)
  (typecase solved
    (list
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
    (array
     (iter (for pos in-generator (product (iota 9) (iota 9)))
       (reducing pos by (lambda (acc pos)
                          (let* ((value (aref solved (first pos) (second pos)))
                                 (cell1 (create-cell (1+ (first pos))
                                                     (1+ (second pos))
                                                     value)))
                            (if (= 0 value)
                                acc
                                (remove-if (lambda (cell2)
                                             (or (equalp (cell-pos cell1) (cell-pos cell2))
                                                 (and (pos-sees (cell-pos cell1)
                                                                (cell-pos cell2))
                                                      (= (cell-value cell1)
                                                         (cell-value cell2)))))
                                           acc))))
                 initial-value candidates)))))


(defstruct findresult found eliminated)

(defun finder (pred cands)
  (let ((gen (product (iota 9 :start 1)
		      (list #'candidates-get-row
			    #'candidates-get-col
			    #'candidates-get-box))))
    (iter (for (i func) in-generator gen)
      ;; (format t "funkkia ~d ~a ~%" i func)
      (finally (return (make-findresult :found found :eliminated eliminated)))
      (let* ((set (funcall func i cands))
	     (result (funcall pred set))
	     (nfound (findresult-found result))
	     (neliminated (findresult-eliminated result)))
	(if (not (null nfound))
	    (unioning nfound into found))
	(if (not (null neliminated))
	    (unioning neliminated into eliminated))))))



(defun find-naked-singles (solved cells)
  (let* ((grouped (group-cells-by-pos cells))
	 (ngrouped (remove-if (lambda (x)
				(let ((pos (car x))
				      (values (cdr x)))
				  (> (length values) 1)))
			      grouped)))
    (iter (for (pos1 . values) in ngrouped)
      (finally (return (make-findresult :found found :eliminated nil)))
      (if (= (length values) 1)
	  (let ((cell (find-if cells (lambda (pos2) (eqpos pos1 pos2) :key #'cell-pos))))
	    ;; (format t "naked single found: ~a~%" cell)
	    (adjoining cell into found))))))

(defun find-singles-in-set (cells)
  (let* ((numbers (mapcar #'cell-value cells))
	 (grouped (groupby (sort numbers #'<))))
    (iter (for (n . ns) in grouped)
      (finally (return (make-findresult :found found :eliminated nil)))
      (if (= (length ns) 1)
	  (let ((cell (find-if (lambda (value) (= n value)) cells :key #'cell-value)))
	    ;; (format t "hidden single found: ~a ~a ~a~%" cell ns cells)
	    (adjoining cell into found))))))

(defun find-hidden-singles (solved cells)
  (finder #'find-singles-in-set cells))

(defstruct solver solved candidates)

(defun solver-remove-eliminated-candidates (solver eliminated)
  (setf (solver-candidates solver) (remove-eliminated-candidates (solver-candidates solver) eliminated)))

(defun solver-remove-solved-candidates (solver solved)
  (setf (solver-candidates solver) (remove-solved-candidates (solver-candidates solver) solved)))

(defun solver-update-solved (solver solved)
  (let ((grid (solver-solved solver)))
    (iter (for cell in solved)
      (let* ((pos (cell-pos cell))
             (row (pos-row pos))
             (col (pos-col pos)))
	;; (format t "set solved ~a ~a ~a~%" row col (cell-value cell))
        (setf (aref grid (1- row) (1- col)) (cell-value cell))))))


(defun solver-solve (solver)
  (let ((candidates (solver-candidates solver))
	(solved (solver-solved solver))
	(funs (list #'find-naked-singles
		    #'find-hidden-singles))
	(exitflag nil))
    (if (null candidates)
	(solver)
	(loop
	  (if exitflag
	      (return solver))
	  (iter (for fun in funs)
	    (finally (setf exitflag t))
	    (let* ((result (funcall fun solved candidates))
		   (eliminated (findresult-eliminated result))
		   (found (findresult-found result)))
	      ;; (format t "fook: ~a ~a ~a~%" fun found eliminated)
	      (cond
		((and (null found) (null eliminated))
		 (next-iteration))
		((not (null found))
		 (solver-update-solved solver found)
		 (solver-remove-solved-candidates solver found)
		 (finish))
		((not (null eliminated))
		 (solver-remove-eliminated-candidates solver eliminated)
		 (finish)))))))))

(defun print-grid (grid)
  (write-line "+-------+-------+-------+")
  (flet ((value-char (value) (if (= 0 value) #\. (digit-char value))))
    (iter (for i in (iota 9))
      (iter (for j in (iota 9))
        (let ((val (aref grid i j)))
          (case (rem (1+ j) 3)
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
          (if (= j 8)
              (progn
                (write-char #\|)
                (terpri)
                (if (= 0 (rem (1+ i) 3))
                    (write-line "+-------+-------+-------+")))))))))

(defun main ()
  (let ((grid1 "400000938032094100095300240370609004529001673604703090957008300003900400240030709")
	(grid "014600300050000007090840100000400800600050009007009000008016030300000010009008570"))
    (print grid)
    ;; (with-input-from-string (s grid)
    ;;  (print (read-char s)))
    ;;(loop for c across grid do (princ c))))
    (let* ((solved (string-to-grid grid))
           (candidates (create-candidates))
           (ncandidates (remove-solved-candidates candidates solved))
	   (solver (make-solver :solved solved :candidates ncandidates)))
      ;; (print solved)
      ;; (print candidates)
      ;; (print ncandidates)
      (print (length ncandidates))
      (terpri)
      (print-grid solved)
      (solver-solve solver)
      (format t "solve finished with ~a left~%" (length (solver-candidates solver)))
      (print-grid (solver-solved solver))
      1)))

;; (main)
