;; Copyright (c) 2022 Jani J. Hakala <jjhakala@gmail.com>, Finland
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

(in-package :cl-sudoku)

(defun groupby (items &key (keyf #'identity) (compf #'eq) (valuef #'identity))
  (let ((last nil)
	(group nil)
	(grouped nil))
    (iter (for item in items)
      (finally (if (null group)
		   (return nil)
		   (return (append grouped (list (cons last (nreverse group)))))))
      (let ((key (funcall keyf item))
	    (value (funcall valuef item)))
	(if (first-iteration-p)
	    (progn
	      (push value group)
	      (setf last key))
	    (if (funcall compf last key)
		(push value group)
		(progn
		  (collect (cons last (nreverse group)) into grouped)
		  (setf last key)
		  (setf group (cons value nil)))))))))
