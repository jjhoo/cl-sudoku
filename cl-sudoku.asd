(defsystem "cl-sudoku"
  :version "0.1.0"
  :author "Jani Hakala"
  :license "GPL-2"
  :depends-on ("alexandria"
               "iterate"
               "snakes")
  :components ((:module "src"
                :components
                ((:file "main")
		 (:file "utils"))))
  :description "Sudoku solver"
  :in-order-to ((test-op (test-op "cl-sudoku/tests"))))

(defsystem "cl-sudoku/tests"
  :author "Jani Hakala"
  :license "GPL-2"
  :depends-on ("cl-sudoku"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-sudoku"
  :perform (test-op (op c) (symbol-call :rove :run c)))
