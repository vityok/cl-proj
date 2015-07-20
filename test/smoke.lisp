;; Smoke-testing the bindings for the PROJ.4 library bindings

;; sbcl --load 'smoke.lisp' --eval '(proj-smoke-test:run)' --quit
;; lx86cl --load 'smoke.lisp' --eval '(proj-smoke-test:run)' --eval '(quit)'
;; ecl -load 'smoke.lisp' -eval '(proj-smoke-test:run)' -eval '(quit)'

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-proj)
  (ql:quickload :lisp-unit))

(defpackage :proj-smoke-test
  (:use :cl :lisp-unit)
  (:export :run))

(in-package :proj-smoke-test)

;; --------------------------------------------------------

(define-test basic-test
    (let* ((from-proj (pj:pj-init-plus "+proj=latlong +datum=WGS84"))
	   (to-proj (pj:pj-init-plus "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
	   (out (pj:geo-transform from-proj to-proj '((47.9456d0 37.5032d0 0.0d0)) :degs T))
	   (out-str (format nil "~{~,2f ~}" (car out))))

      (assert-equal "5337279.78 4509480.41 0.00 " out-str)))

;; --------------------------------------------------------

(setf lisp-unit:*print-summary* T
      lisp-unit:*print-failures* T
      lisp-unit:*print-errors* T)
(lisp-unit:use-debugger T)

(defun run ()
  (lisp-unit:print-errors
   (lisp-unit:run-tests :all (find-package 'proj-smoke-test))))

;; EOF
