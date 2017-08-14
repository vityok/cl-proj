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

      (assert-equal "5337279.78 4509480.41 0.00 " out-str)

      (multiple-value-bind (d m s) (pj:parse-degrees '(:d "°" :m "'" :s ) "47°7'50.09")
        (assert-equal 47 d)
        (assert-equal 7 m)
        (assert-equal 50.09 s))))

;; --------------------------------------------------------

(define-test basic-geod-test
    (let ((g (pj:make-geodesic)))
      ;; where Hwasong-12 can land when launched from Pyongyang?
      ;; (assuming its range of 4500 km
      (let ((lat 39.033333)
            (lon 125.75)
            (azi 90.0)
            ;; distance of a Hwasong-12 missile
            (dist (* 4500 1000)))
        (format t "TEST: calculating~%")
        (multiple-value-bind (lat2 lon2 azi2)
            (pj:direct-problem g lat lon azi dist)
          (declare (ignore azi2))
          (assert-equal "28.62326 173.33474"
                        (format nil "~,5f ~,5f" lat2 lon2))))

      ;; Example, determine the point 10000 km NE of JFK:
      (multiple-value-bind (lat2 lon2 azi2)
          (pj:direct-problem g 40.64 -73.78 45.0 10e6)
        (declare (ignore azi2))
        (assert-equal "32.62110 49.05249"
                      (format nil "~,5f ~,5f" lat2 lon2)))

      ;; Example, determine the distance between JFK and Singapore
      ;; Changi Airport:
      (multiple-value-bind (ps12 pazi1 pazi2)
          (pj:inverse-problem g 40.64 -73.78 1.36 103.99)
        (declare (ignore pazi1 pazi2))
        (assert-equal "15347512.990"
                      (format nil "~,3f" ps12)))
      ))

;; --------------------------------------------------------

(setf lisp-unit:*print-summary* T
      lisp-unit:*print-failures* T
      lisp-unit:*print-errors* T)
(lisp-unit:use-debugger T)

(defun run ()
  (lisp-unit:print-errors
   (lisp-unit:run-tests :all (find-package 'proj-smoke-test))))

;; EOF
