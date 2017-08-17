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

      ;; Example, compute way points between JFK and Singapore Changi
      ;; Airport. Test DIRECT-PROBLEM and GEO-POSITION
      (let ((str-a
             ;; the 'obvious' way using GEO-DIRECT:
             (with-output-to-string (out-a)
               (let ((g (pj:make-geodesic)))
                 (multiple-value-bind (s12 azi1 azi2)
                     (pj:inverse-problem g 40.64 -73.78 1.36 103.99)
                   (declare (ignore azi2))
                   (loop :for i :from 0 :to 100
                      :do (multiple-value-bind (lati loni azii)
                              (pj:direct-problem g 40.64 -73.78 azi1 (* i s12 0.01))
                            (declare (ignore azii))
                            (format out-a "~,5f ~,5f~%" lati loni)))))))
            (str-b
             ;; A faster way using GEOD-POSITION:
             (with-output-to-string (out-b)
               (let ((g (pj:make-geodesic)))
                 (multiple-value-bind (s12 azi1 azi2)
                     (pj:inverse-problem g 40.64 -73.78 1.36 103.99)
                   (declare (ignore azi2))
                   (let ((l (pj:make-geo-line g 40.64 -73.78 azi1)))
                     (loop :for i :from 0 :to 100
                        :do (multiple-value-bind (lati loni azii)
                                (pj:geo-position l (* s12 i 0.01))
                              (declare (ignore azii))
                              (format out-b "~,5f ~,5f~%" lati loni))))))))
            
            (str-c
             ;; An even faster way using GENERAL-POSITION
             (with-output-to-string (out-c)
               (let ((g (pj:make-geodesic)))
                 (multiple-value-bind (a12 ps12 azi1 azi2)
                     (pj:general-inverse-problem g 40.64 -73.78 1.36 103.99)
                   (declare (ignore ps12) (ignore azi2))
                   (let ((l (pj:make-geo-line g 40.64 -73.78 azi1 '(:latitude :longitude))))
                     (loop :for i :from 0 :to 100
                        :do (multiple-value-bind (s12 lati loni)
                                (pj:general-position l '(:arcmode) (* i a12 0.01))
                              (declare (ignore s12))
                              ;; due to floating-point and other
                              ;; calculation errors compare to the
                              ;; reference data produced by the C
                              ;; version. To reduce amount of text
                              ;; limit to first and last 3 values
                              (when (or (< i 3) (> i 97))
                                (format out-c "~,3f ~,3f~%" lati loni))))))))))
        (assert-equal str-a str-b)
        (assert-nil (string/= str-c
                              "40.640 -73.780
42.019 -73.673
43.397 -73.562
4.128 103.869
2.744 103.930
1.360 103.990
")))

      ;; Example, compute the area of Antarctica:
      (let ((g (pj:make-geodesic))
            (lats '(-72.9 -71.9 -74.9 -74.3 -77.5 -77.4 -71.7 -65.9 -65.7
                    -66.6 -66.9 -69.8 -70.0 -71.0 -77.3 -77.9 -74.7))
            (lons '(-74 -102 -102 -131 -163 163 172 140 113
                    88 59 25 -4 -14 -33 -46 -61)))
        (multiple-value-bind (count pa pp)
            (pj:polygon-area g lats lons)
          (declare (ignore count))
          ;; Area in m^2 and perimeter in meters
          (assert-equal "13376855976704.344 14710425.494044874"
                        (format nil "~f ~f" pa pp))))
      ))

;; --------------------------------------------------------

(setf lisp-unit:*print-summary* T
      lisp-unit:*print-failures* T
      lisp-unit:*print-errors* T)
(lisp-unit:use-debugger T)

(defun run ()
  (format t "Proj4 version: ~a, release: ~a~%" pj:+pj-version+ pj:pj-release)
  (lisp-unit:print-errors
   (lisp-unit:run-tests :all (find-package 'proj-smoke-test))))

;; EOF
