;;; Different utility functions for more convenient usage of the
;;; Proj.4 library

;; Copyright (c) 2012, Victor Anyakin <anyakinvictor@yahoo.com>
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;     * Neither the name of the organization nor the
;;       names of its contributors may be used to endorse or promote products
;;       derived from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(in-package :cl-proj)

(defun render-point (ox oy extent size &key
		     (src-cs "+proj=latlong +ellps=WGS84 +datum=WGS84")
		     (dst-cs "+proj=utm +zone=35 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
		     (debug nil))
  "Renders the point with given coordinates using the Proj API.

<p>EXTENT specifies the geographic extents of the original surface area
that is displayed within the screen with SIZE.

<p>oX Easting long?
<p>oY Northing lat?
  "

  (let* ((wgsProj (pj-init-plus src-cs))
	 (mapProj (pj-init-plus dst-cs))
	 (o.x (cffi:foreign-alloc :double))
	 (o.y (cffi:foreign-alloc :double)))

    (setf (cffi:mem-ref o.x :double) (* (float ox 0.0d0) +DEG-TO-RAD+))
    (setf (cffi:mem-ref o.y :double) (* (float oy 0.0d0) +DEG-TO-RAD+))

    (pj-transform wgsProj mapProj 1 1 o.x o.y (cffi:null-pointer))
    (pj-free wgsProj)
    (pj-free mapProj)

    (let ((oMap.width  (getf size :width))
	  (oMap.height (getf size :height))
	  (pt.x        (cffi:mem-ref o.x :double))
	  (pt.y        (cffi:mem-ref o.y :double))
	  (extent.minx (getf extent :minx))
	  (extent.miny (getf extent :miny))
	  (extent.maxx (getf extent :maxx))
	  (extent.maxy (getf extent :maxy)))

      (cffi:foreign-free o.x)
      (cffi:foreign-free o.y)

      (when debug
	(format t "[render-point]: transform: (~2,2f, ~2,2f)-(~2,2f, ~2,2f)~%" ox oy pt.x pt.y))

      (if (and (> (- extent.maxx extent.minx) 0.00001d0)
	       (> (- extent.maxy extent.miny) 0.00001d0))

	  (let* ((pixelX (/ (* oMap.width (- pt.x extent.minx))
			    (- extent.maxx extent.minx)))
		 (pixelY (- oMap.height
			    (* oMap.height (/ (- pt.y extent.miny)
					      (- extent.maxy extent.miny)))))
		 (relX (* 100 (/ pixelX oMap.width)))
		 (relY (* 100 (/ pixelY oMap.height))))

	    (when debug
	      (format t "[render-point]: pixels: (~2,2f, ~2,2f, ~2,2f, ~2,2f)~%"
		      pixelX pixelY relX relY))

	    (values pixelX pixelY relX relY pt.x pt.y))
	  (progn
	    (format t "FAIL: extent.maxx=~a, extent.minx=~a, extent.maxy=~a, extent.miny=~a~%"
		    extent.maxx extent.minx extent.maxy extent.miny)
	    (format t "      oMap.width=~a, oMap.height=~a~%" oMap.width oMap.height))))))

;;---------------------------------------------------------

(defun dec-to-merc-ex ()
  "Example program from the Proj.4 API documentation.

<p>The following program reads latitude and longitude values in
decimal degress, performs Mercator projection with a Clarke 1866
ellipsoid and a 33° latitude of true scale and prints the projected
cartesian values in meters.

<p>For this program, an input of '-16 20.25' would give a result of
'-1495284.21 1920596.79'.

<p>Program sources can be found at: http://trac.osgeo.org/proj/wiki/ProjAPI"

  (let ((pj-merc (pj-init-plus "+proj=merc +ellps=clrk66 +lat_ts=33"))
	(pj-latlong (pj-init-plus "+proj=latlong +ellps=clrk66"))
	(p.x (cffi:foreign-alloc :double))
	(p.y (cffi:foreign-alloc :double)))

    (setf (cffi:mem-ref p.x :double)
	  (* (float (read) 0.0d0) +DEG-TO-RAD+))
    (setf (cffi:mem-ref p.y :double)
	  (* (float (read) 0.0d0) +DEG-TO-RAD+))

    (pj-transform pj-latlong pj-merc 1 1 p.x p.y (cffi:null-pointer))

    (format t "~,2f  ~,2f~%" (cffi:mem-ref p.x :double) (cffi:mem-ref p.y :double))

    (cffi:foreign-free p.x)
    (cffi:foreign-free p.y)
    (pj-free pj-merc)
    (pj-free pj-latlong)))

;; (in-package :cl-proj)(dec-to-merc-ex)

;; EOF