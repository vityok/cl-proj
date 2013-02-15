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

;;---------------------------------------------------------

(defun render-point (ox oy extent size &key
		     (src-cs "+proj=latlong +ellps=WGS84 +datum=WGS84")
		     (dst-cs "+proj=utm +zone=35 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
		     (debug nil))
  
  "Renders the point with given coordinates using the Proj API and
adjusts it to be displayed within a screen with given width and height.

@arg[extent]{specifies the geographic extents of the original surface
 area that is displayed within the screen with given @code{size}.

@code{extent} is a plist with properties (:minx :maxx :miny :maxy)
 specifying geographic bounds in the source geographic system.}

@arg[size]{is a plist specifying (:width and :height) of the resulting
 image in pixels.}

@arg[oX]{Easting long?}
@arg[oY]{Northing lat?}

@arg[src-cs]{Proj.4 string specifying the source coordinate system}
@arg[dst-cs]{Proj.4 string specifying the destination coordinate system}

@begin{return}

In case of success returns multiple values:

@code{pixelX pixelY relX relY pt.x pt.y}

Where @code{pixelX} and @code{pixelY} are coordinates of the given
point in pixels within given screen; @code{relX} and @code{relY} is a
relative position of the given point; @code{pt.X} and @code{pt.Y} are
rendered coordinates of the given point.

@end{return}"

  (let* ((wgsProj (pj-init-plus src-cs))
	 (mapProj (pj-init-plus dst-cs))
	 (o.x (cffi:foreign-alloc :double :count 3))
	 (o.y (cffi:foreign-alloc :double :count 3)))

    (setf (cffi:mem-aref o.x :double 0) (* (float ox 0.0d0) +DEG-TO-RAD+))
    (setf (cffi:mem-aref o.y :double 0) (* (float oy 0.0d0) +DEG-TO-RAD+))

    (setf (cffi:mem-aref o.x :double 1) (* (float (getf extent :minx) 0.0d0) +DEG-TO-RAD+))
    (setf (cffi:mem-aref o.y :double 1) (* (float (getf extent :miny) 0.0d0) +DEG-TO-RAD+))

    (setf (cffi:mem-aref o.x :double 2) (* (float (getf extent :maxx) 0.0d0) +DEG-TO-RAD+))
    (setf (cffi:mem-aref o.y :double 2) (* (float (getf extent :maxy) 0.0d0) +DEG-TO-RAD+))

    (pj-transform wgsProj mapProj 3 1 o.x o.y (cffi:null-pointer))
    (pj-free wgsProj)
    (pj-free mapProj)

    (let ((oMap.width  (getf size :width))
	  (oMap.height (getf size :height))
	  (pt.x        (cffi:mem-aref o.x :double 0))
	  (pt.y        (cffi:mem-aref o.y :double 0))
	  (extent.minx (cffi:mem-aref o.x :double 1))
	  (extent.miny (cffi:mem-aref o.y :double 1))
	  (extent.maxx (cffi:mem-aref o.x :double 2))
	  (extent.maxy (cffi:mem-aref o.y :double 2)))

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

	    (when (or debug (< pixelX 0))
	      (format t "[render-point]: transform: (~2,2f, ~2,2f)-(~2,2f, ~2,2f)~%" ox oy pt.x pt.y)
	      (format t "[render-point]: pixels: (~2,3f, ~2,3f)~%"
		      pixelX pixelY))

	    (values pixelX pixelY relX relY pt.x pt.y))

	  (progn
	    (format t "FAIL: extent.maxx=~a, extent.minx=~a, extent.maxy=~a, extent.miny=~a~%"
		    extent.maxx extent.minx extent.maxy extent.miny)
	    (format t "      pt.x=~a, pt.y=~a~%" pt.x pt.y)
	    (format t "      oMap.width=~a, oMap.height=~a~%" oMap.width oMap.height))))))

;;---------------------------------------------------------

(defun dec-to-merc-ex ()
  "Example program from the Proj.4 API documentation.

The following program reads latitude and longitude values in decimal
degress, performs Mercator projection with a Clarke 1866 ellipsoid and
a 33° latitude of true scale and prints the projected cartesian values
in meters.

For this program, an input of '-16 20.25' would give a result of
'-1495284.21 1920596.79'.

Program sources can be found at:
@a[http://trac.osgeo.org/proj/wiki/ProjAPI]{the ProjAPI page}"

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

;;---------------------------------------------------------

(defun dms-to-dec (deg &optional (min 0d0) (sec 0d0))
  "Converts a degree-minute-second representation to decimal
degrees.

For example, to convert 47°7'50.09 to decimal representation, call
this function with following parameters:

@begin{code}
  (dms-to-dec 47 7 50.9) => 47.130802
@end{code}
"
  (float
   (+ deg
      (/ min 60d0)
      (/ sec
	 3600d0))))

;;---------------------------------------------------------

(defun parse-degrees (pattern str &key dec)
  "Utility function to parse lines like: 47°7'50

If an optional key DEC is set to true converts parsed data into
decimal representation with DMS-TO-DEC function."
  
  (let ((offset 0)
	(degs)
	(mins)
	(secs))
    (dolist (pat pattern)
      (if (stringp pat)
	  (setf offset (+ offset (length pat)))
	  (case pat
	    (:d (multiple-value-bind (digit chars)
		    (parse-integer str :start offset :junk-allowed T)
		  (setf degs digit)
		  (setf offset chars)))
	    (:m (multiple-value-bind (digit chars)
		    (parse-integer str :start offset :junk-allowed T)
		  (setf mins digit)
		  (setf offset chars)))
	    (:s (multiple-value-bind (digit chars)
		    (parse-integer str :start offset :junk-allowed T)
		  (setf secs digit)
		  (setf offset chars)))
	    (otherwise (error "unknown pattern")))))
    (if dec
	(dms-to-dec degs mins secs)
	(values degs mins secs))))

;; (parse-degrees '(:d "°" :m "'" :s ) "47°7'50.09") => 47 7 50.09

;; EOF