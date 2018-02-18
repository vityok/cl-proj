;;; Different utility functions for more convenient usage of the
;;; Proj.4 library

;; Copyright (c) 2012-2018 Victor Anyakin
;; <anyakinvictor@yahoo.com> All rights reserved.

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
;; DISCLAIMED. IN NO EVENT SHALL COPYRIGHT HOLDER BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(in-package :cl-proj)

;;---------------------------------------------------------

(defmacro deg-to-rad (val)
  "Convert given value from degrees to radians."
  `(* ,val +DEG-TO-RAD+))

;;---------------------------------------------------------

(defun deg-to-rad-array (x len)
  "Converts numbers in the given native array of length LEN from
degrees to radians and stores them in the same array."
  (dotimes (i len)
    (setf (cffi:mem-aref x :double i)
          (deg-to-rad (cffi:mem-aref x :double i)))))

;;---------------------------------------------------------

(defun geo-transform (src dst points &key (degs nil))
  "@short{Transform between coordinate systems.}

 The GEO-TRANSFORM function may be used to transform points between
 the two provided coordinate systems.  In addition to converting
 between cartographic projection coordinates and geographic
 coordinates, this function also takes care of datum shifts if
 possible between the source and destination coordinate system.
 Unlike @fun{PJ-FWD} and @fun{PJ-INV} it is also allowable for the
 coordinate system definitions (PJ *) to be geographic coordinate
 systems (defined as +proj=latlong).  The x, y and z arrays contain
 the input values of the points, and are replaced with the output
 values.  The point_offset should indicate the spacing the of x,y,z
 arrays, normally 1.  The function returns zero on success, or the
 error number (also in @variable{pj-errno}) on failure.

 The z array may be passed as NULL if Z values are not available.

 @arg[src]{source (input) coordinate system.}
 @arg[dst]{destination (output) coordinate system.}
 @arg[points]{A list of X, Y and Z coordinate triple values.}
 @arg[digs]{Set this T if source coordinates are degrees.}

 @return{The return is zero on success, or a PROJ.4 error code.}

 Memory associated with the projection may be freed with @fun{pj-free}."

  (let ((len (length points)))
    (cffi:with-foreign-objects ((x :double len)
				(y :double len)
				(z :double len))
      (if degs
	  (dotimes (i len)
	    (setf (cffi:mem-aref x :double i) (deg-to-rad (first (nth i points)))
		  (cffi:mem-aref y :double i) (deg-to-rad (second (nth i points)))
		  (cffi:mem-aref z :double i) (deg-to-rad (third (nth i points)))))
	  (dotimes (i len)
	    (setf (cffi:mem-aref x :double i) (first (nth i points))
		  (cffi:mem-aref y :double i) (second (nth i points))
		  (cffi:mem-aref z :double i) (third (nth i points)))))

      (pj-transform src dst len 1 x y z)

      (loop :for i :from 0 :below len
	 :collect (list (cffi:mem-aref x :double i)
			(cffi:mem-aref y :double i)
			(cffi:mem-aref z :double i))))))
(export 'geo-transform)

;;---------------------------------------------------------

(defun render-point (ox oy extent size &key
		     (src-cs "+proj=latlong +ellps=WGS84 +datum=WGS84")
		     (dst-cs "+proj=utm +zone=35 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
		     (debug nil))

  "@short{Renders the point with given coordinates using the Proj API and
 adjusts it to be displayed within a screen with given width and height.}

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

    (setf (cffi:mem-aref o.x :double 0) (deg-to-rad (float ox 0.0d0))
	  (cffi:mem-aref o.y :double 0) (deg-to-rad (float oy 0.0d0)))

    (setf (cffi:mem-aref o.x :double 1) (deg-to-rad (float (getf extent :minx) 0.0d0))
	  (cffi:mem-aref o.y :double 1) (deg-to-rad (float (getf extent :miny) 0.0d0)))

    (setf (cffi:mem-aref o.x :double 2) (deg-to-rad (float (getf extent :maxx) 0.0d0)))
    (setf (cffi:mem-aref o.y :double 2) (deg-to-rad (float (getf extent :maxy) 0.0d0)))

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

(defun perpendicular-distance (point line1 line2)
  "@short{Calculates a distance from a point to the line specified by two
 points on a Discartes surface.}"

  (let ((x_0 (first point))
	(y_0 (second point))
	(x_1 (first line1))
	(y_1 (second line1))
	(x_2 (first line2))
	(y_2 (second line2)))
    (if (= x_1 x_2)			; line is vertical
	(float (abs (- x_0 x_1)))
	(if (= y_1 y_2)			; line is horizontal
	    (float (abs (- y_0 y_1)))
	    ;; general case
	    (/ (abs (- (* (- x_2 x_1)
			  (- y_1 y_0))
		       (* (- x_1 x_0)
			  (- y_2 y_1))))
	       (sqrt (+ (expt (- x_2 x_1) 2)
			(expt (- y_2 y_1) 2))))))))

;;---------------------------------------------------------

(defun simplify (points epsilon)
  "@short{Simplifies the given polyline using the Ramer–Douglas–Peucker
 algorithm.}

Given a curve composed of line segments, this function finds and
returns a similar curve with fewer points. The algorithm defines
'dissimilar' based on the maximum distance between the original curve
and the simplified curve. The simplified curve consists of a subset of
the points that defined the original curve.

Check the
 @a[http://en.wikipedia.org/wiki/Ramer–Douglas–Peucker_algorithm]{Ramer–Douglas–Peucker
 algorithm} article on Wikipedia.

@arg[points]{coordinates (x y) of points that specify the polyline}

@arg[epsilon]{distance}

@return{Given a curve composed of line segments, this function finds
 and returns a similar curve with fewer points.}
"
  (if (> (length points) 2)
      (if (and (= (first (first points))
		  (first (car (last points))))
	       (= (second (first points))
		  (second (car (last points)))))
	  (concatenate 'list
		       `(,(first points))
		       (simplify (subseq points 1) epsilon))

	  (let ((dmax 0)
		(index 0))
	    ;; Find the point with the maximum distance
	    (loop for i from 1 below (- (length points) 1)
	       for distance = (perpendicular-distance (nth i points)
						      (first points)
						      (car (last points)))
	       then
	       (perpendicular-distance (nth i points)
				       (first points)
				       (car (last points)))
	       when (> distance dmax)
	       do (setf index i
			dmax distance))

	    (if (>= dmax epsilon)
		;; If max distance is greater than epsilon, recursively simplify
		(let ((rec-results1 (simplify (subseq points 0 index) epsilon))
		      (rec-results2 (simplify (subseq points index)   epsilon)))
		  (concatenate 'list rec-results1 rec-results2))

		`(,(first points) ,(car (last points))))))
      ;; there are only two points nothing to simplify
      points))



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
	  (deg-to-rad (float (read) 0.0d0)))
    (setf (cffi:mem-ref p.y :double)
	  (deg-to-rad (float (read) 0.0d0)))

    (pj-transform pj-latlong pj-merc 1 1 p.x p.y (cffi:null-pointer))

    (format t "~,2f  ~,2f~%" (cffi:mem-ref p.x :double) (cffi:mem-ref p.y :double))

    (cffi:foreign-free p.x)
    (cffi:foreign-free p.y)
    (pj-free pj-merc)
    (pj-free pj-latlong)))

;;---------------------------------------------------------

(defun dms-to-dec (deg &optional (min 0d0) (sec 0d0))
  "@short{Converts a degree-minute-second representation to decimal
 degrees.}

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

(defun parse-degrees (pattern str &key dec (start 0))
  "@short{Utility function to parse string representation of angles
 like: 47°7'50}

 PATTERN is a list that consists of strings and keywords: where :D
 stands for degrees, :M for minutes and :S for seconds.

 For instance, next command parses 47°7'50.09:

@begin{code}
 (parse-degrees '(:d \"°\" :m \"'\" :s ) \"47°7'50.09\") => 47 7 50.09
@end{code}

 If an optional key DEC is set to True, returns decimal representation
 of the parsed angle."
  (let ((offset start)
	(degs 0d0)
	(mins 0d0)
	(secs 0d0))
    (dolist (pat pattern)
      (if (stringp pat)
	  (setf offset (+ offset (length pat)))
	  (case pat
	    ;; degrees
	    (:d (multiple-value-bind (digit chars)
		    (parse-integer str :start offset :junk-allowed T)
		  (setf degs digit)
		  (setf offset chars)))
	    ;; minutes
	    (:m (multiple-value-bind (digit chars)
		    (parse-integer str :start offset :junk-allowed T)
		  (setf mins digit)
		  (setf offset chars)))
	    ;; seconds
	    (:s (let ((end offset))
		  (loop for i from offset below (length str)
                     while (or (digit-char-p (char str i))
                               (char= #\. (char str i)))
                     do (incf end))
		  (multiple-value-bind (digit)
		      (parse-number:parse-number str :start offset :end end)
		    (setf secs digit)
		    (setf offset end))))
            ;; any non-numeric characters are skipped
            (:any-chars
             (loop while (and (not (digit-char-p (char str offset)))
                              (< offset (length str)))
                do (incf offset)))
	    (otherwise (error "unknown pattern: ~a" pat)))))
    (if dec
	(values (dms-to-dec degs mins secs) offset)
	(values degs mins secs offset))))

;; (parse-degrees '(:d "°" :m "'" :s ) "47°7'50.09") => 47 7 50.09

;;---------------------------------------------------------

(defparameter *cross-threshold* (float 1e-5 0.0d0))

(defun antimeridian-crossing (g lat lon azi &optional distance)
  "Given a line defined by the point with the specified latitude and
longitude, and an azimuth, calculate coordinates of its intersection
with the antimeridian.

  (defvar *g* (pj:make-geodesic))
  (antimeridian-crossing *g* 39.033333 125.75 90.0 (* 13000 1000))
  ;; => 25.279533446330998d0 179.99999844054264d0 23

"
  ;; the way we are going to solve it is to descend on the approximate
  ;; solution: given the distance to the point on the other side find
  ;; coordinates of a point at 1/2 the distance. If it happens on our
  ;; hemisphere seek to the right, if it is on the opposite side seek
  ;; to the left. Continue until longitude is close enough to the
  ;; antimeridian

  ;; as we know the exact value of the longitude (+-180 depending on
  ;; the hemisphere) what we really want to know is approximate value
  ;; of the latitude

  ;; (format t "cross: ~,2f ~,2f ~,2f ~a~%" lat lon azi distance)
  (iter
    (with next-distance = distance)
    (with step = (float (* distance 0.5d0)))
    (for iteration from 0 below 55)
    ;; (format t "next-distance: ~9f~%" next-distance)
    (multiple-value-bind (lat* lon* azi*)
        (pj:direct-problem g lat lon azi next-distance)
      (declare (ignore azi*))
      (when (< (abs (- (abs lon*) 180)) *cross-threshold*)
        (return-from antimeridian-crossing (values lat* lon* iteration)))
      ;; (format t "remaining delta: ~a~%" (abs (- (abs lon*) 180)))
      (if (< lon* 0) ;; western hemisphere
          (progn
            ;; (format t "shorten distance: lat=~,3f&lon=~,3f~%" lat* lon*)
            (setf next-distance (- next-distance step)
                  step (float (* step 0.5d0))))
          (progn
            ;; (format t "increase distance: lat=~,3f&lon=~,3f~%" lat* lon*)
            (setf next-distance (+ next-distance step)
                  step (float (* step 0.5d0))))))
    (finally (format t "failed after ~a iterations: ~,2f ~,2f ~,2f~%" iteration lat lon azi)))
  ;;  (format t "failed to find crossing")
  )

;;---------------------------------------------------------

(defun missile-range (lat lon radius &key (count 360) (mode :one) (out T))
  "Produce a GeoJSON Polygon circumscribing the ciricle with the
given radius (in meters) with the given center.

Count specifies the number of points the Polygon will contain, the
higher the number, the smoother the circle.

Example: area within Hwasong-12 missile range when fired from
Pyongyang? NB: here we assume the range to be 4500, however, Wikipedia
mentions estimates within range of 3700-6000 km.

  (with-open-file (out \"range.json\" :direction :output :if-exists :supersede)
    (missile-range 39.033333 125.75 (* 4500 1000) :out out))

The resulting GeoJSON file can be converted to other formats (like
Google Earth-compatible KML) with the ogr2ogr command-line
application:

  ogr2ogr -f KML range.kml range.json

Since the range can be large enough to cross antimeridian, the
function takes care to follow GeoJSON specification, section 3.1.9
antimeridian cutting and splits the polygon in two pieces (when MODE
is :SPLIT).
"

  ;; As per RFC 7946: A linear ring MUST follow the right-hand rule
  ;; with respect to the area it bounds, i.e., exterior rings are
  ;; counterclockwise, and holes are clockwise.

  ;; At the same time section 3.1.9. "Antimeridian Cutting" says that:
  ;; In representing Features that cross the antimeridian,
  ;; interoperability is improved by modifying their geometry. Any
  ;; geometry that crosses the antimeridian SHOULD be represented by
  ;; cutting it in two such that neither part's representation crosses
  ;; the antimeridian.

  ;; The antimeridian is located at longitude +-180 depending on the
  ;; direction you are approaching it: it is +180 when approached from
  ;; the Western hemisphere and -180 from the Eastern.

  ;; the code below collects calculated points in two baskets: eastern
  ;; and western. If the circle crosses the antimeridian both of them
  ;; will be output as separate polygons in a MultiPolygon.

  ;; lon is x, lat is y
  (let ((g (pj:make-geodesic))
        (all '())
        (eastern '())
        (western '())
	(amc '()) ; amc - points on the antimeridian
        (split (eql mode :split))
        (sectors (eql mode :sectors)))
    (iter
      (with step = (/ 360 count))
      (for azi from 0 below 360 by step)
      (multiple-value-bind (lat2 lon2 azi2)
	  (pj:direct-problem g lat lon azi radius)
	(declare (ignore azi2))
	(if split
	    ;; todo: when splitting into hemispheres the point from
	    ;; another hemisphere not only should be added there,
	    ;; but also the point on the antimeridian at the point
	    ;; of interesection with the radial arc should be added
	    ;; to this hemisphere. This should correctly specify
	    ;; the bound of the polygon of the area remaining in
	    ;; the current hemisphere and the chunk on the other
	    ;; end
	    (if (and (and (> azi 0)
			  (< azi 180))
		     (< lon2 0))
		;; point on the western hemisphere
		(multiple-value-bind (lat-am lon-am)
		    ;; lat-am is where radius crosses
		    ;; antimeridian. lon-am is close to -180 in W, to
		    ;; +180 in E hemisphere
		    (antimeridian-crossing g lat lon azi radius)
		  (declare (ignore lon-am))
		  (push (list -180.0d0 lat-am) amc)
		  (push (list lon2 lat2) western)
		  ;; eastern feature will be cut by antimeridian
		  (push (list +180.0d0 lat-am) eastern)
		  )
		(push (list lon2 lat2) eastern))

	    ;; do not split by antimeridian
	    (push (list lon2 lat2) all))))

    (when amc
      ;; there are points on the antimeridian, they have to be sorted
      ;; and added to the part of the feature on he western hemisphere

      ;; existing points in the western list are the outer points and
      ;; must be sorted from south to the north
      (setf western (sort western #'> :key #'second))
      (nconc western (sort amc #'< :key #'second)))

    (format out "{
  \"type\": \"Feature\",
  \"geometry\": {
    \"type\": ~s,
    \"coordinates\": [
"
            (if (or split sectors) "MultiPolygon" "Polygon"))
    ;; in GeoJSON first comes the longitude, then latitude
    (cond
      (split
       ;; eastern hemisphere part first
       (format out "[ [ ~:{ [~,5f, ~,5f],~%~}" eastern)
       (format out "~{ [~,5f, ~,5f]~%~} ] ],~%" (first eastern)) ; close the cycle
       ;; western hemisphere part second
       (format out "[ [ ~:{ [~,5f, ~,5f],~%~}" western)
       (format out "~{ [~,5f, ~,5f]~%~} ] ]~%" (first western))) ; close the cycle
      (sectors
       ;; split the circle into sectors, and output each sector as a
       ;; separate polygon inside the MultiPolygon
       (iter
	 (with sector-size = 10)
	 (with prev-point = (pop all))
	 (while all)
	 (iter
	   (for i from 0 below sector-size)
	   (for point = (pop all))
	   (initially (format out "[ [ [~,5f, ~,5f],~%~{ [~,5f, ~,5f],~}~%" lon lat prev-point))
	   (while point)
	   (format out "~{ [~,5f, ~,5f],~%~}" point)
	   (finally (format out " [~,5f, ~,5f] ] ]~[,~;~]~%" lon lat (if all 0 1))
		    (setf prev-point point)))))
      (t
       (format out "[ ~:{ [~,5f, ~,5f],~%~}" all)
       (format out "~{ [~,5f, ~,5f]~%~} ]~%" (first all))))

    (format out "] } }")))

;;---------------------------------------------------------


;; EOF
