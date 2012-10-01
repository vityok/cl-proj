;;; Different utilities for the Proj.4 library

(defun render-point (ox oy extent size &key
		     (src-cs "+proj=latlong +ellps=WGS84 +datum=WGS84")
		     (dst-cs "+proj=utm +zone=35 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
		     (debug t))

  "Renders the point with given coordinates using the Proj API.

EXTENT specifies the geographic extents of the original surface area
that is displayed within the screen with SIZE.

 @param oX Easting long?
 @param oY Northing lat?
  "

  (let* ((wgsProj (pj_init_plus src-cs))
	 (mapProj (pj_init_plus dst-cs))
	 (o.x (cffi:foreign-alloc :double))
	 (o.y (cffi:foreign-alloc :double))
	 (o.z (cffi:foreign-alloc :double)))

    (setf (cffi:mem-ref o.x :double) (* (float ox 0.0d0) DEG_TO_RAD))
    (setf (cffi:mem-ref o.y :double) (* (float oy 0.0d0) DEG_TO_RAD))
    (setf (cffi:mem-ref o.z :double) 0.0d0)

    (pj_transform wgsProj mapProj 1 1 o.x o.y o.z)
    (pj_free wgsProj)
    (pj_free mapProj)

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
	(format t "[render-point]: rendered location: (~2,2f, ~2,2f)-(~2,2f, ~2,2f)~%"
		ox oy pt.x pt.y))
      
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
	      (format t "[render-point]: rendered location: (~2,2f, ~2,2f, ~2,2f, ~2,2f)~%"
		      pixelX pixelY relX relY))
	    
	    (values pixelX pixelY relX relY pt.x pt.y))
	  (progn
	    (format t "FAIL: extent.maxx=~a, extent.minx=~a, extent.maxy=~a, extent.miny=~a~%"
		    extent.maxx extent.minx extent.maxy extent.miny)
	    (format t "      oMap.width=~a, oMap.height=~a~%" oMap.width oMap.height))))))