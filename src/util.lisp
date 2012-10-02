;;; Different utility functions for more convenient usage of the
;;; Proj.4 library

(defun render-point (ox oy extent size &key
		     (src-cs "+proj=latlong +ellps=WGS84 +datum=WGS84")
		     (dst-cs "+proj=utm +zone=35 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
		     (debug nil))
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
      (cffi:foreign-free o.z)

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


;; main(int argc, char **argv) {
;;      char *args[] = { "proj=merc", "ellps=clrk66", "lat_ts=33" };
;;      projUV p;
;;      projPJ pj;

;;      if (!(pj = pj_init(3, args)))
;;         exit(1);
;;      while (scanf("%lf %lf", &p.v, &p.u) == 2) {
;;         p.u *= DEG_TO_RAD;
;;         p.v *= DEG_TO_RAD;
;;         p = pj_fwd(p, pj);
;;         printf("%.2f\t%.2f\n", p.u, p.v);
;;      }
;;      exit(0);
;; }
