;;; groveler definitions from the geodesic.h file

;; Copyright (c) 2017 Victor Anyakin <anyakinvictor@yahoo.com>
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
;; DISCLAIMED. IN NO EVENT SHALL COPYRIGHT HOLDER BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :geodesic-types)

;; --------------------------------------------------------

(include "geodesic.h")

;; --------------------------------------------------------

(constant (+geodesic-version-major+ "GEODESIC_VERSION_MAJOR"))
(constant (+geodesic-version-minor+ "GEODESIC_VERSION_MINOR"))
(constant (+geodesic-version-patch+ "GEODESIC_VERSION_PATCH"))
(constant (+geodesic-version+       "GEODESIC_VERSION"))

;; --------------------------------------------------------

;; The struct containing information about the ellipsoid.  This must
;; be initialized by GEOD-INIT before use.

(cstruct geod-geodesic "struct geod_geodesic"
         (a "a" :type :double)          ; the equatorial radius
         (f "f" :type :double)          ; the flattening
         (f1 "f1" :type :double)
         (e2 "e2" :type :double)
         (ep2 "ep2" :type :double)
         (n "n" :type :double)
         (b "b" :type :double)
         (c2 "c2" :type :double)
         (etol2 "etol2" :type :double)
         (a3x "A3x" :type :double :count 6)
         (c3x "C3x" :type :double :count 15)
         (c4x "C4x"  :type :double :count 21))

;; --------------------------------------------------------

;; The struct containing information about a single geodesic.  This
;; must be initialized by GEOD-LINEINIT before use.

(cstruct geod-geodesicline "struct geod_geodesicline"
         (lat1 "lat1" :type :double)    ; the starting latitude
         (lon1 "lon1" :type :double)    ; the starting longitude
         (azi1 "azi1" :type :double)    ; the starting azimuth
         (a "a" :type :double)          ; the equatorial radius
         (f "f" :type :double)          ; the flattening

         (b "b" :type :double)
         (c2 "c2" :type :double)
         (f1 "f1" :type :double)
         (salp0 "salp0" :type :double)
         (calp0 "calp0" :type :double)
         (k2 "k2" :type :double)

         (salp1 "salp1" :type :double)
         (calp1 "calp1" :type :double)
         (ssig1 "ssig1" :type :double)
         (csig1 "csig1" :type :double)
         (dn1 "dn1" :type :double)
         (stau1 "stau1" :type :double)
         (ctau1 "ctau1" :type :double)
         (somg1 "somg1" :type :double)
         (comg1 "comg1" :type :double)
         (a1m1 "A1m1" :type :double)
         (a2m1 "A2m1" :type :double)
         (a3c "A3c" :type :double)
         (b11 "B11" :type :double)
         (b21 "B21" :type :double)
         (b31 "B31" :type :double)
         (a4 "A4" :type :double)
         (b41 "B41" :type :double)

         (c1a "C1a" :type :double :count 7)
         (c1pa "C1pa" :type :double :count 7)
         (c2a "C2a" :type :double :count 7)
         (c3a "C3a" :type :double :count 6)
         (c4a "C4a" :type :double :count 6)

         (caps "caps" :type :int )       ; the capabilities :signed nil
         )

;; --------------------------------------------------------

;; The struct for accumulating information about a geodesic polygon.
;; This is used for computing the perimeter and area of a polygon.
;; This must be initialized by GEOD-POLYGON-INIT before use.

(cstruct geod-polygon "struct geod_polygon"
         (lat "lat" :type :double)      ; the current latitude
         (lon "lon" :type :double)      ; the current longitude
         (lat0 "lat0" :type :double)
         (lon0 "lon0" :type :double)
         (A "A" :type :double :count 2)
         (P "P" :type :double :count 2)
         (polyline "polyline" :type :int)
         (crossings "crossings" :type :int)
         (num "num" :type :int ) ; the number of points so far :signed nil
         )

;; --------------------------------------------------------

(cenum (geod-mask :base-type :unsigned-int)
       ;; mask values for the \e caps argument to geod_lineinit().
       ((:none "GEOD_NONE") :documentation "Calculate nothing")
       ((:latitude "GEOD_LATITUDE") :documentation "Calculate latitude")
       ((:longitude "GEOD_LONGITUDE") :documentation "Calculate longitude")
       ((:azimuth "GEOD_AZIMUTH") :documentation "Calculate azimuth")
       ((:distance "GEOD_DISTANCE") :documentation "Calculate distance")
       ((:distance-in "GEOD_DISTANCE_IN") :documentation "Allow distance as input")
       ((:reducelength "GEOD_REDUCEDLENGTH") :documentation "Calculate reduced length")
       ((:geodesicscale "GEOD_GEODESICSCALE") :documentation "Calculate geodesic scale")
       ((:aread "GEOD_AREA") :documentation "Calculate reduced length")
       ((:all "GEOD_ALL") :documentation "Calculate everything"))

;; --------------------------------------------------------

(cenum geod-flags
       ;; flag values for the \e flags argument to geod-gendirect and
       ;; geod-genposition
       ((:noflags "GEOD_NOFLAGS") :documentation "No flags")
       ((:arcmode "GEOD_ARCMODE") :documentation "Position given in terms of arc distance")
       ((:unroll "GEOD_LONG_UNROLL") :documentation "Unroll the longitude"))

;; EOF
