;;;

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


(defpackage :cl-proj
  (:use :cl)
  (:nicknames :pj)
  (:export 
   :+PJ-VERSION+
   :PJ-RELEASE
   :+RAD-TO-DEG+
   :+DEG-TO-RAD+
   :PJ-ERRNO
   :projUV
   :PJ-FWD
   :PJ-INV
   :PJ-TRANSFORM
   :PJ-DATUM-TRANSFORM
   :PJ-GEOCENTRIC-TO-GEODETIC
   :PJ-GEODETIC-TO-GEOCENTRIC
   :PJ-COMPARE-DATUMS
   :PJ-APPLY-GRIDSHIFT
   :PJ-DEALLOCATE-GRIDS
   :PJ-IS-LATLONG
   :PJ-IS-GEOCENT
   :PJ-PR-LIST
   :PJ-FREE
   :PJ-SET-FINDER
   :PJ-SET-SEARCHPATH
   :PJ-INIT
   :PJ-INIT-PLUS
   :PJ-GET-DEF
   :PJ-LATLONG-FROM-PROJ
   :PJ-MALLOC
   :PJ-DALLOC
   :PJ-STRERRNO
   :PJ-GET-ERRNO-REF
   :PJ-GET-RELEASE
   :PJ-ACQUIRE-LOCK
   :PJ-RELEASE-LOCK
   :PJ-CLEANUP-LOCK
   ;; utils
   :RENDER-POINT
   :LOAD-DEFAULTS
   :DMS-TO-DEC
   :PARSE-DEGREES
   :SIMPLIFY
   :PERPENDICULAR-DISTANCE))


