;;;

;; Copyright (c) 2015, Victor Anyakin <anyakinvictor@yahoo.com>
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

(in-package :cl-user)

(cffi:define-foreign-library libproj
    (:unix (:or "libproj.so" "libproj.so.0"))
  (t (:default "libproj")))
(cffi:use-foreign-library libproj)

;; --------------------------------------------------------

(defpackage :cl-proj
  (:use :cl)
  (:nicknames :pj)

  (:documentation "CL-PROJ provides bindings for the Proj.4 library.

Constants, variables and function names are extremely close to the
native PROJ.4 library.

A number of utility functions are provided along with bare bindings.")

  (:export
   :+pj-version+
   :pj-release
   :+rad-to-deg+
   :+deg-to-rad+
   :pj-errno
   :proj-uv
   :proj-pj
   :pj-fwd
   :pj-inv
   :pj-transform
   :pj-datum-transform
   :pj-geocentric-to-geodetic
   :pj-geodetic-to-geocentric
   :pj-compare-datums
   :pj-apply-gridshift
   :pj-deallocate-grids
   :pj-is-latlong
   :pj-is-geocent
   :pj-pr-list
   :pj-free
   :pj-set-finder
   :pj-set-searchpath
   :pj-init
   :pj-init-plus
   :pj-get-def
   :pj-latlong-from-proj
   :pj-malloc
   :pj-dalloc
   :pj-strerrno
   :pj-get-errno-ref
   :pj-get-release
   :pj-acquire-lock
   :pj-release-lock
   :pj-cleanup-lock
   ;; utils
   :deg-to-rad
   :deg-to-rad-array
   :geo-transform
   :render-point
   :load-defaults
   :dms-to-dec
   :parse-degrees
   :simplify
   :perpendicular-distance))

;; EOF
