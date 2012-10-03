;;; Generate API documentation using the DOCUMENTATION-TEMPLATE from
;;; Edi Weitz. Additional modifications are then required to fix
;;; references and some notes.
;;;
;;; Just run:
;;;
;;; sbcl --load mkapi.lisp
;;;

(ql:quickload "documentation-template")
(require 'cl-proj)
(documentation-template:create-template :cl-proj :target "api.html")
(quit)

;; EOF