;;; -*- mode: lisp; -*-
     
(asdf:defsystem :cl-proj
    :description "CL-PROJ provides Proj.4 library bindings"
    :version "4.7-a"
    :author "Victor Anyakin <anyakinvictor@yahoo.com>"
    :licence "BSD"
    :components ((:module "src"
			  :components ((:file "package")
				       (:file "cl-proj" :depends-on ("package")))))
    :depends-on (:cffi))