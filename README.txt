CL-PROJ provides bindings for the Proj.4 library
================================================


Usage
=====

Typical library loading and linking into the Lisp image would be:

(cffi:define-foreign-library libproj (t (:default "libproj")))
(cffi:load-foreign-library 'libproj)
(require 'cl-proj)

By now the native libproj shared library should be loaded and linked
into the Lisp image and all the bindings should be resolved.
