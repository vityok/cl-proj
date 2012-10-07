CL-PROJ provides bindings for the Proj.4 library
================================================

PROJ.4 is a cartographic projections library originally written by
Gerald Evenden then of the USGS. PROJ.4 has been placed under an MIT
license.

CL-PROJ provides CFFI-based Common Lisp bindings for the PROJ.4
library. It is placed under the Berkeley Software Distribution (BSD)
license meaning that you can do almost everything you want with it.

PROJ.4 homepage is: [http://trac.osgeo.org/proj/](http://trac.osgeo.org/proj/)

CL-PROJ homepage is:
[http://cl-proj.sourceforge.net/](http://cl-proj.sourceforge.net/) and
the project page is:
[http://sourceforge.net/projects/cl-proj/](http://sourceforge.net/projects/cl-proj/)

Released files can be downloaded from [the downloads
page](http://sourceforge.net/projects/cl-proj/files). And it can also
be installed using the `asdf-install` program.



Usage
=====

Typical library loading and linking into the Lisp image would be:

    (require 'cffi)
    (cffi:define-foreign-library libproj (t (:default "libproj")))
    (cffi:load-foreign-library 'libproj)
    (require 'cl-proj)

Symbols are defined in the `cl-proj` package and are also available
using the `pj` package nickname.

By now the native libproj shared library should be loaded and linked
into the Lisp image and all the bindings should be resolved.

Constants, variables and functions are extremely close to the
PROJ.4. API is documented in the <api.html> file that is supplied in
the distribution.

Copying
=======

    Copyright (c) 2012, Victor Anyakin <anyakinvictor@yahoo.com>
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are met:
        * Redistributions of source code must retain the above copyright
          notice, this list of conditions and the following disclaimer.
        * Redistributions in binary form must reproduce the above copyright
          notice, this list of conditions and the following disclaimer in the
          documentation and/or other materials provided with the distribution.
        * Neither the name of the organization nor the
          names of its contributors may be used to endorse or promote products
          derived from this software without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
    ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
    DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
    (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
    ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

