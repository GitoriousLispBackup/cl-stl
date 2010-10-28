;; Copyright (c) 2010 Raffael L. Mancini <raffael.mancini@hcl-club.lu>

;; This file is part of cl-stl.

;; cl-stl is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Foobar is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Foobar.  If not, see <http://www.gnu.org/licenses/>.

(defpackage cl-stl
  (:use :common-lisp)
  (:export :load-stl :triangle :mesh :vector-3
           #:strip-redundant-vertices
           #:vertex
           #:make-vector-3))

(defpackage cl-stl-test
  (:use :common-lisp
        :cl-stl
        :lisp-unit))
