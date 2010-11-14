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
;; along with cl-stl.  If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-stl-test)

(define-test read-ascii
  (let* ((stl (cl-stl:load-stl #p"test-data/ascii.stl"))
         (first-triangle (first (cl-stl::triangles stl))))
    (assert-equal (length (cl-stl::triangles stl)) 4)
    (assert-true (cl-stl:vector-3-equal-p
                  (cl-stl:normal first-triangle)
                  (cl-stl:make-vector-3 :x 0.0 :y 0.0 :z -1.0)))))

