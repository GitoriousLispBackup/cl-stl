;; Copyright (c) 2010, 2011, 2012 Raffael L. Mancini
;; <raffael.mancini@hcl-club.lu>

;; This file is part of cl-stl.

;; cl-stl is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; cl-stl is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with cl-stl.  If not, see <http://www.gnu.org/licenses/>.

(defsystem :cl-stl
  :version "0.2"
  :depends-on (:parse-number
               :cl-utilities
               :ieee-floats
               :lisp-unit
               :cl-mesh
	       :l-math)
  :components
  ((:file "system")
   (:file "cl-stl" :depends-on ("system"))
   (:file "test" :depends-on ("system" "cl-stl"))))
