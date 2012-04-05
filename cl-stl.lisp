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

(in-package cl-stl)

(defun load-ascii-stl (stream)
  "Return a list of triangles loaded from ascii stream"
  (let ((triangles (make-sequence 'list 0)))
    (loop
       for line = (cl-utilities:split-sequence
		   #\Space
		   (read-line stream nil) :remove-empty-subseqs t)
       for first = (car line)
       with vertices = nil
       with normal = nil
       with in-solid = nil
       with in-facet = nil
       with in-loop = nil
       with solid-name = nil
       while line do
         (cond
           ((equal first "solid")
            (setf solid-name (nth 1 line))
            (setf in-solid t))
           
           ((equal first "facet")
            (setf in-facet t)
            (setf normal
                  (lm:to-vector (cddr line))))

           ((equal line '("outer" "loop"))
            (setf in-loop t))
           
           ((equal first "vertex")
            (push (lm:to-vector (cdr line))
                  vertices))

           ((equal first "endloop")
            (unless in-loop
              (error "Loop not opened")
              (setf in-loop nil)))

           ((equal first "endfacet")
            (push
             (make-instance 'cl-mesh:explicit-triangle
			    :vertices vertices
			    :normal normal)
             triangles)
            (setf vertices nil))
           
           ((equal first "endsolid")
            (unless in-solid
              (error "Solid not opened")
              (setf in-solid nil)))

           (t
            (error "Fail, I don't know this")))
       finally (unless (or in-solid in-facet in-loop)
                 (error "Some tag not closed")))
    triangles))

(defun read-int-32 (stream)
  "Read a 32bit big endian number from stream into number"
  (let ((int 0))
    (dotimes (i 4)
      (setf (ldb (byte 8 (* i 8)) int)
            (read-byte stream)))
    int))

(defun read-vector (stream)
  "Read 3 32bit ieee floats from a binary stream into a vector-3"
  (lm:vector (ieee-floats:decode-float32
	      (read-int-32 stream))
             (ieee-floats:decode-float32
	      (read-int-32 stream))
	     (ieee-floats:decode-float32
	      (read-int-32 stream))))

(defun read-triangle (stream)
  "Read a triangle from binary stream"
  (let ((triangle (make-instance 'cl-mesh:explicit-triangle
                   :normal (read-vector stream)
                   :vertices (list
                              (read-vector stream)
                              (read-vector stream)
                              (read-vector stream)))))
    (file-position stream (+ (file-position stream) 2))
    triangle))

(defun load-binary-stl (stream)
  "Return a list of triangles loaded from binary stream"
  ;; Skip header
  (file-position stream 80)
  (let ((triangle-count (read-int-32 stream)))
    (loop for i upto (1- triangle-count)
       collect (read-triangle stream))))

(defun ascii-stl-p (path)
  "Is the file in ascii format as opposed to binary stl"
  (with-open-file (f path :direction :input :if-does-not-exist :error)
    (handler-case (if (equal (subseq (read-line f) 0 5)
                             "solid")
                      t)
      (condition () nil))))

(defun load-stl (path)
  (let* ((is-ascii (ascii-stl-p path))
         (element-type (if is-ascii :default '(unsigned-byte 8))))
    (with-open-file (f path
                       :direction :input
                       :if-does-not-exist :error
                       :element-type element-type)
      (cl-mesh:strip-redundant-vertices
       (if is-ascii
           (load-ascii-stl f)
           (load-binary-stl f))))))
