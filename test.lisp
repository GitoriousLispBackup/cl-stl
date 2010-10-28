(in-package :cl-stl-test)

(define-test read-ascii
  (let* ((stl (cl-stl:load-stl #p"test-data/ascii.stl"))
         (first-triangle (first (cl-stl::triangles stl))))
    (assert-equal (length (cl-stl::triangles stl)) 4)
    (assert-true (cl-stl::vector-3-equal-p
                  (cl-stl::normal first-triangle)
                  (cl-stl::make-vector-3 :x 0.0 :y 0.0 :z -1.0)))))