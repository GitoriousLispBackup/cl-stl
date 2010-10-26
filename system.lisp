(defpackage cl-stl
  (:use :common-lisp)
  (:export :load-stl :triangle :mesh :vector-3
           #:strip-redundant-vertices
           #:vertex))
