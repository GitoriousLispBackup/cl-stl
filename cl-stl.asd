(defsystem :cl-stl
  :version "0.1"
  :depends-on (:parse-number :cl-utilities :ieee-floats)
  :components
  ((:file "system")
   (:file "cl-stl" :depends-on ("system"))))
