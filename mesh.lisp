(in-package :mesh)

(defun vector-3-hash (v)
  (floor (* (abs (+ (cl-mesh:vector-3-x v) 2))
            (+ (abs (cl-mesh:vector-3-y v)) 3))
            (abs (+ (cl-mesh:vector-3-z v) 5))))

(sb-ext:define-hash-table-test cl-mesh:vector-3-equal-p vector-3-hash)

(defstruct explicit-triangle
  (normal)
  (vertices))

(defclass triangle ()
  ((normal
    :initarg :normal
    :initform '()
    :type cl-mesh:vector-3
    :accessor normal)
   (vertex-ids
    :initarg :vertex-ids
    :initform '()
    :type list
    :accessor vertex-ids))
  (:documentation "A triangle with a normal vector and a list of 3
  vertex ids"))

(defmethod vertex (idx (triangle triangle) mesh)
    (nth
     (nth idx (vertex-ids triangle))
     (vertices mesh)))

(defclass mesh ()
  ((vertices
    :initarg :vertices
    :accessor vertices)
   (triangles
    :initarg :triangles
    :accessor triangles)))

(defun strip-redundant-vertices (triangle-list)
  (let ((vertex-hash (make-hash-table
                      :test #'cl-mesh:vector-3-equal-p
                      :hash-function #'vector-3-hash))
        (mesh (make-instance 'mesh
                             :vertices '()
                             :triangles '()))
        (vertex-id 0))
    (loop
       for triangle in triangle-list
       for new-triangle = (make-instance
                           'triangle
                           :normal (explicit-triangle-normal triangle)) do
         (loop
            for vertex in (explicit-triangle-vertices triangle) do
              (multiple-value-bind (val present)
                  (gethash vertex vertex-hash)
                ;; Newly encountered vertex
                (unless present
                  (setf (gethash vertex vertex-hash) vertex-id)
                  (setf val vertex-id)
                  (incf vertex-id))
                ;; Push a triangle with id
                (push
                 val
                 (vertex-ids new-triangle))))
         (push
          new-triangle
          (triangles mesh)))
    (maphash (lambda (k v) (declare (ignore v)) (push k (vertices mesh))) vertex-hash)
    (setf (vertices mesh) (reverse (vertices mesh)))
    mesh))
