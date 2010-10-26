(in-package cl-stl)

(defstruct vector-3
  (x 0.0 :type float)
  (y 0.0 :type float)
  (z 0.0 :type float))

(defun make-vector-3-from-list (l)
  (make-vector-3 :x (float
                     (parse-number:parse-number (nth 0 l)))
                 :y (float
                     (parse-number:parse-number (nth 1 l)))
                 :z (float
                     (parse-number:parse-number (nth 2 l)))))

(defun vector-3-equal-p (v1 v2)
  (if (and (eql (vector-3-x v1)
                (vector-3-x v2))
           (eql (vector-3-y v1)
                (vector-3-y v2))
           (eql (vector-3-z v1)
                (vector-3-z v2)))
      t
      nil))

(defun vector-3-hash (v)
  (floor (* (abs (+ (vector-3-x v) 2))
            (+ (abs (vector-3-y v)) 3))
            (abs (+ (vector-3-z v) 5))))

(sb-ext:define-hash-table-test vector-3-equal-p vector-3-hash)

(defstruct direct-triangle
  (normal)
  (vertices))

(defclass triangle ()
  ((normal
    :initarg :normal
    :initform '()
    :type vector-3
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

(defun load-ascii-stl (stream)
  "Return a list of triangles loaded from ascii stream"
  (let ((triangles (make-sequence 'list 0)))
    (loop
       for line = (cl-utilities:split-sequence #\Space
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
                  (make-vector-3-from-list (cddr line))))

           ((equal line '("outer" "loop"))
            (setf in-loop t))
           
           ((equal first "vertex")
            (push (make-vector-3-from-list (cdr line))
                  vertices))

           ((equal first "endloop")
            (unless in-loop
              (error "Loop not opened")
              (setf in-loop nil)))

           ((equal first "endfacet")
            (push
             (make-direct-triangle :vertices vertices :normal normal)
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

(defun read-vector-3 (stream)
  "Read 3 32bit ieee floats from a binary stream into a vector-3"
  (make-vector-3 :x (ieee-floats:decode-float32
                     (read-int-32 stream))
                 :y (ieee-floats:decode-float32
                     (read-int-32 stream))
                 :z (ieee-floats:decode-float32
                     (read-int-32 stream))))

(defun read-triangle (stream)
  "Read a triangle from binary stream"
  (let ((triangle (make-direct-triangle :normal (read-vector-3 stream)
                                 :vertices (list
                                            (read-vector-3 stream)
                                            (read-vector-3 stream)
                                            (read-vector-3 stream)))))
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

(defun strip-redundant-vertices (triangle-list)
  (let ((vertex-hash (make-hash-table
                      :test #'vector-3-equal-p
                      :hash-function #'vector-3-hash))
        (mesh (make-instance 'mesh
                             :vertices '()
                             :triangles '()))
        (vertex-id 0))
    (loop
       for triangle in triangle-list
       for new-triangle = (make-instance
                           'triangle
                           :normal (direct-triangle-normal triangle)) do
         (loop
            for vertex in (direct-triangle-vertices triangle) do
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

(defun load-stl (path)
  (let* ((is-ascii (ascii-stl-p path))
         (element-type (if is-ascii :default '(unsigned-byte 8))))
    (with-open-file (f path
                       :direction :input
                       :if-does-not-exist :error
                       :element-type element-type)
      (strip-redundant-vertices
       (if is-ascii
           (load-ascii-stl f)
           (load-binary-stl f))))))
