(in-package #:%first-light)

(defclass grid-spec ()
  ((%size :reader size
          :initarg :size)
   (%cell-size :reader cell-size
               :initarg :cell-size
               :initform nil)
   (%cell-origin :reader cell-origin
                 :initarg :cell-origin
                 :initform nil)
   (%edge-directions :reader edge-directions)
   (%corner-directions :reader corner-directions)))

(defmethod initialize-instance :after ((instance grid-spec) &key size)
  (unless size
    (error "Grid must have a size."))
  (with-slots (%cell-size %cell-origin) instance
    (setf %cell-size (or %cell-size (v2:one))
          %cell-origin (or %cell-origin (v2:zero)))))

(defun make-grid (type &rest args)
  (apply #'make-instance type args))

(defun ensure-grid-cell (grid cell)
  (unless (grid-cell-p grid cell)
    (error "Cell ~s is not a member of the grid." cell)))

(defgeneric grid-cell-neighbor-directions (grid))

(defgeneric grid-cell-neighbor-offsets (grid))

(defgeneric grid-cell-neighbor-by-index (grid cell index)
  (:method :before (grid cell index)
    (ensure-grid-cell grid cell)))

(defgeneric grid-cell-distance (grid cell1 cell2)
  (:method :before (grid cell1 cell2)
    (ensure-grid-cell grid cell1)
    (ensure-grid-cell grid cell2)))

(defgeneric grid-cell-to-point (grid cell)
  (:method :before (grid cell)
    (ensure-grid-cell grid cell)))

(defgeneric grid-cell-from-point (grid point))

(defgeneric grid-cell-select-line (grid cell1 cell2)
  (:method :before (grid cell1 cell2)
    (ensure-grid-cell grid cell1)
    (ensure-grid-cell grid cell2)))

(defgeneric grid-cell-select-range (grid cell range)
  (:method :before (grid cell range)
    (ensure-grid-cell grid cell)))

(defun grid-cell-nudge (cell)
  (v2:+ cell (v2:vec 1e-7 1e-7)))

(defun grid-cell-p (grid cell)
  (v2:with-components ((g (size grid))
                       (c cell))
    (and (>= cx 0)
         (< cx gx)
         (>= cy 0)
         (< cy gy))))

(defun grid-cell-neighbors (grid cell)
  (loop :for direction :in (grid-cell-neighbor-directions grid)
        :for i :from 0
        :for neighbor = (grid-cell-neighbor-by-index grid cell i)
        :when (grid-cell-p grid neighbor)
          :collect direction
          :and
            :collect neighbor))

(defun grid-cell-neighbors-p (grid cell1 cell2)
  (let ((neighbors (u:plist-values (grid-cell-neighbors grid cell1))))
    (when (find cell2 neighbors :test #'equalp)
      t)))

(defun grid-cell-neighbor (grid cell direction)
  (getf (grid-cell-neighbors grid cell) direction))
