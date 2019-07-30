(in-package #:%first-light)

(defclass grid-spec ()
  ((%size :reader size
          :initarg :size)
   (%cell-size :reader cell-size
               :initarg :cell-size
               :initform (v2:one))
   (%cell-origin :reader cell-origin
                 :initarg :cell-origin
                 :initform (v2:zero))
   (%start-angle :reader start-angle
                 :initarg :start-angle)
   (%edge-directions :reader edge-directions
                     :initarg :edge-directions)
   (%corner-directions :reader corner-directions
                       :initarg :corner-directions)))

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

(defun grid-cell-directions (grid)
  (flet ((get-offset (count direction)
           (with-slots (%start-angle %cell-size) grid
             (let* ((factor (/ (+ %start-angle direction) count))
                    (angle (a:lerp factor 0 (* pi 2))))
               (v2:* %cell-size
                     (v2:round (v2:vec (cos angle) (sin angle))))))))
    (loop :with count = (length (grid-cell-neighbor-directions grid))
          :for direction :below count
          :for offset = (get-offset count direction)
          :collect (v2:normalize offset))))

(defun grid-cell-edge-directions (grid)
  (u:interleave
   (edge-directions grid)
   (loop :for (k v) :on (grid-cell-directions grid) :by #'cddr
         :collect v)))

(defun grid-cell-corner-directions (grid)
  (u:interleave
   (corner-directions grid)
   (loop :for (k v) :on (grid-cell-directions grid) :by #'cddr
         :collect k)))

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
