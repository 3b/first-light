(in-package #:%first-light)

(defclass hex-grid (grid-spec)
  ((%forward :reader forward)
   (%inverse :reader inverse)
   (%offset :reader offset
            :initarg :offset
            :initform :even)))

(defgeneric hex-to-cell (grid hex))

(defgeneric hex-from-cell (grid cell))

(defun hex-grid-offset (grid)
  (ecase (offset grid)
    (:even 1)
    (:odd -1)))

(defun hex-cell (x y)
  (v3:vec x y (- (- x) y)))

(defun hex-cell-nudge (hex)
  (v3:+ hex (hex-cell 1e-7 1e-7)))

(defun hex-cell-round (hex)
  (v3:with-components ((r (v3:round hex))
                       (d (v3:abs (v3:- r hex))))
    (cond ((and (> dx dy) (> dx dz))
           (setf rx (- (- ry) rz)))
          ((> dy dz)
           (setf ry (- (- rx) rz))))
    r))

(defmethod grid-cell-neighbor-offsets ((grid hex-grid))
  (vector (hex-cell 1 0)
          (hex-cell 1 -1)
          (hex-cell 0 -1)
          (hex-cell -1 0)
          (hex-cell -1 1)
          (hex-cell 0 1)))

(defmethod grid-cell-neighbor-by-index ((grid hex-grid) cell index)
  (let ((hex (v3:+ (hex-from-cell grid cell)
                   (aref (grid-cell-neighbor-offsets grid) index))))
    (hex-to-cell grid hex)))

(defmethod grid-cell-distance ((grid hex-grid) cell1 cell2)
  (v3:with-components ((c (v3:abs (hex-from-cell grid (v2:- cell1 cell2)))))
    (floor (max cx cy cz))))

(defmethod grid-cell-to-point ((grid hex-grid) cell)
  (with-slots (%cell-size %cell-origin %forward) grid
    (v3:with-components ((c (hex-from-cell grid cell))
                         (s %cell-size))
      (v4:with-components ((f %forward))
        (let* ((x (+ (* fw cx) (* fx cy)))
               (y (+ (* fy cx) (* fz cy)))
               (px (* x sx))
               (py (* y sy)))
          (v2:round (v2:+ (v2:vec px py) %cell-origin)))))))

(defmethod grid-cell-from-point ((grid hex-grid) point)
  (with-slots (%cell-size %cell-origin %inverse) grid
    (v2:with-components ((p (v2:/ (v2:- point %cell-origin) %cell-size)))
      (v4:with-components ((i %inverse))
        (let* ((x (+ (* iw px) (* ix py)))
               (y (+ (* iy px) (* iz py))))
          (hex-to-cell grid (hex-cell-round (hex-cell x y))))))))

(defmethod grid-cell-select-line ((grid hex-grid) cell1 cell2)
  (loop :with distance = (grid-cell-distance grid cell1 cell2)
        :with step = (/ (max distance 1))
        :with start = (hex-cell-nudge (hex-from-cell grid cell1))
        :with end = (hex-cell-nudge (hex-from-cell grid cell2))
        :for hex :to distance
        :for selected = (v3:lerp start end (float (* hex step) 1f0))
        :for cell = (hex-to-cell grid (hex-cell-round selected))
        :when (grid-cell-p grid cell)
          :collect cell))

(defmethod grid-cell-select-range ((grid hex-grid) cell range)
  (loop :with cells
        :for x :from (- range) :to range
        :for min = (max (- range) (- (- x) range))
        :for max = (min range (+ (- x) range))
        :do (loop :for y :from min :to max
                  :for hex = (v3:+ (hex-from-cell grid cell) (v3:vec x y 0))
                  :for selected = (hex-to-cell grid hex)
                  :when (grid-cell-p grid selected)
                    :do (push selected cells))
        :finally (return (nreverse cells))))
