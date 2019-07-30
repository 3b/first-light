(in-package #:%first-light)

(defclass quad-grid (grid-spec) ())

(defmethod initialize-instance :after ((instance quad-grid) &key)
  (with-slots (%edge-directions %corner-directions) instance
    (setf %edge-directions '(:e :n :w :s)
          %corner-directions '(:ne :nw :sw :se))))

(defmethod grid-cell-neighbor-by-index ((grid quad-grid) cell index)
  (v2:+ cell (aref (grid-cell-neighbor-offsets grid) index)))

(defmethod grid-cell-to-point ((grid quad-grid) cell)
  (with-slots (%cell-size %cell-origin) grid
    (v2:with-components ((c cell)
                         (s %cell-size))
      (let ((px (* cx sx))
            (py (* cy sy)))
        (v2:round (v2:+ (v2:vec px py) %cell-origin))))))

(defmethod grid-cell-from-point ((grid quad-grid) point)
  (with-slots (%cell-size %cell-origin) grid
    (v2:with-components ((p (v2:- point %cell-origin))
                         (s %cell-size))
      (let ((px (/ px sx))
            (py (/ py sy)))
        (v2:round (v2:vec px py))))))

(defmethod grid-cell-select-line ((grid quad-grid) cell1 cell2)
  (loop :with distance = (grid-cell-distance grid cell1 cell2)
        :with step = (/ (max distance 1))
        :with start = (grid-cell-nudge cell1)
        :with end = (grid-cell-nudge cell2)
        :for cell :to distance
        :for selected = (v2:round (v2:lerp start end (float (* cell step) 1f0)))
        :when (grid-cell-p grid selected)
          :collect selected))

(defmethod grid-cell-select-range ((grid quad-grid) cell range)
  (loop :with cells
        :for x :from (- range) :to range
        :do (loop :for y :from (- range) :to range
                  :for selected = (v2:+ cell (v2:vec x y))
                  :when (grid-cell-p grid selected)
                    :do (push selected cells))
        :finally (return (nreverse cells))))
