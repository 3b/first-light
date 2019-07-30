(in-package #:%first-light)

(defclass hex-grid/rows (hex-grid) ())

(defmethod initialize-instance :after ((instance hex-grid/rows) &key)
  (with-slots (%forward %inverse %edge-directions %corner-directions) instance
    (setf %forward (v4:vec (sqrt 3) (/ (sqrt 3) 2) 0 (/ 3.0 2.0))
          %inverse (v4:vec (/ (sqrt 3) 3) (/ -1.0 3.0) 0 (/ 2.0 3.0))
          %edge-directions '(:ne :nw :w :sw :se :e)
          %corner-directions '(:ne :n :nw :sw :s :se))))

(defmethod grid-cell-neighbor-directions ((grid hex-grid/rows))
  '(:e :ne :nw :w :sw :se))

(defmethod hex-to-cell ((grid hex-grid/rows) hex)
  (v3:with-components ((h hex))
    (let ((x (+ hx (/ (+ hy (* (hex-grid-offset grid) (mod hy 2))) 2))))
      (v2:vec x hy))))

(defmethod hex-from-cell ((grid hex-grid/rows) cell)
  (v2:with-components ((c cell))
    (let ((x (- cx (/ (+ cy (* (hex-grid-offset grid) (mod cy 2))) 2))))
      (hex-cell x cy))))
