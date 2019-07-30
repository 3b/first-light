(in-package #:%first-light)

(defclass hex-grid/columns (hex-grid) ())

(defmethod initialize-instance :after ((instance hex-grid/columns) &key)
  (with-slots (%forward %inverse %edge-directions %corner-directions) instance
    (setf %forward (v4:vec (/ 3.0 2.0) 0 (/ (sqrt 3) 2) (sqrt 3))
          %inverse (v4:vec (/ 2.0 3.0) 0 (/ -1.0 3.0) (/ (sqrt 3) 3))
          %edge-directions '(:ne :n :nw :sw :s :se)
          %corner-directions '(:e :ne :nw :w :sw :se))))

(defmethod grid-cell-neighbor-directions ((grid hex-grid/columns))
  '(:se :ne :n :nw :sw :s))

(defmethod hex-to-cell ((grid hex-grid/columns) hex)
  (v3:with-components ((h hex))
    (let ((y (+ hy (/ (+ hx (* (hex-grid-offset grid) (mod hx 2))) 2))))
      (v2:vec hx y))))

(defmethod hex-from-cell ((grid hex-grid/columns) cell)
  (v2:with-components ((c cell))
    (let ((y (- cy (/ (+ cx (* (hex-grid-offset grid) (mod cx 2))) 2))))
      (hex-cell cx y))))
