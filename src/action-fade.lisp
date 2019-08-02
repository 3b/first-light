(in-package #:virality.actions)

(defmethod v:on-action-update (action (type (eql 'fade-in)))
  (let ((material (v/comp:material (v:renderer (v:manager action)))))
    (setf (v:mat-uniform-ref material :opacity) (v:action-step action))))

(defmethod v:on-action-finish (action (type (eql 'fade-in)))
  (when (v:repeat-p action)
    (v:replace-action action 'fade-out)))

(defmethod v:on-action-update (action (type (eql 'fade-out)))
  (let ((material (v/comp:material (v:renderer (v:manager action)))))
    (setf (v:mat-uniform-ref material :opacity) (- 1 (v:action-step action)))))

(defmethod v:on-action-finish (action (type (eql 'fade-out)))
  (when (v:repeat-p action)
    (v:replace-action action 'fade-in)))
