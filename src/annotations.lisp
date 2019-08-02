(in-package #:virality.annotations)

;; I designed the system such that the direction the annotation goes is the same
;; for reading and writing.
(defun %material-annotator (mat-val component)
  (with-accessors ((context v:context)) component
    (typecase mat-val
      (symbol
       (v::lookup-material mat-val context))
      (cons
       (destructuring-bind (base-mat-sym new-mat-sym
                            &key shader instances uniforms blocks)
           mat-val
         (let* ((base-mat (v::lookup-material base-mat-sym context))
                (copy-mat (v::copy-material base-mat new-mat-sym)))
           (when blocks
             (error "Material override: :blocks not implemented yet."))
           ;; First, change to the new shader
           (when shader
             (setf (v::shader copy-mat) shader))
           (when instances
             (setf (v::instances copy-mat) instances))
           ;; Then process the initargs for the new shader.
           (when uniforms
             (unless (every (lambda (x) (= (length x) 2)) uniforms)
               (error "Material override: :uniforms entries must have a length ~
                       of 2 ~A~%"
                      uniforms))
             (loop :for (uniform-name value) :in uniforms
                   :do (setf (v:mat-uniform-ref copy-mat uniform-name) value)))
           ;; and return the newly minted material with all the overrides.
           copy-mat)))
      (t
       mat-val))))

(v:define-annotation material
  ;; Often these are the same, but there are common cases where they won't be.
  :getter %material-annotator
  :setter %material-annotator)
