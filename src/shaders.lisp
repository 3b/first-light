(in-package #:virality.engine)

(defun initialize-shaders (core)
  (let ((modify-hook (generate-shader-modify-hook core)))
    (setf (shaders core) (virality.gpu:load-shaders modify-hook))))

(defun generate-shader-modify-hook (core)
  (lambda (programs)
    (queues:qpush (recompilation-queue core)
                  (list :shader-recompilation programs))))
