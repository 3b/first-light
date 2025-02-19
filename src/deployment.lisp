(in-package #:%first-light)

#+sbcl
(defun deploy-binary (file-name scene &key compress-p)
  (v:stop v:*global-controller*)
  (setf uiop/image:*image-dumped-p* t)
  (sb-ext:save-lisp-and-die
   file-name
   :toplevel (lambda () (fl:start-engine :scene scene))
   :executable t
   :compression (when compress-p 9)))

#-sbcl
(defun deploy-binary (file-name scene-name)
  (declare (ignore file-name scene-name))
  (v:warn :fl.core.deploy "Deployment is not supported on this platform."))
