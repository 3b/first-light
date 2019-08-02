(in-package #:cl-user)

(defpackage #:virality.prefab
  (:use #:cl)
  (:export
   #:define-prefab
   #:define-prefab-descriptor
   #:find-prefab
   #:find-prefab-descriptor
   #:make-prefab-instance
   #:ref))
