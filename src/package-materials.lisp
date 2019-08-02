(in-package #:cl-user)

(defpackage #:virality.materials
  (:use #:cl)
  ;; helper functions
  (:export #:total-time/uniform)
  ;; profiles
  (:export
   #:u-model
   #:u-mvp
   #:u-mvpt
   #:u-proj
   #:u-time
   #:u-view
   #:u-vp
   #:u-vpt))
