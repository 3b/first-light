(in-package #:cl-user)

(defpackage #:virality.actions
  (:use #:cl)
  ;; protocol
  (:export
   #:insert-action
   #:make-action-manager
   #:process-actions
   #:remove-action)
  ;; built-in actions
  (:export
   #:fade-in
   #:fade-out
   #:rotate
   #:rotate/reverse
   #:sprite-animate))
