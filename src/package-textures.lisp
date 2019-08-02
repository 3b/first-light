(in-package #:cl-user)

(defpackage #:virality.textures
  (:use #:cl)
  ;; profiles
  (:export
   #:clamp-all-edges
   #:default-profile
   ;; TODO: exploratory framebuffer support
   #:framebuffer)
  ;; textures
  (:export
   #:debug-texture
   ;; TODO: Exploratory framebuffer support
   #:framebuffer-color
   #:framebuffer-depth
   #:framebuffer-depth/stencil
   #:framebuffer-stencil))
