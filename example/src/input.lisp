(in-package :first-light-example)

(defmethod key-down ((display display) (key (eql :scancode-escape)))
  (quit-engine display))

(defmethod mouse-scroll-up ((display display))
  (fl.comp.camera:zoom-camera display 1))

(defmethod mouse-scroll-down ((display display))
  (fl.comp.camera:zoom-camera display -1))
