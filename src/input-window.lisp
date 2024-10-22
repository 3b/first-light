(in-package #:%first-light)

(a:define-constant +window-event-names+
    #(nil :show :hide nil :move :resize nil :minimize :maximize :restore
      :mouse-focus-enter :mouse-focus-exit :keyboard-focus-enter
      :keyboard-focus-exit :close nil nil)
  :test #'equalp)

(defun get-window-title (window)
  (sdl2:get-window-title window))

(defun set-window-title (window title)
  (sdl2:set-window-title window title))

(defun get-window-size (window)
  (multiple-value-list (sdl2:get-window-size window)))

(defun set-window-size (window width height)
  (sdl2:set-window-size window width height))

(defun get-window-mode (window)
  (if (member :fullscreen-desktop (sdl2:get-window-flags window))
      :fullscreen
      :windowed))

(defun set-window-mode (window mode)
  (ecase mode
    (:fullscreen (sdl2:set-window-fullscreen window :desktop))
    (:windowed (sdl2:set-window-fullscreen window :windowed))))

(defun set-window-hidden (window)
  (sdl2:hide-window window))

(defun set-window-visible (window)
  (sdl2:show-window window))

(defun on-window-show (core)
  (declare (ignore core)))

(defun on-window-hide (core)
  (declare (ignore core)))

(defun on-window-move (core &key x y)
  (declare (ignore core x y)))

(defun on-window-resize (core &key width height)
  (declare (ignore core width height)))

(defun on-window-minimize (core)
  (declare (ignore core)))

(defun on-window-maximize (core)
  (declare (ignore core)))

(defun on-window-restore (core)
  (declare (ignore core)))

(defun on-window-mouse-focus-enter (core)
  (declare (ignore core)))

(defun on-window-mouse-focus-exit (core)
  (declare (ignore core)))

(defun on-window-keyboard-focus-enter (core)
  (declare (ignore core)))

(defun on-window-keyboard-focus-exit (core)
  (declare (ignore core)))

(defun on-window-close (core)
  (declare (ignore core)))
