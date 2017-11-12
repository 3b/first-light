(in-package :first-light)

(defclass display (kit.sdl2:gl-window box.fm:frame-manager)
  ((core-state :reader core-state
               :initarg :core-state)
   (hz :reader hz
       :initarg :hz)))

(defun calculate-refresh-rate ()
  (let ((hz (nth-value 3 (sdl2:get-current-display-mode 0))))
    (if (zerop hz) 60 hz)))

(defgeneric make-display (core-state)
  (:method ((core-state core-state))
    (let ((hz (calculate-refresh-rate)))
      (with-cfg (title width height delta periodic-interval
                       debug-frames-interval)
          (context core-state)
        (setf (slot-value core-state '%display)
              (make-instance 'display
                             :core-state core-state
                             :title title
                             :w width
                             :h height
                             :hz hz
                             :delta delta
                             :period periodic-interval
                             :debug-interval debug-frames-interval))
        (slog:emit :display.init width height hz)))))

(defmethod make-display :before ((core-state core-state))
  (with-cfg (log-level gl-version-major gl-version-minor anti-alias-level)
      (context core-state)
    (setf slog:*current-level* log-level)
    (dolist (attr `((:context-major-version ,gl-version-major)
                    (:context-minor-version ,gl-version-minor)
                    (:multisamplebuffers ,(if (zerop anti-alias-level) 0 1))
                    (:multisamplesamples ,anti-alias-level)))
      (apply #'sdl2:gl-set-attr attr))))

(defmethod make-display :after ((core-state core-state))
  (with-cfg (gl-capabilities gl-blend-mode gl-depth-mode vsync)
      (context core-state)
    (setf (kit.sdl2:idle-render (display core-state)) t)
    (apply #'gl:enable gl-capabilities)
    (apply #'gl:blend-func gl-blend-mode)
    (gl:depth-func gl-depth-mode)
    (sdl2:gl-set-swap-interval (if vsync 1 0))))

(defmethod kit.sdl2:render ((display display))
  (gl:clear-color (* 0.2 (abs (sin (* 0.001 (get-internal-real-time))))) 0 0 1)
  (gl:clear :color-buffer :depth-buffer)
  (execute-flow (core-state display)
                :default
                'perform-one-frame
                'entry/perform-one-frame
                :come-from-state-name :ef))

(defmethod kit.sdl2:close-window :around ((display display))
  (with-cfg (width height) (context (core-state display))
    (call-next-method)
    (slog:emit :display.stop width height (hz display))))

(defmethod quit-engine ((display display))
  (with-cfg (title) (context (core-state display))
    (kit.sdl2:close-window display)
    (kit.sdl2:quit)
    (slog:emit :engine.quit title)))
