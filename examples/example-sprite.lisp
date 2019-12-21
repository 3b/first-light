(in-package #:virality.examples)

;;; Textures

(v:define-texture sprites (:texture-2d)
  (:data #(:spritesheet)))

;;; Components

(v:define-component simple-movement ()
  ((%transform :reader transform)))

(defmethod v:on-component-initialize ((self simple-movement))
  (with-slots (%transform) self
    (setf %transform (v:component-by-type (v:actor self) 'c/xform:transform))
    (c/xform:translate %transform (v3:vec -400f0 0f0 0f0)
                       :replace-p t :instant-p t)))

(defmethod v:on-component-update ((self simple-movement))
  (u:mvlet* ((context (v:context self))
             (transform (transform self))
             (lx ly (v:get-gamepad-analog context '(:gamepad1 :left-stick)))
             (rx ry (v:get-gamepad-analog context '(:gamepad1 :right-stick)))
             (instant-p (zerop (v:frame-count context))))
    (let ((vec (v3:vec lx ly 0f0)))
      (v3:scale! vec (if (> (v3:length vec) 1) (v3:normalize vec) vec) 150f0)
      (c/xform:translate transform
                         (v3:+ (v3:vec -400f0 0f0 0f0) vec)
                         :replace-p t
                         :instant-p instant-p)
      (unless (= rx ry 0f0)
        (let* ((angle (atan (- rx) ry))
               (angle (if (minusp angle)
                          (+ pi (- pi (abs angle)))
                          angle))
               (angle (float angle 1f0)))
          (c/xform:rotate transform
                          (q:orient :local :z angle)
                          :replace-p t
                          :instant-p instant-p))))))

(v:define-component shot-mover ()
  ((%transform :reader transform)
   (%velocity :reader velocity
              :initarg :velocity
              :initform 0f0)))

(defmethod v:on-component-initialize ((self shot-mover))
  (with-slots (%transform) self
    (setf %transform (v:component-by-type (v:actor self) 'c/xform:transform))))

(defmethod v:on-component-update ((self shot-mover))
  (c/xform:translate
   (transform self)
   (let ((a (v3:normalize (m4:rotation-axis-to-vec3
                           (c/xform:local (transform self)) :y)))
         (move-delta (float (* (velocity self)
                               (v:frame-time (v:context self)))
                            1f0)))
     (v3:scale a move-delta))))

(v:define-component shot-emitter ()
  ((%transform :reader transform)))

(defmethod v:on-component-initialize ((self shot-emitter))
  (with-slots (%transform) self
    (setf %transform (v:component-by-type (v:actor self) 'c/xform:transform))))

(defmethod v:on-component-update ((self shot-emitter))
  (let ((context (v:context self)))
    (when (or (v:input-enter-p context '(:gamepad1 :a))
              (v:input-enter-p context '(:mouse :left)))
      (let* ((parent-model (c/xform:model (transform self)))
             (parent-translation (m4:get-translation parent-model))
             (parent-rotation (q:from-mat4 parent-model))
             (new-actor (v:make-actor context :display-id "Ship bullet"))
             (transform (v:make-component context
                                          'c/xform:transform
                                          :translate parent-translation
                                          :rotate parent-rotation))
             (shot-mover (v:make-component context 'shot-mover
					   :velocity 1000f0))
             (sprite (v:make-component context
                                       'c/sprite:sprite
                                       :spec :spritesheet-data
                                       :name "bullet01"
                                       :frames 2))
             (render (v:make-component context
                                       'c/render:render
                                       :material `(x/mat:sprite
                                                   ,(a:make-gensym '#:sprite)
                                                   :uniforms
                                                   ((:sprite.sampler sprites)))
                                       :mode :sprite)))
        (v:attach-components
         new-actor transform shot-mover sprite render)
        (v:spawn-actor new-actor)
        (v:destroy new-actor :ttl 2f0)))))

;;; Prefabs

(v:define-prefab "sprite-1" (:library examples)
  (("camera" :copy "/cameras/ortho"))
  ("ship"
   (c/xform:transform :rotate (q:orient :local :z (float (/ pi -2) 1f0)))
   (simple-movement)
   (shot-emitter)
   ("ship-body"
    (c/sprite:sprite :spec :spritesheet-data
                     :name "ship29")
    (c/render:render :material `(x/mat:sprite
                                 ,(a:make-gensym '#:sprite)
                                 :uniforms ((:sprite.sampler sprites)))
                     :mode :sprite)
    ("exhaust"
     (c/xform:transform :translate (v3:vec 0f0 -140f0 0f0))
     (c/sprite:sprite :spec :spritesheet-data
                      :name "exhaust03-01"
                      :frames 8)
     (c/render:render :material `(x/mat:sprite
                                  ,(a:make-gensym '#:sprite)
                                  :uniforms ((:sprite.sampler sprites)))
                      :mode :sprite)
     (c/action:actions :default '((:type x/action:sprite-animate
                                   :duration 0.5f0
                                   :repeat-p t)))))))

(v:define-prefab "sprite-2" (:library examples)
  (("camera" :copy "/cameras/ortho"))
  ("plane"
   (c/xform:transform :scale 2f0)
   (c/sprite:sprite :spec :spritesheet-data
                    :name "planet04")
   (c/render:render :material `(x/mat:sprite
                                ,(a:make-gensym '#:sprite)
                                :uniforms ((:sprite.sampler sprites)))
                    :mode :sprite)
   (c/action:actions :default '((:type x/action:rotate
                                 :duration 4f0
                                 :shape o:bounce-in
                                 :repeat-p t)))))

(v:define-prefab "sprite-3" (:library examples)
  (("camera" :copy "/cameras/ortho"))
  ("plane"
   (c/xform:transform :scale 2f0
                      :rotate/inc (o:make-velocity v3:+forward+
						   (float pi 1f0)))
   (c/sprite:sprite :spec :spritesheet-data
                    :name "planet04")
   (c/render:render :material `(x/mat:sprite
                                ,(a:make-gensym '#:sprite)
                                :uniforms ((:sprite.sampler sprites)))
                    :mode :sprite)))

;;; Prefab descriptors

(v:define-prefab-descriptor sprite-1 ()
  ("sprite-1" examples))

(v:define-prefab-descriptor sprite-2 ()
  ("sprite-2" examples))

(v:define-prefab-descriptor sprite-3 ()
  ("sprite-3" examples))
