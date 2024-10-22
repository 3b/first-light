(in-package #:cl-user)

(defpackage #:%first-light
  (:nicknames #:%fl)
  (:local-nicknames (#:a #:alexandria)
                    (#:u #:golden-utils)
                    (#:v2 #:origin.vec2)
                    (#:v3 #:origin.vec3))
  (:use #:cl)
  (:export
   #:*core-debug*
   #:active-camera
   #:actor
   #:actor-component-by-type
   #:actor-components-by-type
   #:alpha
   #:attach-component
   #:attach-multiple-components
   #:cameras
   #:component
   #:compute-component-initargs
   #:context
   #:copy-material
   #:core
   #:define-annotation
   #:define-component
   #:define-graph
   #:define-material
   #:define-material-profile
   #:define-options
   #:define-resources
   #:define-texture
   #:define-texture-profile
   #:delta
   #:deploy-binary
   #:deregister-collider
   #:destroy
   #:destroy-after-time
   #:detach-component
   #:display-id
   #:find-actors-by-id
   #:find-by-uuid
   #:find-components-by-id
   #:find-resource
   #:frame-count
   #:frame-manager
   #:frame-time
   #:general-data-format-descriptor
   #:get-computed-component-precedence-list
   #:id
   #:input-data
   #:instances
   #:lookup-material
   #:make-actor
   #:make-component
   #:make-scene-tree
   #:mat-uniform-ref
   #:on-component-attach
   #:on-component-destroy
   #:on-component-detach
   #:on-component-initialize
   #:on-component-physics-update
   #:on-component-render
   #:on-component-update
   #:on-collision-enter
   #:on-collision-continue
   #:on-collision-exit
   #:option
   #:prefab-node
   #:print-all-resources
   #:project-data
   #:register-collider
   #:scene-tree
   #:shader
   #:shared-storage
   #:spawn-actor
   #:ss-href
   #:start-engine
   #:state
   #:stop-engine
   #:total-time
   #:ttl
   #:using-material
   #:with-shared-storage)

  ;; metadata
  (:export
   #:meta)

  ;; geometry
  (:export
   #:define-geometry-layout
   #:define-geometry
   #:draw-dynamic-geometry
   #:draw-static-geometry
   #:load-static-geometry
   #:make-dynamic-geometry
   #:update-dynamic-geometry)

  ;; image
  (:export
   #:channels
   #:data
   #:free-storage
   #:get-pixel-size
   #:height
   #:internal-format
   #:origin
   #:pixel-format
   #:pixel-type
   #:read-image
   #:width)

  ;; input
  (:export
   #:get-gamepad-analog
   #:get-gamepad-name
   #:get-mouse-position
   #:get-mouse-scroll
   #:handle-events
   #:input-enabled-p
   #:input-enter-p
   #:input-exit-p
   #:make-input-data
   #:prepare-gamepads
   #:shutdown-gamepads)
  )
