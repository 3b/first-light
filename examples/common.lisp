(in-package #:virality.examples)

(v:define-options ()
  :title "Virality Engine Examples"
  :window-width 1920
  :window-height 1080
  :vsync :off
  :log-level :debug
  :log-repl-categories '(:fl)
  ;; NOTE: Make physics compute faster as fast for these examples.
  ;; This is really here because of the lisp game jam april 2019 codes.
  :delta 1/120
  :initial-scene 'geometric-volumes)

(v:define-resources (:project :virality-examples)
  (:project "data/project")
  (:ext (:project "ext"))
  (:mesh (:project "mesh"))
  (:texture (:project "texture"))
  (:lgj-04/2019 (:project :texture "lisp-game-jam-04-2019"))
  (:log (:project "log"))
  (:log-debug (:project :log "debug.log"))
  (:log-error (:project :log "error.log"))
  (:example-texture (:project :texture "example-texture"))
  (:1da (:project :example-texture "1d-array"))
  (:2da (:project :example-texture "2d-array"))
  (:3d (:project :example-texture "3d"))
  (:cubemap (:project :example-texture "cube-map"))
  (:cubemaparray (:project :example-texture "cube-map-array"))
  (:spritesheet (:project :texture "example-sprite/sprites.tiff"))
  (:spritesheet-data (:project "sprites.sexp"))
  (:damaged-helmet-textures (:project :texture "example-damaged-helmet")))

(defun prologue (context)
  (declare (ignore context))
  (log:trace :virality "Running prologue method."))

(defun epilogue (context)
  (declare (ignore context))
  (log:trace :virality "Running epilogue method."))

;;; Prefabs

(virality.prefab:define-prefab "cameras" (:library examples)
  ("ortho"
   (v/comp:camera :active-p t
                  :mode :orthographic))
  ("perspective"
   (v/comp:camera :active-p t
                  :mode :perspective))
  ("iso"
   (v/comp:transform :rotate (q:orient :local
                                       :x (- (atan (/ (sqrt 2))))
                                       :y (- (/ pi 4))))
   ("camera"
    (v/comp:transform :translate (v3:vec 0 0 10))
    (v/comp:camera :active-p t
                   :mode :orthographic))))

(virality.prefab:define-prefab "mesh" (:library examples)
  (v/comp:static-mesh :location '((:core :mesh) "plane.glb"))
  (v/comp:render :material 'virality.materials:unlit-texture))

;;; Graphs

;;; TODO: Fix graphs to work in user package
(in-package #:virality.engine)

(define-graph :virality.examples
    (:category component-dependency
     :depends-on ((:core (all-unknown-types core-types)))
     :roots (all-ordered-types))
  (subdag all-ordered-types
          ((splice core-types)
           -> (splice all-unknown-types))))

(define-graph :virality
    (:category component-package-order
     :depends-on ((:core-component-order (core-packages)))
     :roots (start-search))
  (subdag current-project (:virality.components -> :virality.examples))
  (subdag start-search
          ((splice current-project)
           -> (splice core-packages))))
