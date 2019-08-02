(in-package #:virality.examples)

;;; Prefabs

(virality.prefab:define-prefab "isometric-view" (:library examples)
  ("camera-handle"
   (v/comp:transform :rotate/inc (q:orient :local :y (/ pi 4)))
   ("iso"
    (v/comp:transform :rotate (q:orient :local
                                        :x (- (atan (/ (sqrt 2))))
                                        :y (- (/ pi 4))))
    ("camera"
     (v/comp:transform :translate (v3:vec 0 0 10))
     (v/comp:camera :active-p t
                    :clip-near .1
                    :clip-far 1024
                    :zoom 100
                    :mode :orthographic))))

  ;; TODO: Can't use this, because of transform propogation bug.
  ;; Once that is fixed I can bring this back in.
  #++(("camera" :copy "/cameras/iso")
      ("camera"
       (v/comp:camera (:policy :new-args) :clip-near .1
                                          :clip-far 1024
                                          :zoom 100)))

  ;; NOTE: cubes are on xz plane.

  (("cube-z-1" :copy "/mesh")
   (v/comp:transform :translate (v3:vec 0 0 -4))
   (v/comp:static-mesh :location '((:core :mesh) "cube.glb")))

  (("cube-z-0" :copy "/mesh")
   (v/comp:transform :translate (v3:vec 0 0 -2))
   (v/comp:static-mesh :location '((:core :mesh) "cube.glb")))

  (("cube-origin" :copy "/mesh")
   (v/comp:transform #++ :rotate/inc #++ (q:orient :local
                                                   (v3:vec -1f0 1f0 1f0) (/ pi 2)))
   (v/comp:static-mesh :location '((:core :mesh) "cube.glb")))

  (("cube-x-0" :copy "/mesh")
   (v/comp:transform :translate (v3:vec 2 0 0))
   (v/comp:static-mesh :location '((:core :mesh) "cube.glb")))

  (("cube-x-1" :copy "/mesh")
   (v/comp:transform :translate (v3:vec 4 0 0))
   (v/comp:static-mesh :location '((:core :mesh) "cube.glb"))))

;;; Prefab descriptors

(virality.prefab:define-prefab-descriptor isometric-view ()
  ("isometric-view" virality.examples:examples))
