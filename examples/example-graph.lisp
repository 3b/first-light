(in-package #:virality.examples)

;;; Materials

(v:define-material graph
    (:profiles (virality.materials:u-mvpt)
     :shader virality.shader.user:graph))

(v:define-material 3d-graph
    (:profiles (virality.materials:u-mvpt)
     :shader virality.shader.user:3d-graph-1
     :instances 1000
     :attributes (:depth :always)
     :uniforms
               ((:size 1)
                (:min 0)
                (:by 1))))

;;; Prefabs

(virality.prefab:define-prefab "graph" (:library examples :context context)
  (("camera" :copy "/cameras/ortho"))
  (("graph" :copy "/mesh")
   (v/comp:transform :scale (v3:vec (/ (v:option context :window-width) 2)
                                    (/ (v:option context :window-height) 2)
                                    0))
   (v/comp:render :material 'graph)))

(virality.prefab:define-prefab "3d-graph-1" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (v/comp:transform :translate (v3:vec 0 70 100))
   (v/comp:camera (:policy :new-args) :zoom 2)
   (v/comp:tracking-camera :target-actor (virality.prefab:ref "/3d-graph-1/graph")))
  (("graph" :copy "/mesh")
   (v/comp:render :material '(3d-graph
                              3d-graph-1
                              :shader virality.shader.user:3d-graph-1
                              :instances 100000
                              :uniforms ((:size 0.5))))))

(virality.prefab:define-prefab "3d-graph-2" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (v/comp:transform :translate (v3:vec 0 50 100))
   (v/comp:camera (:policy :new-args) :zoom 2)
   (v/comp:tracking-camera :target-actor (virality.prefab:ref "/3d-graph-2/graph")))
  (("graph" :copy "/mesh")
   (v/comp:render :material '(3d-graph
                              3d-graph-2
                              :shader virality.shader.user:3d-graph-2
                              :instances 100000
                              :uniforms ((:size 1))))))

;;; Prefab descriptors

(virality.prefab:define-prefab-descriptor graph ()
  ("graph" virality.examples:examples))

(virality.prefab:define-prefab-descriptor 3d-graph-1 ()
  ("3d-graph-1" virality.examples:examples))

(virality.prefab:define-prefab-descriptor 3d-graph-2 ()
  ("3d-graph-2" virality.examples:examples))
