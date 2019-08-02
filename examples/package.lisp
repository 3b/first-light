(in-package #:cl-user)

(defpackage #:virality.examples
  (:local-nicknames (#:a #:alexandria)
                    (#:u #:golden-utils)
                    (#:v2 #:origin.vec2)
                    (#:v3 #:origin.vec3)
                    (#:v4 #:origin.vec4)
                    (#:m4 #:origin.mat4)
                    (#:q #:origin.quat)
                    (#:log #:verbose)
                    (#:v #:virality.engine)
                    (#:v/comp #:virality.components))
  (:use #:cl)
  (:export #:examples)
  (:export #:collision-smoke-test
           #:collision-test-0
           #:collision-test-1
           #:damaged-helmet
           #:geometric-volumes
           #:graph
           #:3d-graph-1
           #:3d-graph-2
           #:isometric-view
           #:noise
           #:sprite-1
           #:sprite-2
           #:texture))

;;;; This is a reasonably scoped shmup that demonstrates use of the engine.
(defpackage #:protect-the-planets
  (:local-nicknames (#:a #:alexandria)
                    (#:u #:golden-utils)
                    (#:v2 #:origin.vec2)
                    (#:v3 #:origin.vec3)
                    (#:v4 #:origin.vec4)
                    (#:m4 #:origin.mat4)
                    (#:q #:origin.quat))
  (:use #:cl)
  ;; prefab library
  (:export #:ptp-base)
  ;; prefab descriptor for "scenes" to start.
  (:export #:ptp
           #:starfield-demo))
