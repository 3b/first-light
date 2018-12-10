(in-package :defpackage+-user-1)

(defpackage+ #:fl.math
  (:nicknames #:flm)
  (:use #:cl)
  (:import-from #:specialization-store #:defstore #:defspecialization)
  (:shadow #:+
           #:-
           #:*
           #:/
           #:length
           #:=
           #:<
           #:<=
           #:>
           #:>=
           #:expt
           #:sqrt
           #:floor
           #:ceiling
           #:mod
           #:round
           #:abs
           #:min
           #:max
           #:sin
           #:cos
           #:tan
           #:asin
           #:acos
           #:atan
           #:trace
           #:conjugate)
  (:export #:vec2
           #:vec3
           #:vec4
           #:mat2
           #:mat3
           #:mat4
           #:quat
           #:with-vec2
           #:with-vec3
           #:with-vec4
           #:with-mat2
           #:with-mat3
           #:with-mat4
           #:with-quat
           #:+zero-vec2+
           #:+zero-vec3+
           #:+zero-vec4+
           #:+zero-mat2+
           #:+zero-mat3+
           #:+zero-mat4+
           #:+zero-quat+
           #:+id-mat2+
           #:+id-mat3+
           #:+id-mat4+
           #:+id-quat+
           #:copy
           #:copy-into
           #:.x
           #:.y
           #:.z
           #:.w
           #:rand
           #:zero
           #:zero-p
           #:unit-p
           #:id
           #:id-p
           #:clamp
           #:stabilize
           #:+
           #:-
           #:*
           #:/
           #:sign
           #:fract
           #:dot
           #:cross
           #:length-squared
           #:length
           #:distance-squared
           #:distance
           #:normalize
           #:negate
           #:lerp
           #:angle
           #:radians
           #:degrees
           #:=
           #:~
           #:<
           #:<=
           #:>
           #:>=
           #:expt
           #:sqrt
           #:floor
           #:ceiling
           #:mod
           #:round
           #:abs
           #:min
           #:max
           #:sin
           #:cos
           #:tan
           #:asin
           #:acos
           #:atan
           #:direction=
           #:parallel-p
           #:get-array
           #:get-column
           #:set-column
           #:get-translation
           #:set-translation
           #:translate
           #:rotate
           #:get-scale
           #:set-scale
           #:scale
           #:transpose
           #:orthogonal-p
           #:trace
           #:diagonal-p
           #:main-diagonal
           #:anti-diagonal
           #:determinant
           #:invert-orthonormal
           #:invert
           #:set-view
           #:set-projection/orthographic
           #:set-projection/perspective
           #:conjugate
           #:inverse
           #:slerp)
  ;; shaping
  (:export #:linear
           #:sine-out
           #:sine-in
           #:sine-in-out
           #:quadratic-out
           #:quadratic-in
           #:quadratic-in-out
           #:cubic-out
           #:cubic-in
           #:cubic-in-out
           #:quartic-out
           #:quartic-in
           #:quartic-in-out
           #:quintic-out
           #:quintic-in
           #:quintic-in-out
           #:exponential-out
           #:exponential-in
           #:exponential-in-out
           #:circular-out
           #:circular-in
           #:circular-in-out
           #:back-out
           #:back-in
           #:back-in-out
           #:elastic-out
           #:elastic-in
           #:elastic-in-out
           #:bounce-out
           #:bounce-in
           #:bounce-in-out
           #:hermite-curve
           #:quintic-curve))