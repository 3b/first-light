(in-package #:defpackage+-user-1)

(defpackage+ #:virality.shader
    (:inherit-from
     #:virality.gpu
     #:define-function
     #:define-struct
     #:define-macro
     #:define-shader)
  (:inherit
   #:cl
   #:vari)
  ;; structs
  (:export
   #:mesh-attrs
   #:mesh/pos
   #:mesh/normal
   #:mesh/tangent
   #:mesh/color
   #:mesh/uv1
   #:mesh/uv2
   #:mesh/joints
   #:mesh/weights)
  ;; utilities
  (:export
   #:mvlet*)
  ;; math
  (:export
   #:+epsilon+
   #:+pi+
   #:+half-pi+
   #:log10
   #:saturate
   #:map-domain))

(defpackage #:virality.shader.swizzle
  (:use #:cl #:vari))

(defpackage+ #:virality.shader.user
  (:use #:virality.shader.swizzle)
  (:inherit #:virality.shader))

(in-package #:cl-user)

(defpackage #:virality.shader.color
  (:use #:virality.shader
        #:virality.shader.swizzle)
  ;; color space conversion
  (:export
   #:rgb->grayscale
   #:hue->rgb
   #:rgb->hcv
   #:rgb->hsv
   #:hsv->rgb
   #:rgb->hcy
   #:hcy->rgb
   #:rgb->hsl
   #:hsl-rgb
   #:rgb->srgb-approx
   #:rgb->srgb
   #:srgb->rgb-approx
   #:srgb->rgb
   #:rgb->xyz
   #:xyz->rgb
   #:xyy->xyz
   #:xyz->xyy
   #:rgb->xyy
   #:xyy->rgb)
  ;; color grading
  (:export
   #:set-exposure
   #:set-saturation
   #:set-contrast
   #:set-brightness
   #:set-gamma
   #:color-filter
   #:tone-map/linear
   #:tone-map/reinhard
   #:tone-map/haarm-peter-duiker
   #:tone-map/hejl-burgess-dawson
   #:tone-map/uncharted2))

(defpackage #:virality.shader.graph
  (:use #:virality.shader
        #:virality.shader.swizzle)
  (:export
   #:graph))

(defpackage #:virality.shader.shaping
  (:use #:virality.shader
        #:virality.shader.swizzle)
  ;; penner
  (:export
   #:linear
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
   #:bounce-in-out)
  ;; iq
  (:export
   #:almost-identity
   #:impulse
   #:cubic-pulse
   #:exponential-step
   #:gain
   #:parabola
   #:power-curve
   #:sinc-curve)
  ;; levin
  (:export
   #:exponential-emphasis
   #:double-exponential-seat
   #:double-exponential-sigmoid
   #:logistic-sigmoid
   #:double-circle-seat
   #:double-circle-sigmoid
   #:double-elliptical-seat
   #:double-elliptical-sigmoid
   #:blinn-wyvill-raised-inverted-cosine
   #:double-cubic-seat
   #:double-cubic-seat/linear-blend
   #:double-odd-polynomial-seat
   #:quadratic-point)
  ;; misc
  (:export
   #:hermite-curve
   #:quintic-curve
   #:quintic-curve/interpolate-derivative
   #:quintic-curve/derivative
   #:quintic-curve/fast
   #:quintic-hermite
   #:quintic-hermite/derivative
   #:falloff-squared-c1
   #:falloff-squared-c2))

(defpackage #:virality.shader.hash
  (:use #:virality.shader
        #:virality.shader.swizzle)
  (:export
   #:blum-blum-shub
   #:blum-blum-shub/hq
   #:sgpp
   #:sgpp/2-per-corner
   #:sgpp/3-per-corner
   #:fast32
   #:fast32/2-per-corner
   #:fast32/3-per-corner
   #:fast32/4-per-corner
   #:fast32/cell
   #:fast32-2
   #:fast32-2/4-per-corner))

(defpackage #:virality.shader.noise
  (:use #:virality.shader
        #:virality.shader.swizzle)
  (:export
   #:perlin
   #:perlin/derivs
   #:perlin-surflet
   #:perlin-surflet/derivs
   #:perlin-improved
   #:cellular
   #:cellular/derivs
   #:cellular-fast
   #:polkadot
   #:polkadot-box
   #:hermite
   #:hermite/derivs
   #:simplex-perlin
   #:simplex-perlin/derivs
   #:simplex-cellular
   #:simplex-polkadot
   #:value
   #:value/derivs
   #:value-perlin
   #:value-hermite
   #:cubist
   #:stars))

(defpackage #:virality.shader.sdf
  (:use #:virality.shader
        #:virality.shader.swizzle)
  (:export
   #:dist/box
   #:dist/circle
   #:dist/line
   #:dist/pie
   #:dist/semi-circle
   #:dist/triangle
   #:mask/fill
   #:mask/inner-border
   #:mask/outer-border))

(defpackage #:virality.shader.texture
  (:use #:virality.shader
        #:virality.shader.swizzle)
  (:export
   #:unlit-color
   #:unlit-color-decal
   #:unlit-texture
   #:unlit-texture-decal))

(defpackage #:virality.shader.sprite
  (:use #:virality.shader
        #:virality.shader.swizzle)
  (:export
   #:sprite))

(defpackage #:virality.shader.visualization
  (:use #:virality.shader
        #:virality.shader.swizzle)
  (:export
   #:collider/sphere))
