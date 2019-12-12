(in-package #:virality.examples)

;;; Textures

(v:define-texture damaged-helmet/metallic-roughness (:texture-2d)
  (:data #((:damaged-helmet-textures "metal-roughness.tiff"))))

(v:define-texture damaged-helmet/color (:texture-2d)
  (:data #((:damaged-helmet-textures "albedo.tiff"))))

(v:define-texture damaged-helmet/normal (:texture-2d)
  (:data #((:damaged-helmet-textures "normal.tiff"))))

(v:define-texture damaged-helmet/ambient-occlusion (:texture-2d)
  (:data #((:damaged-helmet-textures "ao.tiff"))))

(v:define-texture damaged-helmet/emissive (:texture-2d)
  (:data #((:damaged-helmet-textures "emissive.tiff"))))

;;; Materials

(v:define-material damaged-helmet
  (:shader ex/shd:damaged-helmet
   :profiles (x/mat:u-mvp)
   :uniforms
   ((:metallic-roughness-values (v2:one))
    (:metallic-roughness-sampler 'damaged-helmet/metallic-roughness)
    (:base-color-sampler 'damaged-helmet/color)
    (:base-color-factor (v4:one))
    (:normal-sampler 'damaged-helmet/normal)
    (:normal-scale 1.0)
    ;; NOTE: This vector points TOWARDS the light.
    (:light-direction (v3:vec 0 1 1))
    (:light-color (v3:one))
    (:occlusion-sampler 'damaged-helmet/ambient-occlusion)
    (:occlusion-strength 1.0)
    (:emissive-sampler 'damaged-helmet/emissive)
    (:emissive-factor 0.3))))

;;; Prefabs

(v:define-prefab "damaged-helmet" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (c/cam:camera (:policy :new-args) :zoom 10))
  (("helmet" :copy "/mesh")
   (c/xform:transform :rotate (q:orient :local :x (/ pi 2))
                      :rotate/inc (c/xform:angular-velocity :z (- (* pi 1/6)))
                      :scale 4)
   (c/smesh:static-mesh :asset '(:mesh "damaged-helmet.glb"))
   (c/render:render :material 'damaged-helmet)))

;;; Prefab descriptors

(v:define-prefab-descriptor damaged-helmet ()
  ("damaged-helmet" examples))
