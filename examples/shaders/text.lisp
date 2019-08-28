(in-package #:virality.examples.shaders)

(define-struct text-attrs
  (pos :vec3)
  (normal :vec3)
  (uv :vec2))

(define-struct text-atlas
  (sampler :sampler-2d :accessor sampler)
  (index :int :accessor index)
  (pixel-range :float :accessor pixel-range)
  (mode :int :accessor mode))


(define-function text/vert ((attrs text-attrs)
                            &uniform
                            (model :mat4)
                            (view :mat4)
                            (proj :mat4))
  (with-slots (pos uv) attrs
    (values (* proj view model (vec4 pos 1))
            uv)))

(define-function median ((v :vec3))
  (let ((a (.x v))
        (b (.y v))
        (c (.z v)))
    (max (min a b) (min (max a b) c))))

(define-function sample ((sampler :sampler-2d)
                         (mode :int)
                         (uv :vec2))
  (case mode
    ;; 1 = msdf
    (1 (- (median (.xyz (texture sampler uv))) 0.5))
    ;; 0 = sdf/psdf
    (0 (- (.a (texture sampler uv)) 0.5))
    ;; 2 = inverted msdf
    (otherwise (- (- 1 (median (.xyz (texture sampler uv)))) 0.5))))

(define-function text/frag ((uv :vec2)
                            &uniform
                            (font text-atlas))
  (let* ((s #++(.aaa (texture (sampler font)
                           uv
                           #++(/ uv
                          (texture-size (sampler font) 0))))
            (sample (sampler font) (mode font) uv))
         (unit (/ (pixel-range font)
                  (texture-size (sampler font) 0)))
         (d s)
         (c (clamp (+ (* d (dot unit (/ 0.5 (fwidth uv))))
                      0.5)
                   0 1))
         (co (clamp (+ (* (+ d 0.3) (dot unit (/ 0.5 (fwidth uv))))
                       0.5)
                    0 1)))

    (if (< co 0.01)
        (discard)
        (vec4 (mix (vec3 0 0 0) (vec3 1 1 1) c) co))
    #++
    (if (< c 0.01)
        (discard)
        (vec4 1 1 1 c))))

(define-shader text ()
  (:vertex (text/vert text-attrs))
  (:fragment (text/frag :vec2)))
