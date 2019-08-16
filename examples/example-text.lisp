(in-package #:virality.examples)
#++ (ql:quickload 'virality.examples)
#++ (ql:quickload '(3b-bmfont 3b-bmfont/xml 3b-bmfont/json))
;; text

(v:define-options ()
  :window-width 800
  :window-height 600
  :window-x 2500
  :window-y 500
  :vsync :on
  :initial-scene 'text)

(v:define-geometry-layout text
  (:data (:format interleaved)
         (position :type float :count 3)
         (normal :type float :count 3)
         (uv :type float :count 3)))

(v:define-geometry text
  :layout text
  :primitive :triangle-strip
  :vertex-count 4
  :buffers
  (:data (((-0.5 0.5 0) (0 0 1) (-1 1 0))
          ((-0.5 -0.5 0) (0 0 1) (-1 -1 0))
          ((0.5 0.5 0) (0 0 1) (1 1 0))
          ((0.5 -0.5 0) (0 0 1) (1 -1 0)))))

;;; components

(v:define-component text ()
  ((%text :reader text
          :initarg :text
          :initform "text goes here...")
   (font :reader font :initarg :fontspec)
   (target :reader target :initarg :target-geometry))
  ((:cached-font-data equalp)))

(defun quad* (x1 y1 x2 y2 u1 v1 u2 v2)
  `(((,x1 ,y2 0) (0 0 1) (,u1 ,v2 0))
    ((,x1 ,y1 0) (0 0 1) (,u1 ,v1 0))
    ((,x2 ,y2 0) (0 0 1) (,u2 ,v2 0))

    ((,x2 ,y2 0) (0 0 1) (,u2 ,v2 0))
    ((,x1 ,y1 0) (0 0 1) (,u1 ,v1 0))
    ((,x2 ,y1 0) (0 0 1) (,u2 ,v1 0))))

(defun quad (x y w h)
  (quad* x y (+ x w) (+ y h)
         -1 -1 1 1))


(defmethod v:on-component-initialize ((self text))
  (let ((context (v:context self)))
    (with-slots (%text target font) self
      (format t "init: ~s~%" %text)
      (format t "init: ~s~%" (c/dmesh::geometry target))
      (v:with-shared-storage (context context)
                             ((cached-font font-present-p
                                           ('text :cached-font-data font)
                                           (3b-bmfont:read-bmfont
                                            (if (consp font)
                                                (apply #'v::find-resource
                                                       context font)
                                                (v::find-resource context font)))))
        (setf font cached-font))
      #++(let* ((.l (layout-string %text font))
             (length (first .l))
             (l (second .l))
             (d (print (loop with x = (- (/ length 2))
                       with y = (float (- (ascender font)))
                       for (x1 y1 x2 y2 u1 v1 u2 v2) in l
                       append (quad* (+ x x1) (+ y y1) (+ x x2) (+ y y2)
                                     u1 v1 u2 v2)))))
        (virality.geometry::update-dynamic-geometry
         (c/dmesh::geometry target)
         :triangles (length d)
         :data d))
      (let* ((d))
        (3b-bmfont::map-glyphs font
                               (lambda (x y x2 y2
                                        u1 v1 u2 v2)
                                 (setf d
                                       (append (quad* x y x2 y2
                                                      u1 v2 u2 v1)
                                               d)))
                               %text
                               :y-up t)
        (print d)
        (virality.geometry::update-dynamic-geometry
         (c/dmesh::geometry target)
         :triangles (length d)
         :data d)))))

;;; Prefabs
#++
(v:define-texture noto-atlas (:texture-2d)
  (:data #(:noto-font)))
#++
(v:define-texture georgia-atlas (:texture-2d)
  (:data #(:georgia-font)))

(v:define-texture dejavu-atlas (:texture-2d)
  (:data #(:deja-font)))
#++
(v:define-texture georgia-atlas (:texture-2d)
  (:data #(:georgia-font)))

(v:define-texture roboto-json-atlas (:texture-2d)
  (:data #((:fonts "Roboto-msdf.png"))))

(v:define-texture roboto-xml-atlas (:texture-2d)
  (:data #((:fonts "Roboto-Regular.png"))))

(v:define-texture yahei-atlas (:texture-2d)
  (:data #((:fonts "yahei.png"))))

#++
(v:define-material text-noto
  (:profiles (x/mat:u-mvp)
   :shader ex/shd:text
   :uniforms ((:font.sampler 'noto-atlas))))

(v:define-material text-dejavu
  (:profiles (x/mat:u-mvp)
   :shader ex/shd:text
   :uniforms ((:font.sampler 'dejavu-atlas)
              (:font.pixel-range 4)
              (:font.mode 0))))
#++
(v:define-material text-georgia
  (:profiles (x/mat:u-mvp)
   :shader ex/shd:text
   :uniforms ((:font.sampler 'georgia-atlas)
              (:font.pixel-range 4)
              (:font.mode 0))))

(v:define-material text-roboto-json
  (:profiles (x/mat:u-mvp)
   :shader ex/shd:text
   :uniforms ((:font.sampler 'roboto-json-atlas)
              (:font.pixel-range 4)
              (:font.mode 2))))

(v:define-material text-roboto-xml
  (:profiles (x/mat:u-mvp)
   :shader ex/shd:text
   :uniforms ((:font.sampler 'roboto-xml-atlas)
              (:font.pixel-range 4)
              (:font.mode 1))))

(v:define-material text-yahei
  (:profiles (x/mat:u-mvp)
   :shader ex/shd:text
   :uniforms ((:font.sampler 'yahei-atlas)
              (:font.pixel-range 4)
              (:font.mode 1))))



(v:define-prefab "text" (:library examples)
  (("camera" :copy "/cameras/perspective"))
  #++(("plane")
   (c/xform:transform             ;:rotate/inc (q:orient :local :x pi)
    :scale (v3:vec 0.5 0.5 0.5))
   (c/dmesh:dynamic-mesh :geometry 'text)
   (text :target-geometry (v:ref :self :component 'c/dmesh:dynamic-mesh)
         :fontspec :georgia-fontdata)
   (c/render:render :material 'text-georgia
                    :mode :dynamic-mesh))
  #++(("label")
   (c/xform:transform :translate (v3:vec -40 50 0)
                      :scale (v3:vec 0.3 0.3 0.3))
   (c/dmesh:dynamic-mesh :geometry 'text)
   (text :target-geometry (v:ref :self :component 'c/dmesh:dynamic-mesh)
         :text "label 1"
         :fontspec :noto-fontdata)
   (c/render:render :material 'text-noto
                    :mode :dynamic-mesh))
  (("deja")
   (c/xform:transform :translate (v3:vec -50 60 0)
                      :scale (v3:vec 0.3 0.3 0.3))
   (c/dmesh:dynamic-mesh :geometry 'text)
   (text :target-geometry (v:ref :self :component 'c/dmesh:dynamic-mesh)
         :text "DejaVu
text bmfont V."
         :fontspec :deja-fontdata)
   (c/render:render :material 'text-dejavu
                    :mode :dynamic-mesh))
  (("roboto-json")
   (c/xform:transform :translate (v3:vec -50 30 0)
                      :scale (v3:vec 0.3 0.3 0.3))
   (c/dmesh:dynamic-mesh :geometry 'text)
   (text :target-geometry (v:ref :self :component 'c/dmesh:dynamic-mesh)
         :text "Roboto	jsonV."
         :fontspec '(:fonts "Roboto-msdf.json"))
   (c/render:render :material 'text-roboto-json
                    :mode :dynamic-mesh))
  (("roboto-xml")
   (c/xform:transform :translate (v3:vec -50 25 0)
                      :scale (v3:vec 0.3 0.3 0.3))
   (c/dmesh:dynamic-mesh :geometry 'text)
   (text :target-geometry (v:ref :self :component 'c/dmesh:dynamic-mesh)
         :text "Roboto xmlV."
         :fontspec '(:fonts "Roboto-Regular.fnt"))
   (c/render:render :material 'text-roboto-xml
                    :mode :dynamic-mesh))
  (("yahei")
   (c/xform:transform :translate (v3:vec -50 0 0)
                      :scale (v3:vec 0.3 0.3 0.3))
   (c/dmesh:dynamic-mesh :geometry 'text)
   (text :target-geometry (v:ref :self :component 'c/dmesh:dynamic-mesh)
         :text "yahei 世界 r."
         :fontspec '(:fonts "yahei-msdf.json"))
   (c/render:render :material 'text-yahei
                    :mode :dynamic-mesh)))

;;; Prefab descriptors

(v:define-prefab-descriptor text ()
  ("text" examples))

#++
(ql:quickload 'virality.examples)

#++
(virality.engine::start :Scene 'text)
#++
(untrace)
#++
virality.engine::*core-debug*


;;(float 17104/681)25.116005
;;hash-table

#++
(uiop:xdg-config-home)
