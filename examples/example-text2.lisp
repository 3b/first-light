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


;;; fonts

(define-font roboto-xml ()
  ;; metadata includes atlas, field type and range
  (:fontspec (:fonts "Roboto-Regular.fnt")))

(define-font yahei ()
  (:fontspec (:fonts "yahei-msdf.json")))

(define-font roboto-json ()
  (:fontspec (:fonts "Roboto-msdf.json"))
  ;; json file doesn't specify sdf parameters, so specify explicitly
  (:type :inverted-msdf)
  (:range 4))

(define-font dejavu ()
  (:fontspec (:fonts "DejaVu-sdf.fnt"))
  ;; fnt file doesn't specify sdf parameters, so specify explicitly
  (:type :sdf)
  (:range 4)
  ;; fnt file has wrong atlas name, override
  (:atlas (:fonts "DejaVu-sdf.png")))

;;; Prefabs

(v:define-prefab "text" (:library examples)
  (("camera" :copy "/cameras/perspective"))
  (("deja")
   (c/xform:transform :translate (v3:vec -50 60 0)
                      :scale (v3:vec 0.3 0.3 0.3))
   (text :text "DejaVu
text bmfont V."
         :font dejavu))
  (("roboto-json")
   (c/xform:transform :translate (v3:vec -50 30 0)
                      :scale (v3:vec 0.3 0.3 0.3))
   (text :text "Roboto	jsonV."
         :font roboto-json))
  (("roboto-xml")
   (c/xform:transform :translate (v3:vec -50 25 0)
                      :scale (v3:vec 0.3 0.3 0.3))
   (text :text "Roboto xmlV."
         :font roboto-xml))
  (("yahei")
   (c/xform:transform :translate (v3:vec -50 0 0)
                      :scale (v3:vec 0.3 0.3 0.3))
   (text :text "yahei 世界 r."
         :font yahei))
  (("yahei2")
   (c/xform:transform :translate (v3:vec -50 120 1)
                      :scale (v3:vec 1.6 1.6 0.3))
   (text :text "界"
         :font yahei)))

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
(virality.engine::stop virality.engine::*core-debug*)

