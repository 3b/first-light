(in-package #:cl-user)

(defpackage #:virality.nicknames
  (:use #:cl)
  (:import-from
   #+sbcl #:sb-ext
   #+ccl #:ccl
   #+(or ecl abcl clasp) #:ext
   #:add-package-local-nickname)
  (:export #:define-nicknames))

(in-package #:virality.nicknames)

(defmacro define-nicknames (&body body)
  `(progn
     ,@(mapcan
        (lambda (x)
          (destructuring-bind (source . mappings) x
            (mapcar
             (lambda (x)
               `(add-package-local-nickname ,@x ,source))
             mappings)))
        body)))

(define-nicknames
  (:virality.engine
   (:a :alexandria)
   (:u :golden-utils)
   (:log :verbose)
   (:v2 :origin.vec2)
   (:v3 :origin.vec3)
   (:v/comp :virality.components))

  (:virality.actions
   (:a :alexandria)
   (:u :golden-utils)
   (:log :verbose)
   (:v4 :origin.vec3)
   (:q :origin.quat)
   (:v :virality.engine)
   (:v/comp :virality.components))

  (:virality.annotations
   (:v :virality.engine))

  (:virality.components
   (:a :alexandria)
   (:u :golden-utils)
   (:log :verbose)
   (:~ :origin.swizzle)
   (:v2 :origin.vec2)
   (:v3 :origin.vec3)
   (:v4 :origin.vec4)
   (:m4 :origin.mat4)
   (:q :origin.quat)
   (:v :virality.engine))

  (:virality.gpu
   (:a :alexandria)
   (:u :golden-utils)
   (:log :verbose)
   (:v :virality.engine))

  (:virality.materials
   (:v2 :origin.vec2)
   (:v3 :origin.vec3)
   (:v4 :origin.vec4)
   (:m4 :origin.mat4)
   (:v :virality.engine))

  (:virality.prefab
   (:a :alexandria)
   (:u :golden-utils)
   (:log :verbose)
   (:v :virality.engine)
   (:v/comp :virality.components))

  (:virality.textures
   (:v4 :origin.vec4)
   (:v :virality.engine))

  (:virality.shader.swizzle
   (:a :alexandria)
   (:u :golden-utils))

  (:virality.shader.user
   (:u :golden-utils)))
