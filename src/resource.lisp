(in-package #:virality.engine)

;;; DSL Input

;; (define-resources (:project "project-name")
;;   (:core "data")
;;   (:mesh (:core "mesh"))
;;   (:shader (:core "shader"))
;;   (:texture (:core "texture"))
;;   (:char-meshes (:core :mesh "character"))
;;   (:debug-tex (:core :texture "debug.tiff"))
;;   (:project (:core "my-project")))

;;; DSL Expansion

;; Expands to a global definition form whose value bound is the following:
;; hash table (key -> value):

;; :core -> #p"data/"
;; (:core :mesh) -> #p"data/mesh/"
;; (:core :shader) -> #p"data/shader/"
;; (:core :texture) -> #p"data/texture/"
;; (:core :char-meshes) -> #p"data/mesh/character/"
;; (:core :debug-tex) -> #p"data/texture/debug.tiff"
;; :project -> #p"data/my-project/"

;;; Details:

;; 1) Resource Specifications and Identifiers

;; The body of `define-resources` is an arbitrary number of lists; each one
;; being a resource specification which describes where to locate a particular
;; resource. A resource specification has as its first element, a keyword symbol
;; to identify the resource about to be described, and must be unique with other
;; resource identifiers.

;; In the "DSL Input" example above, :shader is an example of a resource
;; identifier.

;; Another example in the above example is :core, which is a special resource
;; identifier. This is where all of Virality Engine's internal resources are
;; stored; here, they are located in the "data" directory directly in the user's
;; project working directory.

;; NOTE: The resource identifiers :core and :project have special semantics that
;; will be described progressively below.

;; NOTE: The order of defined resources is important. This is a simple
;; single-pass parser for ease of maintenance and extensibility, and therfor you
;; cannot build a path to a resource based off of another resource that is
;; defined later in the form. This is a tolerable restriction, as all resources
;; are defined within the same form for a project, and also the user would
;; likely order them in a logical fashion rather than jump around to mentally
;; resolve paths in a hurry, when not using the resource API functions. Resource
;; specifications are read top to bottom, and resolution will fail during
;; compilation time if this rule is not followed.

;; 2) Resource Path Specifications

;; Following the resource identifier in a resource specification is a path
;; specification, which is either a string denoting a root path, or a list
;; denoting how to build off of a pre-existing path.

;; In the "DSL Input" example above, "data" is an example of a string path
;; specification. This denotes that the resource identified by :core is a root
;; path located directly inside the user's project directory.

;; NOTE: Users are permitted to change the path specification of :core from
;; "data" to any string of their choosing, since it is part of their working
;; directory and they can make the required changes to the filesystem within
;; their project.

;; Apart from string path specifications, there is another form. If a path
;; specification is a list, its first element points to a pre-existing resource
;; identifier, hereafter known as the "base resource", and the second element is
;; a string with the same semantics as a string path specifier as described
;; above. In this form, The base resource path is resolved, and the string
;; merged onto the end of the base path to form a new resource path.

;; In the example above, :shader has a base resource as :core, and a string as
;; "shader". First, the resource identifier :core is resolved to #p"data/" and
;; then merged with "shader" to produce #p"data/shader/", as can be seen in the
;; "DSL Expansion" above.

;; There is also a third format for path specifications which is a list of three
;; elements. In this form, the second and last element behave exactly as in the
;; two-element format. The first element is used to disambiguate between :core
;; and :project, the two special resource identifiers. This is
;; because it is completely permitted to use the same keyword resource
;; identifier in a user's project, as one already defined internal to
;; Virality Engine for :core.

;; In the "DSL Input" example above, :debug-tex defines a resource that makes
;; use of the three-element path specification format. Here, we can see that
;; :debug-tex refers to the "debug.tiff" file located by the :texture identifier
;; in Virality Engine's core resource definitions, and not by resolving a
;; user-defined :texture identifier.

;; NOTE: In all three forms of path specifications, there is a string component
;; with consistent semantics. It represents a path to a resource in the
;; filesystem. Therefor, you are permitted to specify a string containing
;; slashes ("/" characters) to point to a file or directory, such as
;; "path/to/file.txt".

;; NOTE: A string component in a path specification is always treated as a
;; relative path. You cannot prepend a slash ("/" character) to a string to
;; force it being absolute. Relative pathnames are required for Common Lisp to
;; properly merge pathnames, and logically all paths to resources are relative
;; to the user's project. Care is taken to strip off any leading or trailing
;; slashes from supplied string components.

;; IMPORTANT: A string component in a path specification is treated as a file if
;; anywhere in the string is a dot ("." character). Paths to directories cannot
;; contain a dot, unless you mean that the last component of that path points to
;; a file. Likewise, a path specification for a file must contain a dot, so
;; ensure that your resource all have a file extensions. This is a fair
;; restriction, as it lets us easily delineate between a file or directory in
;; the DSL's parser, which is important when merging Common Lisp pathnames.

;; IMPORTANT: Virality Engine's :core resource path specification is not
;; permitted to use anything other than the string format, unlike other resource
;; path specifications. Using a list form, such as (:core (:project "data"))
;; will not define Virality Engine's core resources to be relative to the user's
;; project resources. Instead, a new core resource will be defined for the
;; user's project, since we do not enforce any restrictions on user project
;; resource identifier naming.

;; 3) Initialization Form

;; The initialization form of the DSL input is the form directly following the
;; macro name. In the "DSL Input" example above, that would be (:project
;; "project-name"). This is a propertly list of key/value pairs. Currently, the
;; only possible key is :project, and it is mandatory. This must match the name
;; of the ASDF system the user's project is defined in. This is used by the
;; resource resolver to get the full absolute path to a resource on disk when
;; loading the project in a development environment with ASDF.

;; NOTE: In the context of a deployed binary, paths are resolved relative to the
;; executable.

;; 4) API

;; TODO

(setf (meta 'resources) (u:dict #'equalp))

(defun %lookup-resource (key)
  (u:href (meta 'resources) key))

(defun make-relative-pathname (sub-path)
  (let ((components (split-sequence:split-sequence #\. sub-path)))
    (destructuring-bind (name &optional type) components
      (if type
          (make-pathname :name name :type type)
          (make-pathname :directory `(:relative ,name))))))

(defun build-resource-path (id key sub-path)
  (a:if-let ((base-path (%lookup-resource key)))
    (progn
      (when (pathname-type base-path)
        (error "~s is a file resource and cannot merge the sub-path ~s."
               key sub-path))
      (uiop/pathname:merge-pathnames*
       (make-relative-pathname (string-trim "/" sub-path))
       base-path))
    (error "Resource identifier ~s has not been defined yet when attempting to ~
            merge resource ~s."
           key id)))

(defun build-resource-path/2 (id path-spec)
  (destructuring-bind (key sub-path) path-spec
    (build-resource-path id key sub-path)))

(defun build-resource-path/3 (id path-spec)
  (destructuring-bind (root base sub-path) path-spec
    (let ((key (make-resource-key base root)))
      (build-resource-path id key sub-path))))

(defun make-resource-key (id root)
  (cond
    ((eq id :project)
     id)
    ((keywordp root)
     (list root id))
    (t
     id)))

(defun make-resource-path (id root path-spec)
  (flet ((make-path ()
           (case (length path-spec)
             (1 (make-relative-pathname root))
             (2 (build-resource-path/2 id path-spec))
             (3 (build-resource-path/3 id path-spec))
             (t (error "A path specifier in `define-resources` must be a ~
                        string, or a list of 2 or 3 elements."))))
         (validate-core-path (path)
           (u:when-found (key (%lookup-resource :project))
             (when (and (eq id :core)
                        (string= (namestring key) (namestring path)))
               (error "The :core and :project resource path specifications ~
                       must be unique."))))
         (validate-project-path (path)
           (u:when-found (key (%lookup-resource :core))
             (when (and (eq id :project)
                        (string= (namestring key) (namestring path)))
               (error "The :core and :project resource path specifications ~
                       must be unique.")))))
    (let ((path (make-path)))
      (validate-core-path path)
      (validate-project-path path)
      path)))

(defun store-resource-path (id path-spec)
  (unless (keywordp id)
    (error "Identifiers in `define-resources` must be keyword symbols, but ~
            found ~s."
           id))
  (let ((path-spec (a:ensure-list path-spec)))
    (destructuring-bind (root &rest rest) path-spec
      (declare (ignore rest))
      (let ((key (make-resource-key id root)))
        (setf (u:href (meta 'resources) key)
              (make-resource-path id root path-spec)))))
  (u:noop))

(defun get-resource-project (key)
  (let ((key (a:ensure-list key)))
    (destructuring-bind (x &rest rest) key
      (declare (ignore rest))
      (if (eq x :core)
          :virality
          (meta 'user-project)))))

(defun resolve-resource-id (id)
  (cond
    ((eq id :project)
     id)
    ((and (keywordp id)
          (not (eq id :core)))
     (list :project id))
    (t id)))

(defmacro define-resources ((&key project) &body body)
  (let (resources)
    (unless project
      (error "Project name must be specified in a `define-resources` form."))
    (push `(setf (meta 'user-project) ,project) resources)
    (dolist (spec body)
      (destructuring-bind (id path-spec) spec
        (push `(store-resource-path ',id ',path-spec) resources)))
    `(progn
       ,@(nreverse resources))))

;;; Protocol

(defun find-resource (context resource-id &optional sub-path)
  (let* ((id (resolve-resource-id resource-id))
         (resources (resources (core context)))
         (project (get-resource-project id)))
    (u:when-found (resource (u:href resources id))
      (let ((path (uiop:merge-pathnames* sub-path resource)))
        (resolve-system-path project path)))))

(defun print-all-resources ()
  (flet ((compare-keys (k1 k2)
           (let* ((k1 (a:ensure-list k1))
                  (k2 (a:ensure-list k2))
                  (i (mismatch k1 k2)))
             (when i
               (cond
                 ((= i (length k1)) t)
                 ((= i (length k2)) nil)
                 (t (string< (nth i k1) (nth i k2))))))))
    (let ((keys)
          (column-width 0))
      (u:do-hash-keys (k (meta 'resources))
        (push k keys)
        (when (listp k)
          (let ((key-width (length (symbol-name (cadr k)))))
            (when (> key-width column-width)
              (setf column-width (+ key-width 14))))))
      (format t "~&~va ~a~%~%" column-width "Identifier" "Relative path")
      (dolist (k (sort keys #'compare-keys))
        (let ((v (%lookup-resource k)))
          (format t "~&~vs ~s" column-width k (namestring v)))))))

(define-resources (:project :virality)
  (:core "data/core")
  (:ext (:core "ext"))
  (:mesh (:core "mesh"))
  (:texture (:core "texture"))
  (:gamepad-db (:core "gamepad-db.txt"))
  (:debug-tex (:core :texture "debug.tiff")))
