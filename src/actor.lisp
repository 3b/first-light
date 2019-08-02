(in-package #:virality)

(defclass actor (queryable)
  ((%context :reader context
             :initarg :context)
   (%state :accessor state
           :initarg :state
           :initform :initialize)
   (%components :reader components
                :initform (u:dict))
   (%components-by-type :reader %components-by-type
                        :initform (u:dict))
   (%prefab-node :reader prefab-node
                 :initarg :prefab-node
                 :initform nil)
   (%ttl :accessor ttl
         :initarg :ttl
         :initform 0)))

(defun make-actor (context &rest args &key &allow-other-keys)
  (let ((actor (apply #'make-instance 'actor :context context args)))
    (register-object-uuid actor)
    (register-object-id actor)
    actor))

(defun spawn-actor (actor &key (parent :universe))
  "Take the ACTOR and place into the initializing db's and view's in the CORE.
The actor is not yet in the scene and the main loop protocol will not be called
on it or its components. If keyword argument :PARENT is supplied it is an actor
reference which will be the parent of the spawning actor. It defaults to
:universe, which means make this actor a child of the universe actor."
  (let ((core (core (context actor)))
        (actor-transform (component-by-type actor 'v/comp:transform)))
    (cond
      ((eq parent :universe)
       (unless (v/comp:parent actor-transform)
         (v/comp:transform-add-child
          (component-by-type (scene-tree core) 'v/comp:transform)
          (component-by-type actor 'v/comp:transform))))
      ((typep parent 'actor)
       (v/comp:transform-add-child
        (component-by-type parent 'v/comp:transform)
        (component-by-type actor 'v/comp:transform)))
      ((null parent)
       (u:noop))
      (t
       (error "Cannot parent actor ~s to unknown parent ~s" actor parent)))
    (setf (u:href (actor-preinit-db (tables core)) actor) actor)
    (u:do-hash-values (v (components actor))
      (setf (type-table (canonicalize-component-type (component-type v) core)
                        (component-preinit-by-type-view (tables core)))
            v))))

(defun actor/preinit->init (actor)
  (let ((core (core (context actor))))
    (remhash actor (actor-preinit-db (tables core)))
    (setf (u:href (actor-init-db (tables core)) actor) actor)))

(defun actor/init->active (actor)
  (let ((core (core (context actor))))
    (remhash actor (actor-init-db (tables core)))
    (setf (state actor) :active
          (u:href (actor-active-db (tables core)) actor) actor)))

(defmethod destroy-after-time ((thing actor) &key (ttl 0))
  (let* ((core (core (context thing)))
         (table (actor-predestroy-view (tables core))))
    (when (eq thing (scene-tree core))
      (error "Cannot destroy the scene tree root."))
    (setf (ttl thing) (and ttl (max 0 ttl)))
    (if ttl
        (setf (u:href table thing) thing)
        ;; If the TTL is stopped, we want to remove the actor from the
        ;; pre-destroy view!
        (remhash thing table))))

(defun actor/init-or-active->destroy (actor)
  ;; TODO: A different logic error (that of a destroyed object not having its
  ;; components also destroyed), and a destroyed object being destroyed
  ;; twice--which is legal but not explicitly handled) caused this UNLESS to be
  ;; here. Replace with new flow.
  (unless (and (eq (state actor) :destroy)
               (plusp (ttl actor)))
    (let ((tables (tables (core (context actor)))))
      (setf (u:href (actor-destroy-db tables) actor) actor
            (state actor) :destroy)
      (remhash actor (actor-predestroy-view tables))
      (unless (remhash actor (actor-active-db tables))
        (remhash actor (actor-preinit-db tables)))
      (u:do-hash-values (v (components actor))
        (setf (ttl v) 0)
        (enqueue-detach-event v actor)
        (component/init-or-active->destroy v)))))

(defun actor/destroy-descendants (actor)
  (flet ((destroy-actor (actor)
           (setf (ttl actor) 0)
           (actor/init-or-active->destroy actor)
           (deregister-object-uuid actor)
           (deregister-object-id actor)))
    (when actor
      (v/comp:map-nodes
       (lambda (x) (destroy-actor (actor x)))
       (component-by-type actor 'v/comp:transform)))))

(defun actor/disconnect (actor)
  (when (eq (id actor) 'universe)
    (error "Cannot disconnect the top-level universe node."))
  (let ((actor-transform (component-by-type actor 'v/comp:transform)))
    (v/comp:transform-remove-child (v/comp:parent actor-transform)
                                   actor-transform)))

(defun actor/destroy->released (actor)
  (unless (zerop (hash-table-count (components actor)))
    (error "actor/destroy->released: destroyed actor still has components!"))
  (remhash actor (actor-destroy-db (tables (core (context actor))))))

(defun actor/countdown-to-destruction (actor)
  (when (plusp (ttl actor))
    (decf (ttl actor) (frame-time (context actor)))))
