(in-package #:virality.engine)

(defclass component (queryable)
  ((%context :reader context
             :initarg :context
             :initform nil)
   (%type :reader component-type
          :initarg :type)
   (%state :accessor state
           :initarg :state
           :initform :initialize)
   (%actor :accessor actor
           :initarg :actor
           :initform nil)
   (%ttl :accessor ttl
         :initarg :ttl
         :initform 0)
   (%initializer-thunk :accessor initializer-thunk
                       :initarg :initializer-thunk
                       :initform nil)
   (%attach/detach-event-queue :accessor attach/detach-event-queue
                               :initarg :attach/detach-event-queue
                               :initform (queues:make-queue :simple-queue)))
  (:metaclass component-class))

(defmethod initialize-instance :after ((instance component) &key)
  (register-object-uuid instance)
  (register-object-id instance))

(defmethod make-component (context component-type &rest args)
  (a:if-let ((type (qualify-component (core context) component-type)))
    (apply #'make-instance type :type type :context context args)
    (error "Could not qualify the component type ~s." component-type)))

(defun attach-component (actor component)
  (detach-component actor component)
  (enqueue-attach-event component actor)
  (setf (actor component) actor
        (u:href (components actor) component) component)
  (let* ((core (core (context actor)))
         (qualified-type (qualify-component core (component-type component))))
    (push component (u:href (%components-by-type actor) qualified-type))))

(defun attach-components (actor &rest components)
  (dolist (component components)
    (attach-component actor component)))

(defun detach-component (actor component)
  "If COMPONENT is contained in the ACTOR. Remove it. Otherwise, do nothing."
  (when (remhash component (components actor))
    (symbol-macrolet ((typed-components (u:href (%components-by-type actor)
                                                (component-type component))))
      (enqueue-detach-event component actor)
      (setf (actor component) nil)
      (setf typed-components
            (remove-if (lambda (x) (eq x component)) typed-components)))))

(defun component-by-type (actor component-type)
  "Get the first component of type COMPONENT-TYPE for the given ACTOR.
Returns the rest of the components as a secondary value if there are more than
one of the same type."
  (let* ((core (core (context actor)))
         (qualified-type (qualify-component core component-type))
         (components (u:href (%components-by-type actor) qualified-type)))
    (values (first components)
            (rest components))))

(defmethod destroy-after-time ((thing component) &key (ttl 0))
  (let* ((core (core (context thing)))
         (table (u:href (component-predestroy-view (tables core)))))
    (setf (ttl thing) (and ttl (max 0 ttl)))
    (if ttl
        (setf (u:href table thing) thing)
        ;; If the TTL is stopped, we want to remove the component from the
        ;; pre-destroy view!
        (remhash thing table))))

(defun component/countdown-to-destruction (component)
  (when (plusp (ttl component))
    (decf (ttl component) (frame-time (context component)))))

(defun enqueue-attach-event (component actor)
  (queues:qpush (attach/detach-event-queue component) (list :attached actor)))

(defun enqueue-detach-event (component actor)
  (queues:qpush (attach/detach-event-queue component) (list :detached actor)))

(defun dequeue-attach/detach-event (component)
  ;; NOTE: Returns NIL, which is not in our domain, when empty.
  (queues:qpop (attach/detach-event-queue component)))

(defun component/invoke-attach/detach-events (component)
  (loop :for item = (queues:qpop (attach/detach-event-queue component))
        :for (event-kind actor) = item
        :while event-kind
        :do (ecase event-kind
              (:attached (on-component-attach component actor))
              (:detached (on-component-detach component actor)))))
