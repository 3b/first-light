(in-package #:virality.engine)

(defun qualify-component (core component-type)
  "This function tries to resolve the COMPONENT-TYPE symbol into a potentially
different packaged symbol of the same name that corresponds to a component
definition in that package. The packages are searched in the order they are are
defined in a toposort of the graph category COMPONENT-PACKAGE-ORDER. The result
should be a symbol suitable for MAKE-INSTANCE in all cases, but in the case of
mixin superclasses, it might not be desireable.

NOTE: If the component-type is a mixin class/component that is a superclass to a
component, then the first external to the package superclass definition found in
the package search order will be returned as the package qualified symbol.

NOTE: This function can not confirm that a symbol is a component defined by
DEFINE-COMPONENT. It can only confirm that the symbol passed to it is a
superclass of a DEFINE-COMPONENT form (up to but not including the COMPONENT
superclass type all components have), or a component created by the
DEFINE-COMPONENT form."
  (let ((search-table (component-search-table (tables core)))
        (component-type/class (find-class component-type nil))
        (base-component-type/class (find-class 'component))
        (graph (u:href (analyzed-graphs core) 'component-package-order)))
    (u:when-found (pkg-symbol (u:href search-table component-type))
      (return-from qualify-component pkg-symbol))
    (if (or (null component-type/class)
            (not (subtypep (class-name component-type/class)
                           (class-name base-component-type/class))))
        (dolist (potential-package (toposort graph))
          (let ((potential-package-name (second potential-package)))
            (dolist (pkg-to-search (u:href (pattern-matched-packages
                                            (annotation graph))
                                           potential-package-name))
              (u:mvlet ((symbol kind (find-symbol (symbol-name component-type)
                                                  pkg-to-search)))
                (when (and (eq kind :external)
                           (find-class symbol nil))
                  (setf (u:href search-table component-type) symbol)
                  (return-from qualify-component symbol))))))
        component-type)))

(defun component/preinit->init (component)
  (a:when-let ((thunk (initializer-thunk component)))
    (funcall thunk)
    (setf (initializer-thunk component) nil))
  (let* ((core (core (context component)))
         (type (canonicalize-component-type (component-type component) core))
         (preinit-view (component-preinit-by-type-view (tables core)))
         (init-view (component-init-by-type-view (tables core))))
    (type-table-drop component type preinit-view)
    (setf (type-table type init-view) component)))

(defun component/init->active (component)
  (let* ((core (core (context component)))
         (type (canonicalize-component-type (component-type component) core))
         (init-view (component-init-by-type-view (tables core)))
         (active-view (component-active-by-type-view (tables core))))
    (type-table-drop component type init-view)
    (setf (state component) :active
          (type-table type active-view) component)))

(defun component/init-or-active->destroy (component)
  (let* ((core (core (context component)))
         (type (canonicalize-component-type (component-type component) core))
         (tables (tables core))
         (preinit-view (component-preinit-by-type-view tables))
         (active-view (component-active-by-type-view tables))
         (predestroy-view (component-predestroy-view tables))
         (destroy-view (component-destroy-by-type-view tables)))
    (unless (plusp (ttl component))
      (setf (state component) :destroy
            (type-table type destroy-view) component)
      (remhash component predestroy-view)
      (unless (type-table-drop component type active-view)
        (type-table-drop component type preinit-view)))))

(defun component/destroy->released (component)
  (let* ((core (core (context component)))
         (type (canonicalize-component-type (component-type component) core))
         (destroy-view (component-destroy-by-type-view (tables core))))
    (type-table-drop component type destroy-view)
    (detach-component (actor component) component)
    (deregister-object-uuid component)
    (deregister-object-id component)))
