(in-package :fl.core)

(defclass analyzed-graph ()
  ((%category :accessor category
              :initarg :category
              :documentation "The category this graph is for.")
   (%graph :accessor graph
           :initarg :graph
           :initform nil
           :documentation "The completed graph representation of this category.")
   (%toposort :accessor toposort
              :initarg :toposort
              :initform nil
              :documentation "A topological sort of the graph.")
   (%annotation :accessor annotation
                :initarg :annotation
                :initform nil
                :documentation "Context-specific annotations for categories.")
   (%graphdefs :accessor graphdefs
               :initarg :graphdefs
               :initform  (make-hash-table)
               :documentation "graph definitions that participated, keyed by
name, with graphdef instance as value.")))

(defun make-analyzed-graph (&rest init-args)
  (apply #'make-instance 'analyzed-graph init-args))

(defclass graphdef ()
  ((%name :accessor name
          :initarg :name)
   (%original-form :reader original-form
                   :initarg :original-form)
   (%enabled :accessor enabled
             :initarg :enabled)
   (%category :accessor category
              :initarg :category)
   ;; This starts out as a list grabbed from the graph-definition dsl, but we
   ;; transform it later into a hash table with values of graphdef-depends-on
   ;; instances to hold a richer semantic understanding of original depends-on
   ;; forms in the graph-definition dsl.
   (%depends-on :accessor depends-on
                :initarg :depends-on)
   (%roots :accessor roots
           :initarg :roots)
   (%subforms :accessor subforms
              :initarg :subforms)))

(defun make-graphdef (name &rest init-args)
  (apply #'make-instance 'graphdef :name name init-args))

(defclass graphdef-depends-on ()
  ((%name :accessor name
          :initarg :name)
   ;; when we transmute the original form into this class type, we reserve it
   ;; for future debugging.
   (%original-form :accessor original-form
                   :initarg :original-form)
   ;; a reference to the real graphdef containing the referenced subforms.
   (%graphdef :accessor graphdef
              :initarg :graphdef)
   ;; a hash table keyed by subform name, and value is the subform itself in the
   ;; appropriate graphdef instance.
   (%subforms :accessor subforms
              :initarg :subforms
              :initform (make-hash-table))))

(defun make-graphdef-depends-on (name &rest init-args)
  (apply #'make-instance 'graphdef-depends-on :name name init-args))

(defclass subform ()
  ((%name :accessor name
          :initarg :name)
   (%kind :accessor kind
          :initarg :kind)
   (%depforms :accessor depforms
              :initarg :depforms)))

(defun make-subform (name &rest init-args)
  (apply #'make-instance 'subform :name name init-args))

(defclass depform ()
  ;; the original depform for debugging/error output
  ((%original-form :accessor original-form
                   :initarg :original-form)
   ;; the lifted form with the new symbols, if any
   (%canonical-form :accessor canonical-form
                    :initarg :canonical-form)
   ;; :empty, :vertex, or :hyperedges
   (%kind :accessor kind
          :initarg :kind)))

(defun make-depform (&rest init-args)
  (apply #'make-instance 'depform init-args))

;;; annotation classes / protocol for different graph categories

(defgeneric generate-graph-annotation (category angph)
  (:documentation "Generate any annotations this analyzed-graph may need.")
  (:method (category angph)))

(defgeneric make-graph-annotation (category &rest init-args)
  (:documentation "Make an instance of an appropriate graph annotation depending
on cateogry.")
  ;; Default method is no annotation at all.
  (:method (category &rest init-args)))

;; For category COMPONENT-DEPENDENCY
(defclass graph-annotation/component-dependency ()
  ((%unknown-type-id :accessor unknown-type-id
                     :initarg :unknown-type-id)
   (%referenced-types :accessor referenced-types
                      :initarg :referenced-types
                      :initform (make-hash-table))))

(defmethod make-graph-annotation ((category (eql 'component-dependency))
                                  &rest init-args)
  (apply #'make-instance 'graph-annotation/component-dependency init-args))

;; For category COMPONENT-PACKAGE-SEARCH-ORDER
(defclass graph-annotation/component-package-search-order ()
  ((%pattern-matched-packages :accessor pattern-matched-packages
                              :initarg :pattern-matched-packages
                              :initform (make-hash-table))))

(defmethod make-graph-annotation
    ((category (eql 'component-package-search-order)) &rest init-args)
  (apply #'make-instance 'graph-annotation/component-package-search-order
         init-args))

;; TODO: stick in a util file somewhere.
(defun eql/package-relaxed (obj1 obj2)
  (cond
    ((eql obj1 obj2)
     ;; It succeeded? Oh good. Return quickly.
     T)
    ((and (symbolp obj1) (symbolp obj2))
     ;; Otherwise do a slower check.
     (string= (symbol-name obj1)
              (symbol-name obj2)))
    (t
     ;; Hrm, sorry. It didn't EQL match,
     nil)))

(defun graph-roots (graph)
  "Find all vertex roots (vertexes with no parents) in the directed graph GRAPH
and return them as a list in no particular order."
  (let ((results))
    (cl-graph:iterate-vertexes
     graph
     (lambda (v)
       (unless (cl-graph:parent-vertexes v)
         (push v results))))
    results))

(defun graph-leaves (graph)
  "Find all vertex leaves (vertexes with no children) in the graph GRAPH and
return them as a list."
  (let ((results))
    (cl-graph:iterate-vertexes
     graph
     (lambda (v)
       (unless (cl-graph:child-vertexes v)
         (push v results))))
    results))

;; A crappy kind of pattern matching.
(defun is-syntax-form-p (syntax-symbol form)
  (cond
    ((consp syntax-symbol)
     (when (consp form)
       (eql/package-relaxed (first syntax-symbol) (first form))))
    ((symbolp syntax-symbol)
     (when (symbolp form)
       (eql/package-relaxed syntax-symbol form)))
    ;; maybe other cases needed?
    (t nil)))

(defgeneric canonicalize-dependency-form (category dependency-form))

(defmethod canonicalize-dependency-form ((category (eql 'component-dependency))
                                         dependency-form)
  (loop :for element :in dependency-form
        :collect
        (cond
          ((consp element)
           element)
          ((is-syntax-form-p '-> element)
           element)
          (t
           `(component-type ,element)))))

(defmethod canonicalize-dependency-form
    ((category (eql 'component-package-search-order)) dependency-form)
  (loop :for element :in dependency-form
        :collect
        (cond
          ((consp element)
           element)
          ((is-syntax-form-p '-> element)
           element)
          (t
           `(potential-package ,element)))))

(defun segment-dependency-form (category form)
  "Lift splices and then segment the dependency FORM into hyperedges.
If the form is null, return values: NIL, :empty
If the form is not null, but contains no hyper edges, return values:
canonical-form and :vertex
If the form is not null, and contains hyper edges, return values: list of
  hyper-edge pairs, :hyperedges"
  (let* ((canonical-form (canonicalize-dependency-form category form))
         (x (split-sequence:split-sequence
             '-> canonical-form :test #'eql/package-relaxed))
         ;; cut into groups of two with rolling window
         (connections
           (loop :for (k j . nil) :in (maplist #'identity x)
                 :when j
                   :collect `(,k ,j))))
    (cond
      ((null canonical-form)
       (values canonical-form :empty))
      ((= (length x) 1)
       ;; no hyperedges
       (values canonical-form :vertex))
      (t ;; hyperedges found
       (values connections :hyperedges)))))

;; Then the code to perform the parsing.
(defun parse-subform (category form)
  (assert (or (is-syntax-form-p '(subdag) form)
              (is-syntax-form-p '(subgraph) form)))
  (destructuring-bind (kind name . dependency-forms) form
    (make-subform
     name
     :kind kind
     :depforms
     (loop :for dep :in dependency-forms
           :collect
           (multiple-value-bind (lifted-dependency-form kind)
               (segment-dependency-form category dep)
             (make-depform :original-form dep
                           :canonical-form lifted-dependency-form
                           :kind kind))))))

(defun get-graph-option (option-name option-form)
  (second (member option-name option-form)))

(defun parse-graph-definition (form)
  (assert (is-syntax-form-p '(define-graph) form))
  (destructuring-bind (name options . subforms) (rest form)
    (let ((category (get-graph-option :category options)))
      (make-graphdef
       name
       :original-form form
       :enabled (get-graph-option :enabled options)
       :category category
       :depends-on (get-graph-option :depends-on options)
       :roots (get-graph-option :roots options)
       :subforms
       (let ((subform-db (make-hash-table)))
         (loop :for i :in subforms
               :for subform = (parse-subform category i)
               :do (setf (gethash (name subform) subform-db) subform))
         subform-db)))))

(defmethod extension-file-type ((extension-type (eql 'graphs)))
  "gph")

(defmethod prepare-extension ((extension-type (eql 'graphs))
                              owner path)
  ;; Collect ALL graph-definitions into the appropriate analyzed-graph objects.
  ;; The graphs aren't fully analyzed yet. This is only the parsing phase.
  (loop :with defs = (collect-extension-forms extension-type path)
        :for def :in defs
        for parsed-def = (parse-graph-definition def)
        :do (multiple-value-bind (analyzed-graph present)
                (gethash (category parsed-def) (analyzed-graphs owner))
              (unless present
                (let ((new-analyzed-graph (make-analyzed-graph
                                           :category (category parsed-def))))
                  (setf (gethash (category parsed-def) (analyzed-graphs owner))
                        new-analyzed-graph
                        analyzed-graph new-analyzed-graph)))
              (when (enabled parsed-def)
                (setf (gethash (name parsed-def) (graphdefs analyzed-graph))
                      parsed-def))))
  ;; TODO: perform the analysis for each graph category type. Note: a category
  ;; may be a consp form in addition to a symbol. A) Ensure if subdag, all are
  ;; subdag. B) Ensure if subgraph, all are subgraph.
  ;; TODO: convert each category to appropriate cl-graph version
  (loop :for angph :being :the :hash-values :in (analyzed-graphs owner)
        :do (analyze-graph angph)))

(defun add-cross-product-edges (clg source-list target-list)
  (map-product
   (lambda (v1 v2)
     (cl-graph:add-edge-between-vertexes clg v1 v2 :edge-type :directed))
   source-list
   target-list))

(defun annotate-splice (v &rest args)
  (if (is-syntax-form-p '(splice) v)
      `(,v ,@(copy-seq args))
      v))

(defun annotate-splices (val-list &rest args)
  (mapcar
   (lambda (v)
     (apply #'annotate-splice v args))
   val-list))

(defun absorb-depforms (clg gdef depforms)
  "Traverse the depforms and add them into the clg as edges while keeping
reference to the initial gdef associated with the depforms. Return three values:
the cl-graph, the roots as elements, the leaves as elements."
  (let ((roots)
        (leaves))
    (loop :for depform :in depforms
          :for kind = (kind depform)
          :for canonical-form = (canonical-form depform)
          ;; check this when condition for validity.
          :when canonical-form
            :do (ecase kind
                  ((:empty) nil)
                  ((:hyperedges)
                   (pushnew (car (first canonical-form)) roots :test #'equalp)
                   (pushnew (cadar (last canonical-form)) leaves :test #'equalp)
                   (loop :for (from to) :in canonical-form
                         :do (add-cross-product-edges
                              clg
                              (annotate-splices from gdef)
                              (annotate-splices to gdef))))
                  ((:vertex)
                   ;; the :vertex for is not only the roots, but also the leaves.
                   (pushnew canonical-form roots :test #'equalp)
                   (pushnew canonical-form leaves :test #'equalp)
                   (loop :for vert :in canonical-form
                         :do (cl-graph:add-vertex
                              clg
                              (annotate-splice vert gdef))))))
    (values clg
            (remove-duplicates (mapcan #'identity roots) :test #'equalp)
            (remove-duplicates (mapcan #'identity leaves) :test #'equalp))))

(defun analyze-graphdef-depends-on (graph)
  "Transmute the :depends-on form in each graphdef object in the GRAPH into real
graphdef references holding real references to the named subforms."
  (loop :for gdef :being :the :hash-values :in (graphdefs graph)
        :for whole-depends-on-form = (depends-on gdef)
        :do
           ;; depends-on used to be list, but now we're converting it into a
           ;; hash table with more semantic data in it.
           (setf (depends-on gdef) (make-hash-table))
           (loop :for (gdef-name subform-names) :in whole-depends-on-form
                 :for gdef-reference = (gethash gdef-name (graphdefs graph))
                 :for analyzed-depends-on = (make-graphdef-depends-on
                                             gdef-name
                                             :graphdef gdef-reference
                                             :original-form (list gdef-name
                                                                  subform-names))
                 :do (assert gdef-reference)
                     ;; Now set up the subforms entry in the analyzed-depends-on
                     ;; object.
                     (if (eq subform-names :all)
                         ;; get all subform names in gdef
                         (maphash
                          (lambda (subform-name subform-instance)
                            (setf (gethash subform-name
                                           (subforms analyzed-depends-on))
                                  subform-instance))
                          (subforms gdef-reference))
                         ;; find the listed subform-names (which if it is NIL,
                         ;; do nothing) in the gdef and assign them.
                         (loop :for subform-name :in subform-names
                               :do (setf (gethash subform-name
                                                  (subforms analyzed-depends-on))
                                         (gethash subform-name
                                                  (subforms gdef-reference)))))
                     ;; store the semantic analysis of the depends-on form,
                     ;; transmuted into its new graphdef-depends-on form, back
                     ;; into the initiating gdef.
                     (setf (gethash (name analyzed-depends-on) (depends-on gdef))
                           analyzed-depends-on))))

(defun lookup-splice (splice-form gdef)
  "Find the subform describing SPLICE-FORM in GDEF, or in any available
depends-on in that GDEF."
  (let ((splice-name (second splice-form)))
    ;; Check of the splice is natively in the current gdef.
    (when-let ((splice (gethash splice-name (subforms gdef))))
      (return-from lookup-splice
        (values splice gdef)))
    ;; If it isn't, then check which depends-on in the current gdef
    (loop :for dep-inst :being :the :hash-values :in (depends-on gdef)
          :do (multiple-value-bind (subform present)
                  (gethash splice-name (subforms dep-inst))
                (when present
                  (return-from lookup-splice
                    (values subform (graphdef dep-inst))))))
    ;; Otheriwse, you're out of luck. Prolly should put an error here.
    (values nil nil)))

(defun analyze-graph (graph)
  ;; TODO: Huge bug/missing feature. This only knows how to analyze/construct
  ;; directed graphs. Maybe that is actually ok and we don't need undirected
  ;; graphs....in which case, dump the subgraph form in the dsl.
  ;; First, resolve all depends-on lines into meaningful structures with real
  ;; references and such. This helps us look up splices.
  (analyze-graphdef-depends-on graph)
  (let ((clg (cl-graph:make-graph
              'cl-graph:graph-container
              ;; I store complex elements and edges.
              :vertex-test #'equalp
              :edge-test #'equalp
              :default-edge-type :directed)))
    ;; We do an iterative algorithm where we continuously refine the graph we're
    ;; making by expanding slices in additional passes until there are no
    ;; splices left.
    ;; First, add the initial depforms from the roots.
    (loop :for gdef :being :the :hash-values :in (graphdefs graph)
          :when (roots gdef)
            :do (loop :for root :in (roots gdef)
                      ;; For the initial seeding, we don't care about the
                      ;; roots/leaves of the initial root subforms.
                      :do (absorb-depforms
                           clg
                           ;; This is the gdef that these depforms came from...
                           gdef
                           ;; ...in which all splices must be looked up in,
                           ;; either directly or through a graphdef-depends-on
                           ;; object.
                           (depforms (gethash root (subforms gdef))))))
    ;; Then, iterate the graph. Each iteration will substitute the current
    ;; splice forms for the actual graphs indicated by those splice names. This
    ;; may introduce more splices the next iteration will get. Stop when there
    ;; are no more splices to substitute.
    ;; Don't convert to dolist, I need to recompute the find-vertexes-if in
    ;; each iteration.
    (loop :for splices = (cl-graph:find-vertexes-if
                          clg
                          (lambda (v)
                            (is-syntax-form-p
                             ;; TODO: This FIRST here implies a structure that
                             ;; not all vertexes may actually have. It forces
                             ;; canonicalize-dependency-form to always make the
                             ;; element a list, like (component-type foo) or
                             ;; (potential-package :bar)
                             '(splice)
                             (first (cl-graph:element v)))))
          :unless splices
            :return nil
          :do (dolist (splice splices)
                (let ((parents (cl-graph:parent-vertexes splice))
                      (children (cl-graph:child-vertexes splice)))
                  (destructuring-bind (splice-form gdef)
                      (cl-graph:element splice)
                    (multiple-value-bind (lookedup-splice lookedup-gdef)
                        (lookup-splice splice-form gdef)
                      ;; Now, absorb the splice into clg, get the roots and
                      ;; leaves, then fixup the edges.
                      (multiple-value-bind (clg splice-roots splice-leaves)
                          (absorb-depforms
                           clg lookedup-gdef (depforms lookedup-splice))
                        ;; delete the original parent edges.
                        (dolist (parent parents)
                          (cl-graph:delete-edge-between-vertexes
                           clg
                           parent
                           splice))
                        ;; add the new edges from the parents to the new-roots.
                        (add-cross-product-edges
                         clg
                         parents
                         (annotate-splices splice-roots lookedup-gdef))
                        ;; delete the original child edges.
                        (dolist (child children)
                          (cl-graph:delete-edge-between-vertexes
                           clg
                           splice
                           child))
                        ;; add the new edges from the new-leaves to the children.
                        (add-cross-product-edges
                         clg
                         (annotate-splices splice-leaves lookedup-gdef)
                         children)
                        ;; Then finally, delete the expanding splice vertex
                        (cl-graph:delete-vertex clg splice)))))))
    ;; finally store it in the analyhzed-graph.
    (setf (graph graph) clg)
    ;; and then generate any annotations we might need.
    (generate-graph-annotation (category graph) graph)))

(defmethod generate-graph-annotation
    ((category (eql 'component-dependency)) graph)
  (let* ((clg (graph graph))
         (contains-cycles-p
           (cl-graph:find-vertex-if
            clg
            (lambda (vert)
              (cl-graph:in-cycle-p clg vert))))
         (annotation (make-graph-annotation
                      (category graph)
                      :unknown-type-id (gensym "UNKNOWN-TYPE-ID-"))))
    ;; compute/store toposort, can only do if no cycles.
    (unless contains-cycles-p
      (let ((tsort (mapcar #'cl-graph:element (cl-graph:topological-sort clg))))
        (setf (toposort graph) tsort)))
    ;; collect all referenced component-types in the graph.
    (cl-graph:iterate-vertexes
     clg
     (lambda (v)
       (let ((elem-v (cl-graph:element v)))
         (when (is-syntax-form-p '(component-type) elem-v)
           (setf (gethash (second elem-v) (referenced-types annotation)) t)))))
    (setf (annotation graph) annotation)))

(defmethod generate-graph-annotation
    ((category (eql 'component-package-search-order)) graph)
  (let* ((clg (graph graph))
         (contains-cycles-p
           (cl-graph:find-vertex-if
            clg
            (lambda (vert)
              (cl-graph:in-cycle-p clg vert))))
         (annotation (make-graph-annotation (category graph)))
         (all-packages (sort (mapcar #'package-name (list-all-packages))
                             #'string<)))
    ;; compute/store toposort, can only do if no cycles.
    (unless contains-cycles-p
      (let ((tsort (mapcar #'cl-graph:element (cl-graph:topological-sort clg))))
        (setf (toposort graph) tsort)))
    ;; Then, for each vertex in the clg, annotate the actual packages that match
    ;; to it.
    (cl-graph:iterate-vertexes
     clg
     (lambda (v)
       (let* ((elem-v (cl-graph:element v))
              (putative-package-name (symbol-name (second elem-v)))
              ;; This regex is mildly wrong and will match a extraneous stuff
              ;; because it isn't escaped properly, stuff like . + ? | whatever
              ;; in the package name will mess things up.
              (putative-package-name-regex
                (concatenate 'string "^" putative-package-name "$")))
         ;; Kind of a terrible Big-O...
         (dolist (pkg-name all-packages)
           (when-let* ((matched-pkg-name (ppcre:scan-to-strings
                                          putative-package-name-regex pkg-name))
                       (found-pkg (find-package matched-pkg-name)))
             (pushnew found-pkg
                      ;; Use the original symbol from the graph.
                      (gethash (second elem-v)
                               (pattern-matched-packages annotation))))))))
    (setf (annotation graph) annotation)))

(defun canonicalize-component-type (component-type core-state)
  "If the COMPONENT-TYPE is reference in the component-dependency graph, then
return it, otherwise return the unknown-type-id symbol."
  (let* ((putative-component-type component-type)
         (component-dependency-graph (gethash 'component-dependency
                                              (analyzed-graphs core-state)))
         (annotation (annotation component-dependency-graph)))
    (assert annotation)
    (unless (gethash putative-component-type (referenced-types annotation))
      ;; Nope, so we use the unknown type
      (setf putative-component-type (unknown-type-id annotation)))
    ;; The canonicalized component-type
    putative-component-type))

;; NOTE: This is unused! It is strictly here to get indentation right in the
;; *.gph files.
(defmacro define-graph (name (&key category enabled) &body body)
  (declare (ignore name category enabled))
  body)
