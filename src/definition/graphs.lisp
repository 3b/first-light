(in-package #:virality.engine)

(define-graph :core (:category component-dependency)
  (subdag all-unknown-types ((unknown-types)))
  (subdag actions (v/comp:action -> v/comp:action-list))
  (subdag drawables (v/comp:static-mesh -> v/comp:sprite -> v/comp:render))
  (subdag core-types
          (v/comp:transform
           -> (splice actions)
           -> (splice drawables))))

(define-graph :core-component-order (:category component-package-order)
  (subdag core-packages (:v/comp)))
