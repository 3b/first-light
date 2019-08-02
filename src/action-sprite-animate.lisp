(in-package #:virality.actions)

(defmethod v:on-action-update (action (type (eql 'sprite-animate)))
  (a:when-let* ((actor (v:actor (v:renderer (v:manager action))))
                (sprite (v:component-by-type actor 'v/comp:sprite)))
    (v/comp:update-sprite-index sprite (v:action-step action))))

(defmethod v:on-action-finish (action (type (eql 'sprite-animate)))
  (when (v:repeat-p action)
    (v:replace-action action 'sprite-animate)))
