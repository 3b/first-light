(in-package :fl.core)

(simple-logger:define-message :info :engine.start
  "Started ~a.")

(simple-logger:define-message :info :engine.quit
  "Stopped ~a.")

(simple-logger:define-message :trace :extension.load
  "Loaded extension (~(~a~)): ~a.")

(simple-logger:define-message :debug :display.init
  "Display ~dx~d @ ~dHz created.")

(simple-logger:define-message :debug :display.stop
  "Display ~dx~d @ ~dHz destroyed.")

(simple-logger:define-message :debug :input.key.down
  "Key pressed: ~a.")

(simple-logger:define-message :debug :input.key.up
  "Key released: ~a.")

(simple-logger:define-message :debug :input.scroll.up
  "Mouse wheel scrolled up.")

(simple-logger:define-message :debug :input.scroll.down
  "Mouse wheel scrolled down.")

(simple-logger:define-message :trace :flow.enter
  "Entering flow: (~a ~a ~a).")

(simple-logger:define-message :trace :flow.state.process
  "Processing flow-state: ~a, exiting: ~a.")

(simple-logger:define-message :trace :flow.call.selector
  "Calling selector function.")

(simple-logger:define-message :trace :flow.call.action.hash
  "Calling action function (hash).")

(simple-logger:define-message :trace :flow.call.action.instance
  "Calling action function (instance).")

(simple-logger:define-message :trace :flow.call.transition
  "Calling transition function.")

(simple-logger:define-message :trace :flow.exit
  "Exiting flow: (~a ~a ~a).")

(simple-logger:define-message :debug :shader.function.compiled
  "Compiled shader function: ~a")

(simple-logger:define-message :trace :component.mesh.cache.used
  "Used a cached copy of mesh: ~a")

(simple-logger:define-message :trace :component.mesh.cache.created
  "Creating a new cached mesh for: ~a")
