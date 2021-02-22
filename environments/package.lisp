(defpackage :bricabrac.environments
  (:use :cl)
  (:import-from #:alexandria
		alexandria:ensure-list
		alexandria:ensure-gethash
		alexandria:hash-table-values
                alexandria:if-let
                alexandria:once-only
                alexandria:make-keyword
                alexandria:with-gensyms)
  (:export #:attribute-reducer
           #:ref
           #:resolve-value
           #:combine-environments
           #:make-indexer
           #:make-environment
           #:*default-environment-type*
           #:environment-bind
           #:*cons-node-walkers*
           #:walk-meta-node
           #:walk-meta-node-for-kind
           #:map-property-leaves
           #:do-property-leaves
           #:*reducer*))
