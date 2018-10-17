(defpackage :bricabrac.environmenst
  (:use :cl)
  (:import-from #:alexandria
                alexandria:if-let
                alexandria:with-gensyms)
  (:export #:attribute-reducer
           #:combine-environments
           #:make-indexer

           #:*cons-node-walkers*
           #:walk-meta-node
           #:walk-meta-node-for-kind
           #:map-property-leaves
           #:do-property-leaves))
