(defpackage #:bricabrac.property-trees
  (:use :cl #:bricabrac.local-keywords
        #:bricabrac.fold-environments
        #:bricabrac.fold-environments.environment
        )
  (:import-from #:alexandria
                #:ensure-list
                )
  (:export #:defnode
           #:node
           #:node-environment
           #:find-node
           #:within-node

           #:resolve))
