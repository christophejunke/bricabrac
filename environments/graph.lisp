(in-package :bricabrac.environments)

(do-property-leaves
    ((path)
     `((:extending ,bricabrac.shell::*terminal-options-tree*) ()
       ((:path :xterm :test) ()
        a
        b)))
  (print path))
