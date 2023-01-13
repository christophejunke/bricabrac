(defpackage :bricabrac.sdl2.sprites.transform
  (:use :cl)
  (:export #:pixel-rectangle
           #:with-transform
           #:combine
           #:scale
           #:move
           #:transform
           #:identity-transform
           #:sdl-rect-from-env))

(defpackage :bricabrac.sdl2.sprites.spritesheets
  (:use
   :cl
   :alexandria
   :bricabrac.mixins
   :bricabrac.environments
   :bricabrac.environments.hash-consing
   :bricabrac.sdl2.sprites.transform)
  (:export #:define-spritesheet
           #:find-spritesheet

           #:sprite-description
           #:sprite-descriptions
           #:spritesheet-name
           #:spritesheet-tree
           #:spritesheet-sprites
           #:spritesheet-texture-files

           #:branch
           #:switch-branch

           #:finalize-all-sprite-descriptions

           #:timestamp
           #:texture-id
           #:environment
           #:sprite-description-rectangles
           #:rectangles
           #:sprite-description-texture-id
           #:sprite-description-env
           #:sprite-texture
           #:next-index
           #:spritesheet
           #:flip-p))

(defpackage :bricabrac.sdl2.sprites.textures
  (:use
   :cl
   :alexandria
   :sdl2
   :bricabrac.environments
   :bricabrac.sdl2.sprites.spritesheets)
  (:export #:with-active-spritesheets
           #:call-with-active-spritesheets
           #:*active-textures*
           #:make-sprite
           #:*default-sprite-class*

           #:sprite-description
           #:sdl2-render
           #:sdl2-render-index
           
           #:sdl2-render-sprite-rect
           #:sdl2-render-sprite-index))
