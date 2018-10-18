(in-package :bricabrac.sdl2.event-loop)

(define-event-macro with-controller-axis-motion-event :controlleraxismotion)
(define-event-macro with-controller-button-down-event :controllerbuttondown)
(define-event-macro with-controller-button-up-event :controllerbuttonup)
(define-event-macro with-controller-device-added-event :controllerdeviceadded)
(define-event-macro with-controller-device-remapped-event :controllerdeviceremapped)
(define-event-macro with-controller-device-removed-event :controllerdeviceremoved)
(define-event-macro with-dollar-gesture-event :dollargesture)
(define-event-macro with-drop-file-event :dropfile)
(define-event-macro with-finger-motion-event :fingermotion)
(define-event-macro with-finger-down-event :fingerdown)
(define-event-macro with-finger-up-event :fingerup)
(define-event-macro with-joyaxis-motion-event :joyaxismotion)
(define-event-macro with-joyball-motion-event :joyballmotion)
(define-event-macro with-joy-button-down-event :joybuttondown)
(define-event-macro with-joy-button-up-event :joybuttonup)
(define-event-macro with-joy-device-added-event :joydeviceadded)
(define-event-macro with-joy-device-removed-event :joydeviceremoved)
(define-event-macro with-joy-hat-motion-event :joyhatmotion)
(define-event-macro with-key-down-event :keydown)
(define-event-macro with-key-up-event :keyup)
(define-event-macro with-mouse-button-down-event :mousebuttondown)
(define-event-macro with-mouse-button-up-event :mousebuttonup)
(define-event-macro with-mouse-motion-event :mousemotion)
(define-event-macro with-mouse-wheel-event :mousewheel)
(define-event-macro with-multi-gesture-event :multigesture)
(define-event-macro with-sys-wm-event :syswmevent)
(define-event-macro with-text-editing-event :textediting)
(define-event-macro with-text-input-event :textinput)
(define-event-macro with-user-event :userevent)

(define-event-macro with-raw-window-event :windowevent)

(defmacro define-windowevents-macro ()
  (flet ((make-form (keyword)
           `(define-windowevent-macro
                ,(symbolicate 'with-window-event-
                              (subseq (string keyword)
                                      #.(length (string :windowevent-))))
                ,keyword)))
    `(progn
       ,@(map 'list
              #'make-form
              (bricabrac.sdl2.event-loop::sdl2-ffi-windowevents)))))

(define-windowevents-macro)
