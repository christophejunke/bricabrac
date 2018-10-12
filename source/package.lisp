(defpackage :sdl2-extensions
  (:use :cl :sdl2 :autowrap :alexandria)
  (:export

   #:with-captured-bindings

   #:do-match-events
   #:do-events
   #:event-type-case

   #:with-controller-axis-motion-event
   #:with-controller-button-down-event
   #:with-controller-button-up-event
   #:with-controller-device-added-event
   #:with-controller-device-remapped-event
   #:with-controller-device-removed-event
   #:with-dollar-gesture-event
   #:with-drop-file-event
   #:with-finger-down-event
   #:with-finger-motion-event
   #:with-finger-up-event
   #:with-joy-button-down-event
   #:with-joy-button-up-event
   #:with-joy-device-added-event
   #:with-joy-device-removed-event
   #:with-joy-hat-motion-event
   #:with-joyaxis-motion-event
   #:with-joyball-motion-event
   #:with-key-down-event
   #:with-key-up-event
   #:with-lisp-message-event
   #:with-mouse-button-down-event
   #:with-mouse-button-up-event
   #:with-mouse-motion-event
   #:with-mouse-wheel-event
   #:with-multi-gesture-event
   #:with-sys-wm-event
   #:with-text-editing-event
   #:with-text-input-event
   #:with-user-event

   #:with-raw-window-event
   #:windowevent

   #:with-window-event-shown
   #:with-window-event-hidden
   #:with-window-event-exposed
   #:with-window-event-moved
   #:with-window-event-resized
   #:with-window-event-size-changed
   #:with-window-event-minimized
   #:with-window-event-maximized
   #:with-window-event-restored
   #:with-window-event-enter
   #:with-window-event-leave
   #:with-window-event-focus-gained
   #:with-window-event-focus-lost
   #:with-window-event-close))
