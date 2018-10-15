BRICABRAC.SDL2
==============

# Overview

The extensions provides a different event loop than
SDL2:WITH-EVENT-LOOP, with the following goals in mind:

- Be it easy to either (i) handle all events in the same
  function, like SDL2:WITH-EVENT-LOOP, or (ii) delegate
  event handling to other functions.

- Help Common Lisp environments display context-sensititve
  information about each kind of event, notably which
  keyword arguments are available for each event-type.

- Allow window events, which are further categorized into
  different kinds of event subtypes, to be handled like
  other events.

This is achieved, basically, by separating the existing
SDL2:WITH-EVENT-LOOP macro as different macros, DO-EVENTS
which iterates over events, and EVENT-TYPE-CASE which
dispatches according to an event type. Furthermore, each
event-type is associated with a destructuring macro, making
it easier than SDL2:WITH-EVENT-LOOP to know which arguments
are expected for each kind of event.

The :BRICABRAC.SDL2.TESTS package gives an overview of how
BRICABRAC.SDL2 macros are expected to be used. It exports
two functions, SINGLE-LOOP and DISPATCH, that start an SDL
window and log events to *STANDARD-OUTPUT*.

# DO-EVENTS

DO-EVENTS performs the same polling/waiting code as
SDL2:WITH-EVENT-LOOP but does not dispatch events to event
handlers. Unlike the existing SDL2:WITH-EVENT-LOOP, nested
dynamic invocations are not protected (RECURSIVE parameter).

The macro is a DO-style loop with an implicit NIL block,
which repeatedly binds EVENT to an SDL event object, and
optionally bind EVENT-TYPE to the event's type (the type
must be computed even when anonymous).

    (do-events (<event> [:event-type ( symbol )]
                        [:method ( :poll* | :wait )]
                        [:timeout ( nil* | milliseconds )]
                        [:background ( nil* | t )]
                        [:rebind ( list-of-special-variables )])
      <body>)

BODY is executed in the main SDL2 thread; consequently, the
dynamic bindings in effect inside BODY are different from
the one outside of DO-EVENTS. The :REBIND option accepts a
designator for an unevaluated list of symbols, whose
bindings are captured in the current thread and
re-established inside body. This is done using the auxiliary
macro WITH-CAPTURED-BINDINGS. If :REBIND is T, the standard
output is rebound.

Other options, :METHOD, :TIMEOUT and :BACKGROUND have the
same meaning as in SDL2:WITH-EVENT-LOOP:

  - The :BACKGROUND flag, when non-NIL, indicates that
    current thread does not wait for the event-loop to
    terminate.

  - :METHOD is either :WAIT (blocking) or :POLL
    (non-blocking, the default).

  - When provided, :TIMEOUT must be a positive integer
    representing a timeout in milliseconds. :TIMEOUT is only
    meaningful when used in conjunction with the :WAIT
    method (the blocking wait operation terminates after
    TIMEOUT milliseconds).

When METHOD is :WAIT with a non-NIL :TIMEOUT, or when METHOD
is :POLL, the EVENT-TYPE might be :IDLE.

EVENT-TYPE might be one of the following keywords:

   :CONTROLLERAXISMOTION :CONTROLLERBUTTONDOWN
   :CONTROLLERBUTTONUP :CONTROLLERDEVICEADDED
   :CONTROLLERDEVICEREMAPPED :CONTROLLERDEVICEREMOVED
   :DOLLARGESTURE :DROPFILE :FINGERDOWN :FINGERMOTION
   :FINGERUP :JOYAXISMOTION :JOYBALLMOTION :JOYBUTTONDOWN
   :JOYBUTTONUP :JOYDEVICEADDED :JOYDEVICEREMOVED
   :JOYHATMOTION :KEYDOWN :KEYUP :MOUSEBUTTONDOWN
   :MOUSEBUTTONUP :MOUSEMOTION :MOUSEWHEEL :MULTIGESTURE
   :QUIT :SYSWMEVENT :TEXTEDITING :TEXTINPUT :USEREVENT
   :WINDOWEVENT

Additionally, it can be equal to any symbol registered
through SDL2:REGISTER-USER-EVENT-TYPE. If so, a call to
SDL2::FREE-USER-DATA is done after each iteration or when
unwinding from the loop.

# Destructuring events

All SDL2 events listed above are associated with a
corresponding macro which is used to destructure a given
event into its components.

For example, in definitions.lisp, the following line defines
a macro named WITH-KEY-DOWN-EVENT associated with the
:KEYDOWN event type:

    (define-event-macro with-key-down-event :keydown)

The macro has the following signature:

    (with-key-down-event
      (event &key key timestamp window-id state repeat keysym)
      &body body)

Each keyword argument is used to name a local variable that
represents the event's field. Like in SDL2:WITH-EVENT-LOOP,
only those slots that are referenced in the argument list
are bound.

For example, here is a sample usage of this macro:

    (with-key-down-event (e :keysym k)
      (print (scancode-value k)))

Here below is one level of macroexapnsion:

    (LET ((K (PLUS-C:C-REF E SDL2-FFI:SDL-EVENT :KEY :KEYSYM)))
      (PRINT (SCANCODE-VALUE K)))

Unlike with SDL2:WITH-EVENT-LOOP, each event is documented
separtely by its signature.

# Window events

Window events are further categorized as different subtypes
of window events. The :WINDOWEVENT type is associated with a
macro named WITH-RAW-WINDOW-EVENT.

That kind of event has an :EVENT slot, as well as two
general-purpose slots named :DATA1 and :DATA2.

The DEFINE-WINDOWEVENT-MACRO defines one macro for each
subtype of windowevents, making it possible to directly
destructure an SDL2 event as a specific kind of window
event.

For example: WITH-WINDOW-EVENT-LEAVE can be used as follows:

    (with-window-event-moved (e :x x :y y :window-id w)
      (print (list (get-window-title w) x y)))

One step of macroexpansion introduces WITH-RAW-WINDOW-EVENT,
where :X and :Y where replaced respectively by :DATA1 and
:DATA2:

    (BRICABRAC.SDL2:WITH-RAW-WINDOW-EVENT (E :DATA1 X :DATA2 Y :WINDOW-ID W)
      (PRINT (LIST (SDL2:GET-WINDOW-TITLE W) X Y)))

# EVENT-TYPE-CASE

The DO-EVENTS loop above iterate over events and for each
one, determines its type (a keyword). The EVENT-TYPE-CASE
macro provides a way to dispatch on event types using either
two kinds of clauses:

- a single keyword which identifies an event-type, T, or OTHERWISE
- a form with the same syntax as the destructuring WITH- macros seen
  previously. That allows to reuse the same syntax for both dispatch
  and destructuring, and benefits from auto-completion facilities that
  are available for WITH- macros in the scope of a case.

For example, consider the following expression:

    (event-type-case (event event-type)
      (with-key-up-event (_ :repeat repeat)
        (print repeat))
      (with-window-event-shown (_ :window-id w)
        (print w))
      (with-window-event-maximized (_ :window-id w)
        (print w))
      (with-finger-down-event (_ :pressure p)
        (print p))
      (t (print "other")))

The above is macroexpanded as:

    (CASE EVENT-TYPE
      (:KEYUP
       (WITH-KEY-UP-EVENT (EVENT :REPEAT REPEAT)
         (PRINT REPEAT)))
      (:FINGERDOWN
       (WITH-FINGER-DOWN-EVENT (EVENT :PRESSURE P)
         (PRINT P)))
      (:WINDOWEVENT
       (WITH-RAW-WINDOW-EVENT (EVENT :EVENT #:G775)
         (CASE (WINDOWEVENT #:G775)
           (:WINDOWEVENT-SHOWN
            (WITH-WINDOW-EVENT-SHOWN (EVENT :WINDOW-ID W)
              (PRINT W)))
           (:WINDOWEVENT-MAXIMIZED
            (WITH-WINDOW-EVENT-MAXIMIZED (EVENT :WINDOW-ID W)
              (PRINT W))))))
      (T (PRINT "other")))

Notice that:

- Each WITH- form implicitly knows which event-type keyword to match
  against.

- Window events are grouped as a single clause, and further dispatched
  according to the WINDOWEVENT tag in that clause.

- The EVENT parameter of each WITH- form is redundant in the context
  of EVENT-TYPE-CASE; it can thus be omitted (either NIL or a symbol
  which is STRING= to "_"). If, however, you put a symbol here, it is
  locally bound to the current event (see DO-MATCH-EVENTS):

        (event-type-case (event event-type)
          (with-key-down-event (e)
            (print e)))

  After macroexpansion:

        (CASE EVENT-TYPE
          (:KEYDOWN
           (WITH-KEY-DOWN-EVENT (EVENT)
             (LET ((E EVENT))
               (PRINT E)))))

# DO-MATCH-EVENTS

DO-MATCH-EVENTS is a simple macro that combines DO-EVENTS
with EVENT-TYPE-CASE, in such a way that neither EVENT nor
EVENT-TYPE needs be explicitly named. For example, consider:

     (do-match-events (:method :wait)
       (:quit (return))
       (:idle (redisplay))
       (with-window-event-moved (e :x x :y y)
         (print (list :moved e x y))))

The above is equivalent to the follwing form:

    (do-events (event
                :event-type event-type
                :method :wait)
      (event-type-case (event event-type)
        (:quit (return))
        (:idle (redisplay))
        (with-window-event-moved (e :x x :y y)
          (print (list :moved e x y)))))

The local binding E in WITH-WINDOW-EVENT-MOVED refers to the
implicit event being handled.
