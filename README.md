SDL2-EXTENSIONS

# Event loop

The extensions provides a different event loop than
SDL2:WITH-EVENT-LOOP, with the following goals in mind:

- Be it easy to either handle all events in the same
  function, like SDL2:WITH-EVENT-LOOP, but also to delegate
  event handling to other functions.

- Allow tools to provide live documentation about each kind
  of event, notably which keyword arguments are available
  for each event-type.

- Allow window events, which are further categorized into
  different kinds of event subtypes, to be handled like
  other events.

## Package :SDL2-EXTENSIONS.TESTS

The package exports SINGLE-LOOP and DISPATCH, two examples
of event loops that should provide an overview of how
SDL2-EXTENSIONS macros are expected to be used.

Both functions start an SDL window that receives events and
log them to *STANDARD-OUTPUT*. Under an Emacs/Slime
environment, the output is likely to be redirected to the
*INFERIOR-LISP* buffer.

## DO-EVENTS

The most basic event-loop is DO-EVENTS, which performs the
same polling/waiting code as SDL2:WITH-EVENT-LOOP but does
not dispatch events to event handlers. Unlike the existing
SDL2:WITH-EVENT-LOOP, nested dynamic invocations are not
protected (RECURSIVE parameter).

The macro is a DO-style loop with an implicit NIL block,
which repeatedly binds <event> to an SDL event object, and
optionally bind <event-type> to the event's type (the type
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

## Destructuring events

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

## Window events

Window events are further categorized as different subtypes
of window events. The :WINDOWEVENT is associated with a
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

    (SDL2-EXTENSIONS:WITH-RAW-WINDOW-EVENT (E :DATA1 X :DATA2 Y :WINDOW-ID W)
      (PRINT (LIST (SDL2:GET-WINDOW-TITLE W) X Y)))

## DO-MATCH-EVENTS

The DO-MATCH-EVENTS macro combines the simple DO-EVENTS loop
with the destructuring macros seen above. It performs a CASE
selection over the even-type and each event.

/////
