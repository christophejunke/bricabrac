(in-package :bricabrac.sdl2.event-loop)

(defun windowevent (code)
  "Return the keyword corresponding to a windowevent code"
  (aref #(:WINDOWEVENT-NONE
          :WINDOWEVENT-SHOWN
          :WINDOWEVENT-HIDDEN
          :WINDOWEVENT-EXPOSED
          :WINDOWEVENT-MOVED
          :WINDOWEVENT-RESIZED
          :WINDOWEVENT-SIZE-CHANGED
          :WINDOWEVENT-MINIMIZED
          :WINDOWEVENT-MAXIMIZED
          :WINDOWEVENT-RESTORED
          :WINDOWEVENT-ENTER
          :WINDOWEVENT-LEAVE
          :WINDOWEVENT-FOCUS-GAINED
          :WINDOWEVENT-FOCUS-LOST
          :WINDOWEVENT-CLOSE)
        code))

(defun windowevent-keywords (event)
  "List of known keyword arguments for a windowevent symbol.

  The function return a list of KEY or (KEY . ORIGINAL-KEY) elements, where KEY
  and ORIGINAL-KEY are keywords. Each KEY represents a keyword parameter that
  can be extracted from given window event, and ORIGINAL-KEY is the name of the
  field in a WINDOWEVENT event that stores KEY. Typically, ORIGINAL-KEY is one
  of the general-purpose slot :DATA1 or :DATA2, and KEY is the user-friedly
  name of the value stored in DATA1 or DATA2 in an event struct."
  (list* :timestamp :window-id
         (case event
           (:windowevent-resized '((:width . :data1) (:height . :data2)))
           (:windowevent-size-changed '((:width . :data1) (:height . :data2)))
           (:windowevent-moved '((:x . :data1) (:y . :data2))))))

(defmacro do-events
    ((event &key
              (event-type (gensym))
              (method :poll)
              timeout
              background
              rebind)
     &body body)
  "Indefintely loop over SDL events in the main SDL2 thread.

METHOD and TIMEOUT are passed to the underlying call to SDL2:NEXT-EVENT.
BACKGROUND is passed unmodified to SDL2:IN-MAIN-THREAD.

During each iteration of the event loop, EVENT is bound with dynamic extent
to the current event being processed.

If EVENT-TYPE is also provided, it must be a symbol used to name a local
variable, which holds the type of EVENT according to SDL2:GET-EVENT-TYPE; the
type can also be :IDLE in case (i) no event is actually being processed
and (ii) the loop is not waiting for events. That case can happen when METHOD
is :POLL or when METHOD is :WAIT and a :TIMEOUT occurs.

BODY is executed in the main SDL2 thread; consequently, the dynamic bindings in
effect inside BODY are different from the one outside of DO-EVENTS. The :REBIND
option accepts a designator for a list of symbols, whose bindings are captured
in the current thread and re-established inside BODY. This is done using the
auxiliary macro WITH-CAPTURED-BINDINGS.

DO-EVENTS adds an implicit NIL block."
  (check-type method (member :poll :wait))
  (with-gensyms (rc binder event-id)
    (once-only (method timeout)
      `(with-captured-bindings (,binder
                                ,@(if (eq rebind t)
                                      '(*standard-output*)
                                      (alexandria:ensure-list rebind)))
         (in-main-thread (:background ,background)
           (with-sdl-event (,event)
             (,binder
              (loop
                (let* ((,rc (next-event ,event ,method ,timeout))
                       (,event-type
                         (if (zerop ,rc)
                             :idle
                             (get-event-type ,event)))
                       (,event-id (and (sdl2::user-event-type-p ,event-type)
                                       (,event :user :code))))
                  (declare (type (signed-byte 32) ,rc))
                  (case ,event-type
                    (:lisp-message (sdl2::get-and-handle-messages))
                    (t
                     (unwind-protect (progn ,@body)
                       (when ,event-id
                         (sdl2::free-user-data ,event-id))))))))))))))

(defun accessor-keywords (event-type)
  "Map EVENT-TYPE to an ACCESSOR keyword and all event-specific parameters.

Retrieve the ACCESSOR symbol as established by SDL2::*EVENT-TYPE-TO-ACCESSOR*.
Based on that accessor, retrieves the struct type associated with event type,
and get all the available fields for that type, except useless ones such as
padding fields and the type tag"
  (flet ((fields (type) (foreign-record-fields (unaliased-type type))))
    (let ((accessor (cdr (assoc event-type sdl2::*event-type-to-accessor*))))
      (values accessor
              (set-difference
               (mapcar #'foreign-type-name
                       (fields
                        (foreign-type
                         (find accessor
                               (fields (find-type 'sdl2-ffi:sdl-event))
                               :key #'foreign-type-name))))
               '(:type :padding4 :padding3 :padding2 :padding1))))))

(defmacro define-event-macro (name event-type)
  "Define a macro NAME which destructures an event of type EVENT-TYPE.

   Also, register NAME as having a dispatch expansion for DO-MATCH-EVENTS.

   For example, (DEFINE-EVENT-MACRO WITH-KEY-DOWN-EVENT :KEYDOWN) defines the
   WITH-KEY-DOWN-EVENT, which can be used as follows:

   (WITH-KEY-DOWN-EVENT (EVENT :KEYSYM KEYSYM)
     ... keysym is bound to the KEYSYM field in EVENT,
     ... which must be a C struct SDLEvent type of type SDL_KEYDOWN)

   When used as a clause in DO-MATCH-EVENTS, the above form needs no EVENT
   symbol (it can be replaed by an anonymous variable named _ or NIL). See
   DO-MATCH-EVENTS."
  (check-type event-type keyword)
  (multiple-value-bind (accessor keywords) (accessor-keywords event-type)
    (assert accessor)
    (let ((keys (mapcar #'copy-symbol keywords)))
      (destructuring-bind #1=(event body rest) (mapcar #'copy-symbol '#1#)
        `(prog1 ',name
           (defmacro ,name ((,event &rest ,rest &key ,@keys) &body ,body)
             (declare (ignore ,@keys))
             (second (sdl2::expand-handler ,event
                                           ,event-type
                                           ,rest
                                           ,body)))
           (setf (get ',name 'event-type) ,event-type))))))

(defmacro do-match-events ((&key
                              (method :poll)
                              timeout
                              background
                              rebind)
                           &body clauses)
  (with-gensyms (event event-type)
    `(do-events (,event :event-type ,event-type
                        :method ,method
                        :background ,background
                        :timeout ,timeout
                        :rebind ,rebind)
       (event-type-case (,event ,event-type)
         ,@clauses))))

(deftype windowevent ()
  '(member
    :WINDOWEVENT-NONE
    :WINDOWEVENT-SHOWN
    :WINDOWEVENT-HIDDEN
    :WINDOWEVENT-EXPOSED
    :WINDOWEVENT-MOVED
    :WINDOWEVENT-RESIZED
    :WINDOWEVENT-SIZE-CHANGED
    :WINDOWEVENT-MINIMIZED
    :WINDOWEVENT-MAXIMIZED
    :WINDOWEVENT-RESTORED
    :WINDOWEVENT-ENTER
    :WINDOWEVENT-LEAVE
    :WINDOWEVENT-FOCUS-GAINED
    :WINDOWEVENT-FOCUS-LOST
    :WINDOWEVENT-CLOSE))

(defmacro define-windowevent-macro (name windowevent)
  "Define a macro NAME which destructures a window event of type WINDOWEVENT"
  (check-type windowevent windowevent)
  (loop
    for item in (windowevent-keywords windowevent)
    for bind = (consp item)
    for keyword-variable = (copy-symbol (if bind (car item) item))
    when bind collect item into subst
      collect keyword-variable into keywords
    finally
       (return
         (destructuring-bind #1=(event rest body from to) (mapcar #'copy-symbol '#1#)
           `(prog1 ',name
              (defmacro ,name ((,event &rest ,rest &key ,@keywords) &body ,body)
                (declare (ignore ,@keywords))
                ,@(when subst
                    `((loop for (,from . ,to) in ',subst
                            do (setf ,rest (subst ,to ,from ,rest)))))
                `(with-raw-window-event (,,event ,@,rest)
                   ,@,body))
              (setf (get ',name 'event-type) (cons :windowevent ,windowevent)))))))

(defmacro event-type-case ((event event-type) &body clauses)
  (let ((windowevents nil)
        (catch-all nil))
    (flet
        ((clause (form)
           (assert (consp form) () "A clause should be a non-empty list")
           (typecase (car form)
             ((member T OTHERWISE)
              (setf catch-all form))
             (keyword form)
             (symbol
              (when catch-all
                (error "Dead code due to default case (~a)" catch-all))
              (destructuring-bind (with (ev . args) . rest) form
                (flet ((case-form (test)
                         `(,test
                           (,with (,event ,@args)
                                  ,@(if (and ev (not (string= ev "_")))
                                        `((let ((,ev ,event)) ,@rest))
                                        rest)))))
                  (let ((type (get with 'event-type)))
                    (assert type () "Unknown event type ~a" with)
                    (etypecase type
                      (cons
                       (prog1 nil
                         (destructuring-bind (name . type-arg) type
                           (ecase name
                             (:windowevent
                              (push
                               (case-form type-arg)
                               windowevents))))))
                      (symbol
                       (case-form type))))))))))
      (let ((clauses (remove nil (mapcar #'clause clauses))))
        (when (and windowevents (find :windowevent clauses :key #'car))
          (error "Ambiguous :windowevent clauses"))
        (when catch-all
          (setf clauses (butlast clauses)))
        `(case ,event-type
           ,@clauses
           ,@(and windowevents
              (let ((code (gensym)))
                `((:windowevent
                   (with-raw-window-event (,event :event ,code)
                     (case (windowevent ,code)
                       ,@(reverse windowevents)))))))
           ,@(and catch-all (list catch-all)))))))
