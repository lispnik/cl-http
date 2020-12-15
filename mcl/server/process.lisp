In response to one of John's notes on CPI. The current CLIM-SYS
package provided for UNIX/PC ports of CL-HTTP has
an exported interface for processes, locks and resources.
[If CLIM is already present in the image, it tries to reuse
the vendor provided CLIM-SYS package and extend it minimally].

Below is the current "documented" interface provided by CLIM-SYS
under CL-HTTP (UNIX/PC) in the next release. The "-LOOP" names are
only added to support implementations without built-in multithreading
substrate (MINIPROC).

I've tried to annotate extensions and differences with the current
HTTP:PROCESS stuff that I don't think are justified below (search
"NOTE:").
.
Everthing else that is usually true about CLIM-SYS (in CLIM) should be true
below except for extensions. A goal is to identify the most common
extended version of CLIM-SYS (in future: Chris Vincent's CLIM PRESENTATION
system as well) and make it available everywhere CL-HTTP needs to run.

Olivier
-------------------------Common Process Interface-------------------
;;; Refer to your favorite CLIM manuals for more detail on these

*MULTIPROCESSING-P* 

ALL-PROCESSES ()

ALLOCATE-RESOURCE (RESOURCE &REST ARGS)
Get a copy of the NAMEd resource, given the args (for the initializer, 
   matcher and constructor). Name is evaluated.

ATOMIC-DECF (REFERENCE &OPTIONAL DELTA)

ATOMIC-INCF (REFERENCE &OPTIONAL DELTA)

CLEAR-RESOURCE (RESOURCE)

CURRENT-PROCESS ()

DEALLOCATE-RESOURCE (RESOURCE OBJECT)
Return object to pool name. It's a bad idea to free an object to the wrong
pool. Name is evaluated.

DEFGENERIC* ;; No current definition unless CLIM present

DEFMETHOD*  ;; No current definition unless CLIM present

DEFRESOURCE (NAME PARAMETERS &KEY CONSTRUCTOR INITIALIZER DEINITIALIZER
             INITIAL-COPIES MATCHER)
Name, an unevaluated symbol, will become the name of the new resource.
   PARAMETERS, a lambda list, are used to initialize (create) instances of the 
   resource, and come from allocate-resource (so it can be used to supply, 
   e.g. default arguments)

   CONSTRUCTOR is a function to call to make a new object when the resource
   is empty, and accepts the PARAMETERS as arguments. Note this is required.

   Options are:

        :INITIAL-COPIES (used to set up the pool to begin with).

        :INITIALIZER (called on a newly allocated object, and the other 
        parameters). Note the constructor isn't called on objects that
        are already in the pool.

        :DEINITIALIZER (called on a newly freed object) Useful to allow gc
        of objects the resource refers to.

        :MATCHER Args are like initializer, but is expected to be a predicate
        that succeeds if the unallocated pool object is appropriate for the 
        call. The default one assumes only pool objects created with the same
        parameters are appropriate.
        This is useful if you are going to put different size objects in the
        pool, but don't want to have to create a new object when a (bigger)
        one already exists.

DESTROY-PROCESS (PROCESS)

DISABLE-PROCESS (PROCESS)

ENABLE-PROCESS (PROCESS)

MAKE-LOCK (&OPTIONAL LOCK-NAME)

;;; CLIM-SYS Extension: lambda list
;;;
MAKE-PROCESS (FUNCTION &REST INITIALIZATION-ARGS &KEY NAME &ALLOW-OTHER-KEYS)
;;; Extended to use NIL function argument for process resourcing
;;; Extended to pass other keys to the substrate for things like
;;; quantum and priority (implementation dependent still).
;;; IMPORTANT NOTE: FUNCTION TAKES NO ARGUMENT AS IN CLIM-SYS
;;;
When using this macro it is assumed the user provided FUNCTION
is written in a portable way for all platforms including non
MP implementations based on MINIPROC. For example FUNCTION can
contain a PROCESS-WAIT form or a PROCESS-LOOP form but should
NOT include tight and time consumming LOOP clauses unless it
is implementing a MINP:SCHEDULER take-over and comprises sufficient
calls to PROCESS-YIELD. This standard CLIM-SYS:MAKE-PROCESS standard
definition is extended to support a NIL function argument this
is used to create a minimum empty process that can be reused
via an interrupt or using the WITH-CURRENT-PROCESS form. This may
be used to provide process resources -- such processes should not be
destroyed to insure they can later be interrupted and restarted.

;;; CLIM-SYS Extension for MINIPROC implementation on PC and AKCL
;;;
MAKE-PROCESS-LOOP (&REST LOOPKEYS &KEY NAME CONTEXT WITH DECLARE INITIALLY
                   WHILE UNTIL DO FINALLY)
MAKE-PROCESS-LOOP (&rest LOOPKEYS) is a short-hand macro equivalent to:
`(MAKE-PROCESS #'(LAMBDA () (PROCESS-LOOP ,@LOOPKEYS)) :NAME ,NAME).
It is the original portable declaration for process iterations.
Make-Process-Loop is intended to provide a uniform declaration
for iterative processes that should behave identically in a multiprocess
system (MP) or in a mini process system (MINPROC).

MAKE-RECURSIVE-LOCK (&OPTIONAL LOCK-NAME)
;;; When CLIM is not available this implementation provides recursive
;;; ordered locks derived from Sonya Keene's book.

MAP-RESOURCE (FUNCTION RESOURCE &REST ARGS)

;;; CLIM-SYS Extension for CL-HTTP
;;;
PROCESS-ACTIVE-P (PROCESS)

PROCESS-INTERRUPT (PROC LAMBDA)

;;; CLIM-SYS Extension for MINIPROC
;;;
PROCESS-ITERATE (PROCESS &KEY NAME CONTEXT INITIALLY WHILE DO FINALLY PRESET)

;;; CLIM-SYS Extension for MINIPROC
;;;
PROCESS-LOOP (&KEY NAME CONTEXT WITH DECLARE INITIALLY WHILE UNTIL DO FINALLY
              PRESET)
A mini loop macro to declare process iterations in a way that is portable
to non MP implementations based on MINIPROC. See examples in SYSTEST file.

PROCESS-NAME (PROCESS)

;;; CLIM-SYS Extension for CL-HTTP
;;;
PROCESS-PRESET (PROCESS INITIAL-FUNCTION &REST INITIAL-ARGS)

;;; CLIM-SYS Extension for MINIPROC
;;;
PROCESS-SLEEP (TIMEOUT)

PROCESS-STATE (PROCESS)

;;; NOTE: Predicate takes NO argument in CLIM-SYS (see CL-HTTP source code)
;;;
PROCESS-WAIT (WAIT-REASON PREDICATE)

;;; NOTE: Predicate takes NO argument in CLIM-SYS
;;;
PROCESS-WAIT-WITH-TIMEOUT (WAIT-REASON TIMEOUT PREDICATE)

PROCESS-WHOSTATE (PROCESS)

PROCESS-YIELD ()

PROCESSP (PROCESS)

RESTART-PROCESS (PROCESS)

SETF*   ;; No current definition unless CLIM present

;;; CLIM-SYS Extension for MINIPROC
;;;
UNWIND-PROCESS (FORM &REST UNWINDS)
Similar to unwind-protect but extended for MINIPROC to provide
an unwind completion within the current mini process if available.

USING-RESOURCE ((VARIABLE RESOURCE &REST ARGS) &BODY BODY)
VARIABLE is bound to an object from RESOURCE which is initialized with ARGS.

WITH-LOCK-HELD ((LOCK &OPTIONAL STATE) &BODY BODY)

WITH-RECURSIVE-LOCK-HELD ((LOCK &OPTIONAL STATE) &REST BODY)
;;; When CLIM is not available this implementation provides recursive
;;; ordered locks derived from Sonya Keene's book.

WITHOUT-SCHEDULING (&REST BODY)

-------------------End-of-Common Process Interface-------------------
