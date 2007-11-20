;; ----------------------------------------------------------------------------
;; ifc.scm - kernel-to-object interface
;; ----------------------------------------------------------------------------

;; Define a vtable scheme - a list of (<name> <body>) pairs
(define (vtable-call vtable method args)
  ;;(println method ":" args)
  (let ((proc (assoc method vtable)))
    (if (eq? #f proc)
        ;; If we fail to find the desired method then see if there's a default
        ;; method
        (let ((proc (assoc 'default vtable)))
          (if (eq? #f proc)
              '()
              (apply (cdr proc) args)))
        (apply (cdr proc) args))))

;; Each entry in a vtable is a tagged procedure
(define (method name body) (cons name body))

;; Implement method inheritance by merging the new methods with the parent
;; class's vtable
(define (inherit parent methods)
  (if (null? parent)
      methods
      (append methods (parent 'vtable))))

;; ----------------------------------------------------------------------------
;; The purpose of this list is to prevent the scheme gc from harvesting the
;; scroll interfaces which are created on-the-fly in mk-scroll. Without this
;; I'd have to explicitly assign a variable to each ifc, which is needlessly
;; verbose.
;;
;; The scheme interpreter reclaims any cells not referred to by another cell,
;; recursively. It can't detect cells that are only referred to by kernel data
;; structures, and will reclaim them. To prevent this, I add all ifcs to this
;; list. The list is referred to by the scheme top-level environment, and it
;; refers to all ifcs that are added to it.
;; ----------------------------------------------------------------------------
(define ifc-list '())

(define (ifc-protect ifc)
  (set! ifc-list (cons ifc ifc-list))
  ifc)

;; Define a dispatch for a list of methods
(define (ifc parent . methods)
  (let ((vtable (inherit parent methods)))
    (ifc-protect 
     (lambda (op . args)
       ;;(display op)(newline)
       (cond ((eq? op 'vtable) vtable)
             ((eq? op 'can) 
              (begin
                (not (eq? #f (assoc (car args) vtable)))))
             (else (vtable-call vtable op args)))))))

;; Map standard interface calls to a bitmap for fast lookup in the kernel
(define (ifc-cap ifc)
  (define (cap ifc calls)
    (if (null? calls) 0
        (+ (* 2 (cap ifc (cdr calls)))
           (if (ifc 'can (car calls)) 1 0))))
  (if (null? ifc) 0
      (cap ifc (list 'get 'use 'exec 'open 'handle 'step 'attack 'mix 
                     'enter 'cast 'bump 'hit-loc 'buy 'search 'sense 'xamine 'describe 'on-attack
                     'describe))))

;; The gob internal api:
(define (gob-mk kobj members) (list kobj members))
(define (gob-kobj gob) (car gob))
(define (gob-ifc gob) (kobj-ifc (gob-kobj gob)))
(define (gob-data gob) (if (null? gob) nil (cadr gob)))

;; Bind a kernel object to a gob and initialize it
(define (bind kobj gob-data)
  (kern-obj-set-gob kobj (gob-mk kobj gob-data))
  (let ((ifc (kobj-ifc kobj)))
    (cond ((null? ifc) '())
          (else (ifc 'init kobj))))
  kobj)

(define (bind-astral-body kobj gob-data)
  (kern-astral-body-set-gob kobj (gob-mk kobj gob-data)))

;; Make a wrapper for kern-mk-obj-type which inserts the ifc cap info
(define (mk-obj-type tag name sprite layer ifc)
  (kern-mk-obj-type tag name sprite layer (ifc-cap ifc) ifc mmode-smallobj))

;; Same as mk-obj-type but flag this type as critical for a quest
(define (mk-quest-obj-type tag name sprite layer ifc)
  (kern-type-set-quest-item-flag (mk-obj-type tag name sprite layer ifc)
                                 #t))

;; ----------------------------------------------------------------------------
;; send-signal - send a signal to an object. ksender is nil or a pointer to a
;; kernel object, tag is the tag of the target object (eg 'door-1) and sig is
;; the symbol of the signal to send (eg 'lock). Examples:
;;   (send-signal nil 'door-1 'lock)
;;   (send-signal kplayer 'handle-1b 'on)
;; ----------------------------------------------------------------------------
(define (send-signal ksender kobj sig)
  ((kobj-ifc kobj) sig kobj ksender))

;;----------------------------------------------------------------------------
;; Yet Another Way to Send a Signal. This one works with arbitrary-length
;; parameters, so it is more generally applicable to a variety of signals than
;; previous efforts. It automatically repackages the kobj itself as the first
;; parameter, so make sure the designated signal handlers expect this.
;;----------------------------------------------------------------------------
(define (ifccall kobj sig . parms)
  (apply (kobj-ifc kobj) (cons sig (cons kobj parms))))

;;----------------------------------------------------------------------------
;; This returns #t iff kobj has an interface and it has a handler for the sig.
;;----------------------------------------------------------------------------
(define (handles? kobj sig)
  (let ((ifc (kobj-ifc kobj)))
    (and (not (null? ifc))
         (ifc 'can sig))))