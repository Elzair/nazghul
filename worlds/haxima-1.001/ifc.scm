;; ----------------------------------------------------------------------------
;; ifc.scm - kernel-to-object interface
;; ----------------------------------------------------------------------------

;; Define a vtable scheme - a list of (<name> <body>) pairs
(define (vtable-call vtable method args)
  ;(display "vtable-call")(newline)
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

;; Define a dispatch for a list of methods
(define (ifc parent . methods)
  (let ((vtable (inherit parent methods)))
    (lambda (op . args)
      ;;(display op)(newline)
      (cond ((eq? op 'vtable) vtable)
            ((eq? op 'can) 
             (begin
               (not (eq? #f (assoc (car args) vtable)))))
            (else (vtable-call vtable op args))))))

;; Map standard interface calls to a bitmap for fast lookup in the kernel
(define (ifc-cap ifc)
  (define (cap ifc calls)
    (if (null? calls) 0
        (+ (* 2 (cap ifc (cdr calls)))
           (if (ifc 'can (car calls)) 1 0))))
  (if (null? ifc) 0
      (cap ifc (list 'get 'use 'exec 'open 'handle 'step 'attack 'mix 
                     'enter 'cast 'bump 'hit-loc))))

;; The gob internal api:
(define (gob-mk kobj members) (list kobj members))
(define (gob-kobj gob) (car gob))
(define (gob-ifc gob) (kobj-ifc (gob-kobj gob)))
(define (gob-data gob) (cadr gob))

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
  (kern-mk-obj-type tag name sprite layer (ifc-cap ifc) ifc))
