;; OO support (polymorphism/inheritance/encapsulation) in 30 lines of code

;; Define a vtable scheme - a list of (<name> <body>) pairs
(define (vtable-call vtable method self args)
  (let ((proc (assoc method vtable)))
    (if (eq? #f proc)
        '()
        (apply (cdr proc) (cons self args)))))

;; Each entry in a vtable is a tagged procedure
(define (method name body) (cons name body))

;; Implement method inheritance by merging the new methods with the parent
;; class's vtable
(define (inherit parent methods)
  (if (null? parent)
      methods
      (append methods (parent 'vtable))))

;; Define a procedure for creating new classes
(define (class parent ctor . methods)
  (let ((vtable (inherit parent methods)))
    (lambda (op . class-args)
      (cond ((eq? op 'new)
             (let ((self class-args))
               (lambda (method . method-args)
                 (cond ((eq? method 'can) 
                        (not (eq? #f (assoc (car method-args) vtable))))
                       (else (vtable-call vtable method self method-args))))))
            ((eq? op 'vtable) vtable)
            ((eq? op 'ctor) ctor)
            (else '())))))

;; Define a dispatch for a list of methods
(define (ifc parent . methods)
  (let ((vtable (inherit parent methods)))
    (lambda (op . args)
      (vtable-call vtable op args))))
