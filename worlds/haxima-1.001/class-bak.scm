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

;; ----------------------------------------------------------------------------

(define mushroom
  (class '()
         '()
         (method 'get (lambda (obj kobj getter)
                        (kern-obj-remove kobj)
                        (kern-obj-add-food getter 10)
                        (kern-obj-destroy kobj)))))

;; Create a new class called "obj-class"
(define obj-class 
  (class '()
         (lambda (sprite loc) (list sprite loc))
         (method 'sprite (lambda (obj) (car obj)))
         (method 'loc (lambda (obj) (cadr obj)))
         (method 'set-loc! (lambda (obj loc) (set-car! (cdr obj) loc)))))

;; Create an instance of obj-class called "stone"
(define stone (obj-class 'new 'S '()))

;; Call some methods on "stone"
(display " sprite: ")(display (stone 'sprite))(newline)
(display "    loc: ")(display (stone 'loc))(newline)
(stone 'set-loc! (list 'narnia 34 56))
(display "new loc: ")(display (stone 'loc))(newline)

;; ----------------------------------------------------------------------------

;; Create a new class called "arms-class" which inherits from "obj-class"
(define arms-class
  (class obj-class
         (lambda (sprite loc damage) (list sprite loc damage))
         (method 'damage (lambda (obj) (caddr obj)))))

;; Create an instance of arms-class called "sword" and call some methods
(define sword (arms-class 'new '| (list 'Rome 12 57) 10))
(display " sprite: ")(display (sword 'sprite))(newline)
(display "    loc: ")(display (sword 'loc))(newline)
(display " damage: ")(display (sword 'damage))(newline)
(stone 'set-loc! (list 'narnia 34 56))
(display "new loc: ")(display (sword 'loc))(newline)

(define (mk-potion-class color)
  (class obj-class
         (obj-class 'ctor)
         (method 'color (lambda (obj) color))))

(define potion ((mk-potion-class "green") 'new '% "wherever"))
