
;; init.scm -- contains lots of common scheme utilities
(load "init.scm")

;; Override the default error hook to warn the loader when the interpreter
;; encounters any errors during evaluation.
(define (my-error-hook . x)
  (kern-interp-error x)
  (apply throw x))
(define *error-hook* my-error-hook)

;; kern-load -- loads a file but also tells the kernel to make a note of it so
;; that saved sessions will know to use the file, too.
(define (kern-load fname)
  (kern-include fname)
  (load fname))

(define nil '())

;; safe-eval -- evaluates an expression; bad expressions evaluate to '()
;; instead of causing an interpreter error
(define (safe-eval expr)
  (cond ((null? expr) '())
        ((symbol? expr)
         (if (defined? expr)
             (eval expr)
             '()))
        (eval expr)))

;; filter -- filter-in elements from a list
(define (filter pred seq)
  (cond ((null? seq) nil)
        ((pred (car seq))
         (cons (car seq)
               (filter pred (cdr seq))))
        (else (filter pred (cdr seq)))))

;; simple, non-prioritized, generic search (not very efficient, so don't try
;; it on large search spaces)
(define (search here? next start maxdepth)
  (define (do-search queue visited depth)
    (if (or (= depth 0) (null? queue)) nil
        (let ((loc (car queue)))
          (if (here? loc) loc
              (do-search (append (cdr queue)
                                 (filter (lambda (v) (not (member v visited)))
                                         (next loc)))
                         (append (list loc) visited)
                         (- depth 1))))))
  (do-search (list start) nil maxdepth))

;; similar to search, but a) it's exhaustive and b) it runs a procedure on
;; every entry (warning: not sure if or how well this works)
(define (bfs-apply start next proc)
  (define (do-search queue visited)
    (if (null? queue) nil
        (let ((loc (car queue)))
          (proc loc)
          (do-search (append (cdr queue)
                             (filter (lambda (v) (not (member v visited)))
                                     (next loc)))
                     (append (list loc) visited)))))
  (do-search (list start) nil))
  
;; Set element k of list x to val (zero-indexed)
(define (list-set-ref! x k val)
  (if (zero? k)
      (set-car! x val)
      (list-set-ref! (cdr x) (- k 1) val)))

;; Check if a list contains an element.
(define (in-list? elem lst)
  (foldr (lambda (a b) (or a (eqv? b elem))) 
         #f 
         lst))

;; Check if a location is passable to a character
(define (passable? loc char)
  (kern-place-is-passable loc char))

;; Check if a location is occupied by a character or party
(define (occupied? loc)
  (foldr (lambda (val kobj) (or val (kern-obj-is-char? kobj)))
         #f 
         (kern-get-objects-at loc)))

;; Given a starting location, search outward for a passable, unoccupied place
;; to put a character.
(define (pick-loc origin char)
  (search (lambda (loc) (and (passable? loc char)
                             (not (occupied? loc))))
          neighbors 
          origin
          10))

;; Generic proc to summon other beings. Used by spells and some special
;; effects.
(define (summon origin mk-critter count)
  (define (run-loop n)
    (if (= n 0) nil
        (let* ((critter (kern-obj-set-temporary (mk-critter) #t))
               (loc (pick-loc origin critter)))
          (cond ((null? loc) nil)
                (else
                 (kern-obj-put-at critter loc)
                 (run-loop (- n 1)))))))
  (run-loop count))

;; Like summon but the beings are permanent, not temporary.
(define (psummon origin mk-critter count)
  (define (run-loop n)
    (if (= n 0) nil
        (let* ((critter (mk-critter))
               (loc (pick-loc origin critter)))
          (cond ((null? loc) 
                 (kern-obj-destroy critter) 
                 nil)
                (else
                 (kern-obj-put-at critter loc)
                 (run-loop (- n 1)))))))
  (run-loop count))


;; Check if an object is hostile toward a character
(define (is-hostile? kchar kobj)
  (and (kern-obj-is-char? kobj)
       (kern-char-is-hostile? kchar kobj)))

;; Find all characters hostile to the given character
(define (all-hostiles kchar)
  (filter (lambda (kobj) (is-hostile? kchar kobj))
          (kern-place-get-objects (loc-place (kern-obj-get-location kchar)))))

(define (is-visible-hostile? kchar kobj)  
  (and (kern-obj-is-char? kobj)
       (kern-char-is-hostile? kchar kobj)
       (let ((charloc (kern-obj-get-location kchar))
             (objloc (kern-obj-get-location kobj)))
         (display "is-visible-hostile? charloc=")(display charloc)
         (display " objloc=")(display objloc)(newline)
         (and (<= (kern-get-distance charloc objloc)
                  (kern-obj-get-vision-radius kchar))
              (kern-in-los? charloc objloc)
              (kern-obj-is-visible? kobj)))))

(define (all-visible-hostiles kchar)
  (display "all-visible-hostiles")(newline)
  (filter (lambda (kobj) (is-visible-hostile? kchar kobj))
          (kern-place-get-objects (loc-place (kern-obj-get-location kchar)))))

;; Find all the characters in a place
(define (all-chars kplace)
  (filter kern-obj-is-char? (kern-place-get-objects kplace)))

(define (all-in-range origin radius objlst)
  (filter (lambda (kobj) (<= (kern-get-distance origin 
                                                (kern-obj-get-location kobj))
                             radius))
          objlst))

;; Convenience proc for rolling dtables by hand
(define (dtable-row . cols)
  (define (dtable-col val)
    (list (list 0 val)))
  (map dtable-col cols))

(define (distance kobj-a kobj-b)
  (let ((loc-a (kern-obj-get-location kobj-a))
        (loc-b (kern-obj-get-location kobj-b)))
  (display "distance:loc-a=")(display loc-a)(display " loc-b=")(display loc-b)(newline)
  (kern-get-distance loc-a loc-b)))

;; Result codes
(define result-ok          0)
(define result-no-target   1)
(define result-no-effect   2)

;; Inefficient code to find nearest obj from a list
(define (nearest-obj kobj klist)
  (display "nearest-obj")(newline)
  (if (null? klist) nil
      (foldr (lambda (a b) (if (< (distance kobj a) (distance kobj b)) a b))
             (car klist) (cdr klist))))

;; Move an object one step along a path to a destination.
(define (pathfind kobj dest)
  ;;(display "pathfind")(newline)
  (define (follow-path path)
    (if (not (null? path))
        (let ((coords (car path))
              (origin (kern-obj-get-location kobj)))
          ;;(display "pathfind:coords=")(display coords)(newline)
          (let ((dx (- (car coords) (loc-x origin)))
                (dy (- (cdr coords) (loc-y origin))))
            ;;(display "pathfind:dx=")(display dx)(display " y=")(display dy)(newline)
            (kern-obj-move kobj dx dy)))))
  (let ((path (kern-obj-find-path kobj dest)))
    ;;(display "pathfind:path=")(display path)(newline)
    (if (not (null? path))
        ;; skip the first location in the path
        (follow-path (cdr path)))))

(define (notnull? val) (not (null? val)))

(define (is-alive? kchar)
  (> (kern-char-get-hp kchar) 0))

(define (has-ap? kobj) 
  (> (kern-obj-get-ap kobj) 0))

(define (flee kchar)
  (display "flee")(newline)
  (kern-char-set-fleeing kchar #t))

(define (wander kchar)
  (kern-obj-wander kchar))

(define (weakest kchar-a kchar-b)
  (if (< (kern-char-get-hp kchar-a)
         (kern-char-get-hp kchar-b))
      a
      b))

;; ----------------------------------------------------------------------------
;; search-rect -- apply a procedure to every location in a rectangular region
;; and return a list of its non-nil results.
;; ----------------------------------------------------------------------------
(define (search-rect kplace x y w h proc)
  (filter notnull? (map proc (loc-enum-rect kplace x y w h))))

;;----------------------------------------------------------------------------
;; Return a list of locations with matching terrain
;;----------------------------------------------------------------------------
(define (find-terrain kplace x y w h kter)
  (define (check loc)
    (if (eqv? (kern-place-get-terrain loc) kter)
        loc
        nil))
  (search-rect kplace x y w h check))

;;----------------------------------------------------------------------------
;; find-objects -- return a list of locations with the given object on them
;;----------------------------------------------------------------------------
(define (find-objects kplace x y w h ktype)
  (define (check loc)
    (define (scanobjlst lst)
      (foldr (lambda (a b) 
               (or a (eqv? (kern-obj-get-type b) ktype)))
             #f
             lst))
    (if (scanobjlst (kern-place-get-objects-at loc))
        loc
        nil))
  (search-rect kplace x y w h check))

(define (in-inventory? kchar ktype)
  (define (hasit? item inv)
    (display "in-inventory:hasit")
    (display " inv=")(display inv)
    (newline)
    (cond ((null? inv) #f)
          ((eqv? item (car (car inv))) #t)
          (else (hasit? item (cdr inv)))))
  (hasit? ktype (kern-char-get-inventory kchar)))

;;============================================================================
;; Modulo system procedures -- useful on wrapping maps
;;============================================================================
(define (madd a b R) (modulo (+ a b) R))
(define (msub a b R) (modulo (- a b) R))
(define (minc a R) (modulo (+ a 1) R))
(define (mdec a R) (modulo (- a 1) R))

;;----------------------------------------------------------------------------
;; mdist - find the distance between two numbers in a modulo system. There are
;; always 2 distances (additive and subtractive). This picks the shortest
;; distance..
;;----------------------------------------------------------------------------
(define (mdist a b R) (min (msub a b R) (msub b a R)))

;; ----------------------------------------------------------------------------
;; Turn on/off verbose scheme garbage collection. Useful if you think scheme is
;; gc'ing some of your code behind your back.
;; ----------------------------------------------------------------------------
;; (gc-verbose #t)

(define (profile proc . args)
  (let ((t (kern-get-ticks))
        (result (apply proc args)))
    (display "*** TIME: ")(display (- (kern-get-ticks) t)) (display " ms")
    (newline)
    result))

;; ----------------------------------------------------------------------------
;; find-object-types-at -- return a list of objects of the given type which can
;; be found at the given location
;; ----------------------------------------------------------------------------
(define (find-object-types-at loc ktype)
  (filter (lambda (a) (eqv? (kern-obj-get-type a) ktype))
          (kern-place-get-objects-at loc)))

;; ----------------------------------------------------------------------------
;; kobj-get -- remove an object from the map and put it into another object
;; ----------------------------------------------------------------------------
(define (kobj-get kchar kobj)
  (kern-obj-remove kobj)
  (kern-obj-put-into kobj kchar)
  (kern-map-repaint))

;; ----------------------------------------------------------------------------
;; kobj-get-at -- get an object of a specific type from the location
;; ----------------------------------------------------------------------------
(define (kobj-get-at kchar loc ktype)
  (let ((objs (find-object-types-at loc ktype)))
    (if (notnull? objs)
        (kobj-get kchar (car objs)))))

;; ----------------------------------------------------------------------------
;; place-random-corner -- randomly select a corner and return it as a location
;; ----------------------------------------------------------------------------
(define (place-random-corner kplace)
  (case (kern-dice-roll "1d4")
    ((1) (mk-loc kplace  0  0))
    ((2) (mk-loc kplace  0  (- (kern-place-get-width kplace 1))))
    ((3) (mk-loc kplace  (- (kern-place-get-height kplace) 1) 0))
    ((4) (mk-loc kplace  
                 (- (kern-place-get-height kplace) 1) 
                 (- (kern-place-get-width kplace) 1)))))
