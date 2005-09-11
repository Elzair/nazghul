
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

(define (obj-is-char? kobj) (kern-obj-is-being? kobj))

;; Check if a location is occupied by a character or party
(define (occupied? loc)
  (foldr (lambda (val kobj) (or val (obj-is-char? kobj)))
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
(define (summon origin mk-critter faction count)
  (define (run-loop n)
    (if (= n 0) nil
        (let* ((critter (kern-obj-set-temporary (kern-being-set-base-faction 
                                                 (mk-critter) 
                                                 faction) 
                                                #t))
               (loc (pick-loc origin critter)))
          (cond ((null? loc) nil)
                (else
                 (kern-obj-put-at critter loc)
                 (run-loop (- n 1)))))))
  (run-loop count))

;; Like summon but the beings are permanent, not temporary.
(define (psummon origin mk-critter count)
  (display "psummon")(newline)
  (define (run-loop n)
    (if (= n 0) nil
        (let* ((critter (kern-obj-inc-ref (mk-critter)))
               (loc (pick-loc origin critter)))
          (cond ((null? loc) (kern-obj-dec-ref critter))
               (else
                (kern-obj-put-at critter loc)
                (kern-obj-dec-ref critter)
                (run-loop (- n 1)))))))
  (run-loop count))

;; check if klooker can see kobj
(define (can-see? klooker kobj)
  (let ((from (kern-obj-get-location klooker))
        (to (kern-obj-get-location kobj)))
    (and (kern-in-los? from to)
         (<= (kern-get-distance from to)
             (kern-obj-get-vision-radius klooker))
         (kern-obj-is-visible? kobj))))

;; check if klooker can can see anything in the list kobs
(define (can-see-any? klooker kobjs)
  (if (null? kobjs)
      #f
      (or (can-see? klooker (car kobjs))
          (can-see-any? klooker (cdr kobjs)))))

;; check if knpc can see any of the player party members
(define (any-player-party-member-visible? knpc)
  (can-see-any? knpc 
                (kern-party-get-members (kern-get-player))))

;; Check if an object is hostile toward a character
(define (is-hostile? kbeing kobj)
  (kern-being-is-hostile? kbeing kobj))

;; Check if an object is allied with a character
(define (is-ally? kbeing kobj)
  (kern-being-is-ally? kbeing kobj))

;; Find all characters hostile to the given character
(define (all-hostiles kchar)
  (filter (lambda (kobj) (is-hostile? kchar kobj))
          (kern-place-get-beings (loc-place (kern-obj-get-location kchar)))))

;; Find all friendlies
(define (all-allies kchar)
  (filter (lambda (kobj) (is-ally? kchar kobj))
          (kern-place-get-beings (loc-place (kern-obj-get-location kchar)))))
  

;; Count the number of hostiles
(define (num-hostiles kchar)
  (length (all-hostiles kchar)))

;; Count the number of friendlies
(define (num-allies kchar)
  (length (all-allies kchar)))

;; Find all beings hostile 
(define (all-visible-hostiles kbeing)
  (kern-being-get-visible-hostiles kbeing))

;; Find all allies
(define (all-visible-allies kbeing)
  (kern-being-get-visible-allies kbeing))

;; Find all the characters in a place
(define (all-chars kplace)
  (kern-place-get-beings kplace))

;; Filter objects out of range
(define (all-in-range origin radius objlst)
  (filter (lambda (kobj) 
            (<= (kern-get-distance origin 
                                   (kern-obj-get-location kobj))
                radius))
          objlst))

;; Return a list of all hostiles in the given range
(define (get-hostiles-in-range kchar range)
  (all-in-range (kern-obj-get-location kchar)
                range
                (kern-being-get-visible-hostiles kchar)))

;; Convenience proc for rolling dtables by hand
(define (dtable-row . cols) cols)

(define (distance kobj-a kobj-b)
  (let ((loc-a (kern-obj-get-location kobj-a))
        (loc-b (kern-obj-get-location kobj-b)))
  (kern-get-distance loc-a loc-b)))

;; Result codes
(define result-ok          0)
(define result-no-target   1)
(define result-no-effect   2)

;; Inefficient code to find nearest obj from a list
(define (nearest-obj kobj klist)
  (if (null? klist) nil
      (foldr (lambda (a b) (if (< (distance kobj a) (distance kobj b)) a b))
             (car klist) (cdr klist))))

;; Move an object one step along a path to a destination.
(define (old-pathfind kobj dest)
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

;; pathfind - use the built-in kernel call that uses cached paths and tries to
;; handle blocking mechanisms
(define (pathfind kobj kdest)
  (and (kern-obj-is-being? kobj)
       (kern-being-pathfind-to kobj kdest)))

(define (can-pathfind? kobj dest)
  (not (null? (kern-obj-find-path kobj dest))))

(define (notnull? val) (not (null? val)))

(define (is-alive? kchar)
  (> (kern-char-get-hp kchar) 0))

(define (dead? kchar)
  (not (is-alive? kchar)))

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

(define (join-player kchar)
  (kern-char-join-player kchar))

;;============================================================================
;; taunt
;;============================================================================
(define (random-select list)
  (list-ref list (modulo (random-next) (length list))))

(define (taunt kchar ktarg taunts)
  (say kchar (random-select taunts)))

;; ----------------------------------------------------------------------------
;; search-rect -- apply a procedure to every location in a rectangular region
;; and return a list of its non-nil results.
;; ----------------------------------------------------------------------------
(define (search-rect kplace x y w h proc)
  (filter notnull? (map proc (loc-enum-rect kplace x y w h))))

;; ----------------------------------------------------------------------------
;; foldr-rect -- similar to search-rect above, but the procedure must
;; accumulate its own results. Faster because it doesn't have to run the
;; filter.
;; ----------------------------------------------------------------------------
(define (foldr-rect kplace x y w h proc ival)
  (foldr proc ival (loc-enum-rect kplace x y w h)))

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
    (if (scanobjlst (kern-get-objects-at loc))
        loc
        nil))
  (search-rect kplace x y w h check))

(define (in-inventory? kchar ktype)
  (define (hasit? item inv)
    (display "inv: ")(display inv)(newline)
    (cond ((null? inv) #f)
          ((eqv? item (car (car inv))) #t)
          (else (hasit? item (cdr inv)))))
  (hasit? ktype (kern-char-get-inventory kchar)))

(define (use-item-from-inventory-on-self kchar ktype)
  (kern-obj-remove-from-inventory kchar ktype 1)
  (display "using")(newline)
  (apply (kern-type-get-gifc ktype) (list 'use ktype kchar))
  (kern-log-msg (kern-obj-get-name kchar)
                " uses a(n) "
                (kern-type-get-name ktype)))

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
          (kern-get-objects-at loc)))

;; ----------------------------------------------------------------------------
;; is-object-type-at? -- check for an object (by type) at a location
;; ----------------------------------------------------------------------------
(define (is-object-type-at? loc ktype)
  (foldr (lambda (a b) (or a (eqv? (kern-obj-get-type b) ktype)))
         #f
         (kern-get-objects-at loc)))
  

;; ----------------------------------------------------------------------------
;; kobj-get -- remove an object from the map and put it into another object
;; ----------------------------------------------------------------------------
(define (kobj-get kobj kchar)
  (kern-obj-inc-ref kobj)
  (kern-obj-remove kobj)
  (kern-obj-put-into kobj kchar)
  (kern-obj-dec-ref kobj)
  (kern-map-repaint))

;; ----------------------------------------------------------------------------
;; kobj-get-at -- get an object of a specific type from the location
;; ----------------------------------------------------------------------------
(define (kobj-get-at kchar loc ktype)
  (let ((objs (find-object-types-at loc ktype)))
    (if (notnull? objs)
        (kobj-get (car objs) kchar))))

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

;; ----------------------------------------------------------------------------
;; do-or-goto -- if the location is adjacent then the proc, otherwise have
;; the char pathfind to it
;; ----------------------------------------------------------------------------
(define (do-or-goto kchar coords proc)
  (display "do-or-goto")(newline)
  (if (or (loc-adjacent? (kern-obj-get-location kchar) coords)
          (eq? coords (kern-obj-get-location kchar)))
      (proc kchar coords)
      (pathfind kchar coords)))

;; ----------------------------------------------------------------------------
;; evade -- simple alg for evading melee foes
;;
;; Simple approach: each foe's coordinates forms a vector to the char's
;; coordinates. Take the sum of these coordinates to get the evasion
;; vector. "Normalize" the vector components by rounding them to the nearest 0,
;; 1 or -1. This is the dx/dy to move. If the terrain is impassable in the
;; preferred direction then try zeroing out the non-zero components and
;; moving. This will give two backup vectors to try.
;;
;; ADDENDUM: I don't want to allow diagonal evasion, so the "normalized" vector
;; must be skipped if it's a diagonal, thus causing us to try the fallbak
;; vector(s).
;; ----------------------------------------------------------------------------
(define (evade kchar foes)
  (let* ((tloc (kern-obj-get-location kchar))
         (v (loc-norm (foldr (lambda (a b) 
                               (loc-sum a 
                                        (loc-diff tloc 
                                                  (kern-obj-get-location b))))
                             (mk-loc (loc-place tloc) 0 0)
                             foes))))
    (define (move dx dy)
      ;; Note: stepping on impassable terrain can have bad side effects, so
      ;; avoid it
      (if (kern-place-is-passable (loc-sum (mk-loc (loc-place tloc) dx dy) tloc) kchar)
          (kern-obj-move kchar dx dy)
          #f))
    (define (evade-on-normal)
      (and (or (eq? 0 (loc-x v))
               (eq? 0 (loc-y v)))
           (move (loc-x v) (loc-y v))))
    (or (evade-on-normal)
        (and (not (eq? 0 (loc-y v)))
             (move (loc-x v) 0))
        (and (not (eq? 0 (loc-x v)))
             (move 0 (loc-y v))))))

;; ----------------------------------------------------------------------------
;; closest-obj -- given an origin and a list of objects, return the object from
;; the list that is closest (in city-block distance) to the origin
;; ----------------------------------------------------------------------------
(define (closest-obj origin lst)
  (if (null? lst) nil
      (foldr (lambda (a b) 
               (if (loc-closer? (kern-obj-get-location a)
                                (kern-obj-get-location b)
                                origin)
                   a
                   b))
               (car lst)
               (cdr lst))))

;; ----------------------------------------------------------------------------
;; blit-maps -- blit multiple maps to a single target map
(;; ---------------------------------------------------------------------------
define (blit-maps kmap . blits)
  (define (blit dstx dsty srcmap srcx srcy w h)
    (kern-blit-map kmap dstx dsty srcmap srcx srcy w h))
  (foldr (lambda (a b) (apply blit b))
         kmap
         blits))

(define (fill-terrain kter kplace ox oy ow oh)
  (define (fill x y w h)
    (if (> h 0)
        (if (> w 0)
            (begin
              (kern-place-set-terrain (list kplace x y) kter)
              (fill (+ x 1) y (- w 1) h))
            (fill ox (+ y 1) ow (- h 1)))))
  (fill ox oy ow oh))

;;============================================================================
;; rect 
;;============================================================================
(define (mk-rect x y w h) (list x y w h))
(define (rect-x r) (car r))
(define (rect-y r) (cadr r))
(define (rect-w r) (caddr r))
(define (rect-h r) (cadddr r))
(define (rect-ex r) (+ (rect-x r) (rect-w r)))
(define (rect-ey r) (+ (rect-y r) (rect-h r)))
(define (x-in-rect? x r)
  (and (>= x (rect-x r))
       (< x (rect-ex r))))
(define (y-in-rect? y r)
  (and (>= y (rect-y r))
       (< y (rect-ey r))))
(define (xy-in-rect? x y r)
  (and (x-in-rect? x r)
       (y-in-rect? y r)))
(define (rect-in-rect? a b)
  (and (xy-in-rect? (rect-x a) (rect-y a) b)
       (xy-in-rect? (rect-ex a) (rect-ey a) b)))
(define (loc-in-rect? loc rect)
  (xy-in-rect? (loc-x loc)
               (loc-y loc)
               rect))

(define original-load load)  
(define (load file)
  (display (kern-get-ticks))(display " loading ")(display file)(newline)
  (original-load file))

(define (put obj x y) (list obj x y))

;; lookup-spell-by-handler -- find a spell in the list of all spells
(define (lookup-spell handler)
  (define (search-spells slist)
    (if (null? slist)
        nil
        (let ((spell (car slist)))
          (if (eqv? (spell-handler spell)
                    handler)
              spell
              (search-spells (cdr slist))))))
  (search-spells spells))

;; can-cast -- check if a char has enough mana to cast a spell
(define (can-cast? kchar handler)
  (let ((spell (lookup-spell handler)))
    (if (null? spell)
        #f
        (and (>= (kern-char-get-mana kchar)
                 (spell-cost spell))
             (>= (kern-char-get-level kchar)
                 (spell-level spell))))))
  
;; cast0 - cast a spell which requires no args if possible, assumes kchar has
;; enough mana
(define (cast0 kchar spell)
  (apply (spell-handler spell) (list kchar))
  (kern-char-dec-mana kchar (spell-cost spell))
  (kern-obj-dec-ap kchar (spell-ap spell))
  (kern-log-msg (kern-obj-get-name kchar) 
                " casts " 
                (spell-name spell)))

;; cast1 - cast a spell which requires one arg if possible, assumes kchar has
;; enough mana
(define (cast1 kchar spell ktarg)
  (display "cast1: ")(display spell)(newline)
  (apply (spell-handler spell) (list kchar ktarg))
  (kern-char-dec-mana kchar (spell-cost spell))
  (kern-obj-dec-ap kchar (spell-ap spell))
  (kern-log-msg (kern-obj-get-name kchar) 
                " casts " 
                (spell-name spell)
                " on "
                (kern-obj-get-name ktarg)
                "!"))
  
;; ----------------------------------------------------------------------------
;; terrain-ok-for-field? -- check if the terrain at a given location will allow
;; a field to be dropped on it. Terrains with passability class equivalent to
;; Grass, trees and forest are ok, everything else is not.
;; ----------------------------------------------------------------------------
(define (terrain-ok-for-field? loc)
  (let ((pclass (kern-terrain-get-pclass (kern-place-get-terrain loc))))
    (display "pclass=")(display pclass)(newline)
    (foldr (lambda (a b) (or a (= pclass b)))
           #f
           (list pclass-grass pclass-trees pclass-forest))))

(define (get-8-neighboring-tiles loc)
  (let ((kplace (loc-place loc))
        (x (loc-x loc))
        (y (loc-y loc)))
    (filter kern-is-valid-location?
            (map (lambda (offset) (mk-loc kplace 
                                          (+ (car offset) x)
                                          (+ (cdr offset) y)))
                 (list (cons -1 -1)
                       (cons  0 -1)
                       (cons  1 -1)
                       (cons -1  0)
                       (cons  1  0)
                       (cons -1  1)
                       (cons  0  1)
                       (cons  1  1))))))

(define (get-4-neighboring-tiles loc)
  (let ((kplace (loc-place loc))
        (x (loc-x loc))
        (y (loc-y loc)))
    (filter kern-is-valid-location?
            (map (lambda (offset) (mk-loc kplace 
                                          (+ (car offset) x)
                                          (+ (cdr offset) y)))
                 (list (cons  0 -1)
                       (cons -1  0)
                       (cons  1  0)
                       (cons  0  1)
                       )))))

(define (shake-map dur)
  (if (> dur 0)
      (begin
        (kern-map-set-jitter #t)
        (kern-map-repaint)
        (shake-map (- dur 1)))
      (begin
        (kern-map-set-jitter #f)
        (kern-map-repaint))))

(define (push kobj dx dy dist)
  (let* ((loc (loc-sum (kern-obj-get-location kobj)
                       (mk-loc nil dx dy))))
    (if (kern-place-is-passable loc kobj)
        (begin 
          (kern-obj-relocate kobj loc nil)
          #t)
        #f)))

(define (stagger kchar)
  (display "stagger")(newline)
  (let ((vdir (random-select (list (cons -1 0) 
                                   (cons 1 0) 
                                   (cons 0 -1) 
                                   (cons 0 1)))))
  (push kchar (car vdir) (cdr vdir) 1)))

(define (end-turn kobj)(kern-obj-set-ap kobj 0))

(define (add-effect-multiple kobj keff fgob q)
  (if (> q 0)
      (begin
        (kern-obj-add-effect kobj keff fgob)
        (add-effect-multiple kobj keff fgob (- q 1)))))

;; time procs for use with return value from kern-get-time:
(define (time-hour time)(car time))
(define (time-minute time) (cdr time))

(define (is-player-party-member? kchar)
  (in-list? kchar 
            (kern-party-get-members (kern-get-player))))
