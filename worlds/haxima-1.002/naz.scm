
;; init.scm -- contains lots of common scheme utilities
(load "init.scm")

;; Result codes (these belong here because they are tied to kernel values, see
;; result.h)
(define result-ok          0)
(define result-no-target   1)
(define result-no-effect   2)
(define result-no-hostiles 3)
(define result-lacks-skill 4)
(define result-failed      5)
(define result-not-here    6)
(define result-critical-fail 7)
(define result-not-now     8)

;; Test if a result code indicates that the action was not completed
(define (abortive-result? result)
  (or (eq? result result-no-target)
      (eq? result result-not-here)))

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
         
(define (in-text-list? elem lst)
  (foldr (lambda (a b) (or a (equal? b elem))) 
         #f 
         lst))

;; Check if a location is passable to a character
(define (passable? loc kobj)
  (kern-place-is-passable loc kobj))

(define (obj-is-char? kobj) (kern-obj-is-being? kobj))
(define (is-being? kobj) (kern-obj-is-being? kobj))

;; Check if a location is occupied by a character or party
(define (occupied? loc)
  (foldr (lambda (val kobj) (or val (obj-is-char? kobj)))
         #f 
         (kern-get-objects-at loc)))

(define (get-beings-at loc)
  (filter kern-obj-is-being?
          (kern-get-objects-at loc)))

;; Given a starting location, search outward for a passable, unoccupied place
;; to put a character.
(define (pick-loc origin char)
  (search (lambda (loc) (and (kern-is-valid-location? loc)
									(passable? loc char)
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
  ;;;(display "psummon")(newline)
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

;; gets location of player character (not party- ie 'works' in temporary map)
(define (player-member-loc)
  (let ((loc (kern-obj-get-location (car (kern-party-get-members (kern-get-player))))))
    (if (null? loc)
        nil
        (loc-place loc))))
                
(define (num-player-party-members)
  ;;(display "num-player-party-members")(newline)
  (length (kern-party-get-members (kern-get-player))))

(define (is-only-living-party-member? kchar)
  (and (is-alive? kchar)
       (is-player-party-member? kchar)
       (not (foldr (lambda (found kchar2)
                     (println  found " " (kern-obj-get-name kchar2))
                     (or found
                         (and (not (eqv? kchar kchar2))
                              (is-alive? kchar2))))
                   #f
                   (kern-party-get-members (kern-get-player))))
       ))

;; Check if an object is hostile toward a character
(define (is-hostile? kbeing kobj)
  (and (is-being? kobj)
       (kern-being-is-hostile? kbeing kobj)))

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

(define (any-visible-hostiles? kchar)
  (> (length (all-visible-hostiles kchar)) 0))

(define (nearest-visible-hostile kchar)
  (nearest-obj kchar (all-visible-hostiles kchar)))

;; Find all allies
(define (all-visible-allies kbeing)
  (kern-being-get-visible-allies kbeing))

;; Count the number of visible friendlies
(define (num-visible-allies kchar)
  (length (all-visible-allies kchar)))

;; Count the number of hostiles
(define (num-visible-hostiles kchar)
  (length (all-visible-hostiles kchar)))


;; Find all the characters in a place
(define (all-chars kplace)
  (kern-place-get-beings kplace))

;; Check if an object is in the given range of the origin point
(define (in-range? origin radius kobj)
  (<= (kern-get-distance origin
                         (kern-obj-get-location kobj))
      radius))

;; Check if a character's target is in range
(define (can-hit? kchar ktarg range)
  ;;(println "can-hit: " range)
  (in-range? (kern-obj-get-location kchar)
             range
             ktarg))

;; Filter objects out of range
(define (all-in-range origin radius objlst)
  (filter (lambda (kobj) 
            (<= (kern-get-distance origin 
                                   (kern-obj-get-location kobj))
                radius))
          objlst))

;; Return a list of all hostiles in range of the given location
(define (get-hostiles-in-range-of-loc kchar range loc)
  (all-in-range loc
                range
                (kern-being-get-visible-hostiles kchar)))

;; Return a list of all hostiles in range of the kchar's current location
(define (get-hostiles-in-range kchar range)
  (get-hostiles-in-range-of-loc kchar
                                range
                                (kern-obj-get-location kchar)))

;; Return a list of beings within the given range
(define (get-beings-in-range kobj range)
  (let ((loc (kern-obj-get-location kobj)))
  (all-in-range loc
                range
                (kern-place-get-beings (loc-place loc)))))

;; Convenience proc for rolling dtables by hand
(define (dtable-row . cols) cols)

(define (distance kobj-a kobj-b)
  (let ((loc-a (kern-obj-get-location kobj-a))
        (loc-b (kern-obj-get-location kobj-b)))
  (kern-get-distance loc-a loc-b)))

;; Inefficient code to find nearest obj from a list
(define (nearest-obj kobj klist)
  (if (null? klist) nil
      (foldr (lambda (a b) (if (< (distance kobj a) (distance kobj b)) a b))
             (car klist) (cdr klist))))

;; Inefficient code to find nearest location from a list
(define (nearest-loc kobj klist)
  (println "nearest-loc: " klist)
  (if (null? klist) 
      nil
      (let ((kloc (kern-obj-get-location kobj)))
        (foldr (lambda (a b) 
                 (if (< (loc-city-block-distance kloc a) 
                        (loc-city-block-distance kloc b)) 
                     a 
                     b))
               (car klist) 
               (cdr klist)))))

;; Move an object one step along a path to a destination.
(define (old-pathfind kobj dest)
  ;;;;(display "pathfind")(newline)
  (define (follow-path path)
    (if (not (null? path))
        (let ((coords (car path))
              (origin (kern-obj-get-location kobj)))
          ;;;;(display "pathfind:coords=");;(display coords)(newline)
          (let ((dx (- (car coords) (loc-x origin)))
                (dy (- (cdr coords) (loc-y origin))))
            (kern-obj-move kobj dx dy)))))
  (let ((path (kern-obj-find-path kobj dest)))
    ;;;;(display "pathfind:path=");;(display path)(newline)
    (if (not (null? path))
        ;; skip the first location in the path
        (follow-path (cdr path)))))

;; pathfind - use the built-in kernel call that uses cached paths and tries to
;; handle blocking mechanisms
(define (pathfind kobj kdest)
  ;;(println "pathfind(" (kern-obj-get-name kobj) "," kdest ")")
  (and (kern-obj-is-being? kobj)
       (kern-being-pathfind-to kobj kdest)))

(define (can-pathfind? kobj dest)
  (println "can-pathfind?")
  (or (loc-8-adjacent? dest 
                     (kern-obj-get-location kobj))
      (not (null? (kern-obj-find-path kobj dest)))))

(define (notnull? val) (not (null? val)))

(define (being-at? loc)
  (not (null? (filter kern-obj-is-being? (kern-get-objects-at loc)))))

(define (get-being-at loc)
  (let ((beings (filter kern-obj-is-being? (kern-get-objects-at loc))))
    (if (null? beings)
        nil
        (car beings))))

(define (is-dead? kchar)
  (kern-char-is-dead? kchar))

(define (is-alive? kchar)
  (not (is-dead? kchar)))

(define (has-ap? kobj) 
  (> (kern-obj-get-ap kobj) 0))

(define (has-ap-debt? kobj)
  (< (kern-obj-get-ap kobj) 0))

(define (has-skill? kchar kskill)
  (in-list? kskill
            (kern-char-get-skills kchar)))

(define (flee kchar)
  ;;;(display "flee")(newline)
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

(define (random-select list)
  (if (or (null? list)
          (= 0 (length list)))
      nil
      (list-ref list (modulo (random-next) (length list)))))

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

(define (on-terrain? kobj kter)
  (eqv? kter (kern-place-get-terrain (kern-obj-get-location kobj))))

(define (all-visible-terrain-of-type kobj kter)
  (filter (lambda (x)
            (eqv? kter
                  (kern-place-get-terrain x)))
          (kern-being-get-visible-tiles kobj)))

(define (find-nearest-visible-terrain-of-type kobj kter)
  (nearest-loc kobj (all-visible-terrain-of-type kobj kter)))
    
(define (hidden? kchar)
  ;;(println "hidden?")
  ;; Just check if the 8 neighbors are all los-blocking
  (let ((loc (kern-obj-get-location kchar)))
    (foldr-rect (loc-place loc)
                (- (loc-x loc) 1) (- (loc-y loc) 1)
                3 3
               (lambda (val neighbor)
                 ;;(println neighbor " neighbor? " (equal? neighbor loc) " blocks? " (kern-place-blocks-los? neighbor))
                 (and val
                      (or (eq? neighbor loc)
                          (kern-place-blocks-los? neighbor))))
                #t
                )))
  
;; kobj-is-type -- check if the object is of the given type
(define (kobj-is-type? kobj ktype)
  (eqv? (kern-obj-get-type kobj)
        ktype))

;; kplace-get-objects-of-type -- return a list of all objects of the given type
;; in the given place
(define (kplace-get-objects-of-type kplace ktype)
  (filter (lambda (kobj) (kobj-is-type? kobj ktype))
          (kern-place-get-objects kplace)))

;;----------------------------------------------------------------------------
;; find-objects -- return a list of locations with the given object on them
;;----------------------------------------------------------------------------
(define (find-objects kplace x y w h ktype)
  (define (check loc)
    (define (scanobjlst lst)
      (foldr (lambda (a b) 
               (or a (kobj-is-type? b ktype)))
             #f
             lst))
    (if (scanobjlst (kern-get-objects-at loc))
        loc
        nil))
  (search-rect kplace x y w h check))

(define (in-inventory? kchar ktype)
  ;;(println (kern-type-get-name ktype))
  (define (hasit? item inv)
    (cond ((null? inv) #f)
          ((eqv? item (car (car inv))) #t)
          (else 
           ;;(println " " (kern-type-get-name (car (car inv))))
           (hasit? item (cdr inv)))))
  (hasit? ktype (kern-char-get-inventory kchar)))

(define (num-in-inventory kchar ktype)
  (define (count-em item inv)
    ;;;(display "inv: ");;(display inv)(newline)
    (cond ((null? inv) 0)
          ((eqv? item (car (car inv))) (cdr (car inv)))
          (else (count-em item (cdr inv)))))
  (count-em ktype (kern-char-get-inventory kchar)))

(define (any-in-inventory? kchar lst)
  (foldr (lambda (v k)
           (or v
               (in-inventory? kchar k)))
         #f
         lst))

(define (all-in-inventory? kchar lst)
  (foldr (lambda (v k)
           (and v
               (in-inventory? kchar k)))
         #t
         lst))

;; Note: I commented out the remove-from-inventory call because the things
;; should remove themselves (eg, potions do)
(define (use-item-from-inventory-on-self kchar ktype)
  ;;(kern-obj-remove-from-inventory kchar ktype 1)
  ;;;(display "using")(newline)
  (apply (kern-type-get-gifc ktype) (list 'use ktype kchar))
  (kern-log-msg (kern-obj-get-name kchar)
                " uses 1 "
                (kern-type-get-name ktype))
  #t)

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
(gc-verbose #t)

(define (profile proc . args)
  (let ((t (kern-get-ticks))
        (result (apply proc args)))
    ;;(display "*** TIME: ");;(display (- (kern-get-ticks) t)) ;;(display " ms")
    (newline)
    result))

;; ----------------------------------------------------------------------------
;; find-object-types-at -- return a list of objects of the given type which can
;; be found at the given location
;; ----------------------------------------------------------------------------
(define (find-object-types-at loc ktype)
  (filter (lambda (a) (kobj-is-type? a ktype))
          (kern-get-objects-at loc)))

;; ----------------------------------------------------------------------------
;; is-object-type-at? -- check for an object (by type) at a location
;; ----------------------------------------------------------------------------
(define (is-object-type-at? loc ktype)
  (foldr (lambda (a b) (or a (kobj-is-type? b ktype)))
         #f
         (kern-get-objects-at loc)))

;; ----------------------------------------------------------------------------
;; any-object-types-at? -- returns #t iff one or more objects at loc is of one
;; of the given types
;; ----------------------------------------------------------------------------
(define (any-object-types-at? loc ktypes)
  (foldr (lambda (a b) (or a (is-object-type-at? loc b)))
         #f
         ktypes))

;; is-player-party-member? -- #t iff kchar is in player party  
(define (is-player-party-member? kchar)
  (in-list? kchar
            (kern-party-get-members (kern-get-player))))

;; ----------------------------------------------------------------------------
;; kobj-get -- remove an object from the map and put it into another object
;; ----------------------------------------------------------------------------
(define (kobj-get kobj kchar)
  (if (kern-obj-put-into kobj kchar)
      (begin
        (if (not (is-player-party-member? kchar))        
            (kern-log-msg (kern-obj-get-name kchar)
                          " gets "
                          (kern-obj-get-name kobj)))
        (kern-obj-inc-ref kobj)
        (kern-obj-remove kobj)
        (kern-obj-dec-ref kobj)
        (kern-obj-dec-ap kchar (/ norm 5))
        (kern-map-repaint))))

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
  ;;;(display "do-or-goto")(newline)
  (if (or (loc-4-adjacent? (kern-obj-get-location kchar) coords)
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
;;
;; Now allowing diagonals, since that factor has changed
;;
;; TODO: probably shouldnt flee over dangerous terrains
;;
;; ----------------------------------------------------------------------------
(define (evade kchar foes)
  (let* ((tloc (kern-obj-get-location kchar))
         (v (loc-canonical
				(foldr
					(lambda (accum thisfoe) 
						(loc-sum accum 
							(loc-diff (kern-obj-get-location thisfoe) tloc)
						))
					(mk-loc (loc-place tloc) 0 0)
					foes)
				))
			)
		(define (move dx dy)
			(if (kern-place-is-passable
					(loc-sum
						(mk-loc (loc-place tloc) dx dy) 
						tloc) 
					kchar)
				(kern-obj-move kchar dx dy)
				#f))
		(define (evade-on-normal)
				(move (loc-x v) (loc-y v)))    
				   
		(or (evade-on-normal)
			(and (not (eq? 0 (loc-y v)))
				(move (loc-x v) 0))
			(and (not (eq? 0 (loc-x v)))
				(move 0 (loc-y v))))
		))


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
;; ---------------------------------------------------------------------------
(define (blit-maps kmap . blits)
  (define (blit dstx dsty srcmap srcx srcy w h)
    (kern-blit-map kmap dstx dsty srcmap srcx srcy w h))
  (foldr (lambda (a b) (apply blit b))
         kmap
         blits))

(define (fill-terrain-prob kter kplace ox oy ow oh prob)
  (define (fill x y w h)
    (if (> h 0)
        (if (> w 0)
            (begin
              (if (<= (modulo (random-next) 
                              100) 
                      prob)
                  (kern-place-set-terrain (list kplace x y) kter))
              (fill (+ x 1) y (- w 1) h))
            (fill ox (+ y 1) ow (- h 1)))))
  (fill ox oy ow oh))

(define (fill-terrain kter kplace ox oy ow oh)
  (fill-terrain-prob kter kplace ox oy ow oh 100))

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
(define (rect-random rect)
  (list (+ (rect-x rect) (modulo (random-next) (rect-w rect)))
        (+ (rect-y rect) (modulo (random-next) (rect-h rect)))))

;;;; (define original-load load)  
;;;; (define (load file)
;;;;    (display (kern-get-ticks))
;;;;    (display " loading ")
;;;;    (display file)(newline)
;;;;    (original-load file))

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

;; generic lookup
(define (lookup this? slist)
  (if (null? slist)
      nil
      (if (this? (car slist))
          (car slist)
          (lookup this? (cdr slist)))))

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
  ;;;(display "cast1: ");;(display spell)(newline)
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
  (let ((kter (kern-place-get-terrain loc)))
    (println "kter: " kter)
    (if (null? kter)
        #f
        (let ((pclass (kern-terrain-get-pclass kter)))
          (foldr (lambda (a b) (or a (= pclass b)))
                 #f
                 (list pclass-grass pclass-trees pclass-forest))))))

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

(define (random-vdir)
  (random-select (list (cons -1 0) 
                       (cons 1 0) 
                       (cons 0 -1) 
                       (cons 0 1))))

(define (random-neighbor-loc kobj)
  (let ((vdir (random-vdir)))
    (loc-sum (kern-obj-get-location kobj)
             (mk-loc nil (car vdir) (cdr vdir)))))

(define (push kobj dx dy dist)
  (let* ((loc (loc-sum (kern-obj-get-location kobj)
                       (mk-loc nil dx dy))))
    (if (and (kern-place-is-passable loc kobj)
             (not (occupied? loc)))
        (begin 
          (kern-obj-relocate kobj loc nil)
          #t)
        #f)))

(define (stagger kchar)
  (let ((vdir (random-vdir)))
    (push kchar (car vdir) (cdr vdir) 1)))

(define (end-turn kobj)(kern-obj-set-ap kobj 0))

(define (add-effect-multiple kobj keff fgob q)
  (if (> q 0)
      (begin
        (kern-obj-add-effect kobj keff fgob)
        (add-effect-multiple kobj keff fgob (- q 1)))))

;; time procs for use with return value from kern-get-time:
(define (time-mk yr mo we da hr mi)
  (list yr mo we da hr mi))
(define (time-year time) (list-ref time 0))
(define (time-month time) (list-ref time 1))
(define (time-week time) (list-ref time 2))
(define (time-day time) (list-ref time 3))
(define (time-hour time) (list-ref time 4))
(define (time-minute time) (list-ref time 5))

;; wants-healing? -- check if a char is <= 50% max hp
(define (wants-healing? kchar)
  (<= (kern-char-get-hp kchar)
      (/ (kern-char-get-max-hp kchar) 2)))

;; wants-healing? -- check if a char is <= 25% max hp
(define (wants-great-healing? kchar)
  (<= (kern-char-get-hp kchar)
      (/ (kern-char-get-max-hp kchar) 4)))

;; wants-mana? -- check if a char is <= 50% max mana
(define (wants-mana? kchar)
  (<= (kern-char-get-mana kchar)
      (/ (kern-char-get-max-mana kchar) 2)))

;; has-mana-potion? -- check if a char has a mana potion in inventory
(define (has-mana-potion? kchar)
  (in-inventory? kchar t_mana_potion))

;; drink-mana-potion -- use a mana potion from inventory
(define (drink-mana-potion kchar)
  (use-item-from-inventory-on-self kchar t_mana_potion))

;; has-heal-potion? -- check if a char has a heal potion in inventory
(define (has-heal-potion? kchar)
  (in-inventory? kchar t_heal_potion))

;; drink-heal-potion -- use a heal potion from inventory
(define (drink-heal-potion kchar)
  (use-item-from-inventory-on-self kchar t_heal_potion))

(define (set-max-hp kchar)
  (kern-char-set-hp kchar 
                    (kern-char-get-max-hp kchar)))

;; max-hp -- calc max hp given species, level and occ
(define (max-hp sp occ lvl mod mult)
  (+ (kern-species-get-hp-mod sp)
     (if (null? occ) 0 (kern-occ-get-hp-mod occ))
     mod
     (* lvl
        (+ (kern-species-get-hp-mult sp)
           (if (null? occ) 0 (kern-occ-get-hp-mult occ))
           mult))))

;; max-mp -- calc max mp given species, level and occ
(define (max-mp sp occ lvl mod mult)
  (+ (kern-species-get-mp-mod sp)
     (if (null? occ) 0 (kern-occ-get-mp-mod occ))
     mod
     (* lvl
        (+ (kern-species-get-mp-mult sp)
           (if (null? occ) 0 (kern-occ-get-mp-mult occ))
           mult))))
  

;; set-level -- set character to level and max out hp and mana (intended for
;; new npc creation)
(define (set-level kchar lvl)
  (kern-char-set-level kchar lvl))

;; use-potion? -- use potion on self if desired and available
(define (use-potion? kchar)
  (or (and (wants-healing? kchar)
           (has-heal-potion? kchar)
           (drink-heal-potion kchar))
      (and (wants-mana? kchar)
           (has-mana-potion? kchar)
           (drink-mana-potion kchar))))

(define (use-heal-spell-on-self? kchar)
  ;;;;(display "use-heal-spell-on-self?")(newline)
  (and (wants-healing? kchar)
       (can-use-ability? heal-ability kchar)
       (use-ability heal-ability kchar kchar)))

(define (use-great-heal-spell-on-self? kchar)
  ;;;;(display "use-great-heal-spell-on-self?")(newline)
  (and (wants-great-healing? kchar)
       (can-use-ability? great-heal-ability kchar)
       (use-ability great-heal-ability kchar kchar)))

(define (use-spell-on-self? kchar)
  ;;;;(display "use-spell-on-self?")(newline)
  (or (use-great-heal-spell-on-self? kchar)
      (use-heal-spell-on-self? kchar)))

(define (avoid-melee? kchar)
  ;;;;(display "avoid-melee? kchar")(newline)
  (let ((nearby-foes (get-hostiles-in-range kchar 1)))
    (if (null? nearby-foes)
        #f
        (evade kchar nearby-foes))))

(define (dump-char kchar)
  (if (null? kchar)
      (println "nil")
      (begin
        (println "npc: " (kern-obj-get-name kchar)
                 "[" (kern-char-get-level kchar) "]"
                 " hp=" (kern-char-get-hp kchar) "/" (kern-char-get-max-hp kchar)
                 " mp=" (kern-char-get-mana kchar) "/" (kern-char-get-max-mana kchar)
                 " @[" (loc-x (kern-obj-get-location kchar)) 
                 "," (loc-y (kern-obj-get-location kchar)) "]"
                 ))))
           

(define (get-nearest-patient kchar)
  (let ((kloc (kern-obj-get-location kchar)))
    (foldr (lambda (kpatient ktarg)
             ;;(display "  checking ")(dump-char ktarg)
             (if (and (wants-healing? ktarg)
                      (or (null? kpatient)                      
                          (< (kern-get-distance kloc 
                                                (kern-obj-get-location ktarg))
                             (kern-get-distance kloc 
                                                (kern-obj-get-location kpatient)))))
                 ktarg
                 kpatient))
           nil
           (all-visible-allies kchar))))

;; This is for medics. A patient is an ally that needs healing. If a patient is
;; less than 2 tiles away then do nothing. If a patient is more than 2 tiles
;; away then pathfind toward it.
(define (move-toward-patient? kchar)
  (let ((patient (get-nearest-patient kchar)))
    (if (null? patient)
        #f
        (begin
          ;;(display "selected ")(dump-char patient)
          (if (in-range? (kern-obj-get-location kchar)
                         2
                         patient)
              #f
              (pathfind kchar (kern-obj-get-location patient)))))))

(define (prompt-for-key)
  (kern-log-msg "<Hit any key to continue>")
  (kern-ui-waitkey))

(define (ship-at? loc) (not (null? (kern-place-get-vehicle loc))))

(define (take-player-gold q)
  (kern-player-set-gold (- (kern-player-get-gold) q)))

(define (give-player-gold q)
  (kern-player-set-gold (+ (kern-player-get-gold) q)))

(define (player-has-gold? q)
  (>= (kern-player-get-gold) q))

;; services -- used with trade-service below
(define (svc-mk name price proc) (list name price proc))
(define (svc-name svc) (car svc))
(define (svc-price svc) (cadr svc))
(define (svc-proc svc) (caddr svc))

;; some standard healer services
(define (heal-service kchar knpc)
  ;;(display "heal-service")(newline)
  (let ((hp (- (kern-char-get-max-hp kchar)
               (kern-char-get-hp kchar))))
    (if (> hp 0)
        (begin
          (say knpc "VAS MANI! Be healed, "
               (kern-obj-get-name kchar))
          (kern-map-flash hp)
          (kern-obj-heal kchar hp)
          #t)
        (begin
          (say knpc (kern-obj-get-name kchar)
               " is not wounded!")
          (prompt-for-key)
          #f))))
  
(define (cure-service kchar knpc)
  ;;(display "cure-service")(newline)
  (if (is-poisoned? kchar)
      (begin
        (say knpc "AN NOX! You are cured, "
             (kern-obj-get-name kchar))
        (kern-map-flash 1)
        (kern-obj-remove-effect kchar ef_poison))
      (begin
        (say knpc (kern-obj-get-name kchar)
             " is not poisoned!")
        (prompt-for-key)
        #f)))

(define (resurrect-service kchar knpc)
  ;;(display "resurrect-service")(newline)
  (if (is-dead? kchar)
      (begin
       (say knpc "IN MANI CORP! Arise, "
            (kern-obj-get-name kchar))
       (kern-map-flash 500)
       (resurrect kchar)
       (kern-obj-heal kchar 10))
      (begin
        (say knpc (kern-obj-get-name kchar)
             " is not dead!")
        (prompt-for-key)
        #f)))

;; trade-services -- take a list of services which operate on a party member
;; and prompt the player, check prices, and otherwise handle the transaction
(define (trade-services knpc kpc services)

  (define (list-services)
    (map (lambda (svc)
           (string-append (svc-name svc) 
                          "..." 
                          (number->string (svc-price svc))
                          " gold"))
         services))

  ;; line-name - convert a string like "Heal...30 gold" to "Heal"
  (define (line-name line)
    (define (extract l)
      (if (null? l)
          nil
          (if (char=? (car l) #\.)
              nil
              (cons (car l) (extract (cdr l))))))
    (if (null? line)
        nil
        (list->string (extract (string->list line)))))

  (define (lookup-svc line)
    (let ((name (line-name line)))
      (if (null? name)        
          nil
          (lookup (lambda (svc) 
                    (string=? name
                              (svc-name svc)))
                  services))))

  (define (choose-svc)
    (lookup-svc (apply kern-ui-select-from-list (list-services))))

  (let ((svc (choose-svc)))

    (define (can-pay?)
      (if (player-has-gold? (svc-price svc))
          #t
          (begin
            (say knpc "You don't have enough gold!")
            #f)))

    (define (apply-svc)
      (let ((kchar (kern-ui-select-party-member)))
        (if (null? kchar)
            #f
            (if (apply (svc-proc svc) (list kchar knpc))
                (begin 
                  (take-player-gold (svc-price svc))
                  #t)))))
    
    (and (not (null? svc))
         (can-pay?)
         (apply-svc))))

;; player-out-of-sight -- no LOS between kobj and any party member
(define (player-out-of-sight? kobj)
  (define (can-see? members)
    (if (null? members)
        #f
        (or (kern-in-los? (kern-obj-get-location (car members))
                          (kern-obj-get-location kobj))
            (can-see? (cdr members)))))
  (not (can-see? (kern-party-get-members (kern-get-player)))))

(define (improve-relations kb1 kb2)
  (kern-dtable-inc (kern-being-get-current-faction kb1)
                   (kern-being-get-current-faction kb2)))

(define (harm-relations kb1 kb2)
  (kern-dtable-dec (kern-being-get-current-faction kb1)
                   (kern-being-get-current-faction kb2)))

(define (make-enemies kb1 kb2)
  (harm-relations kb1 kb2)
  (harm-relations kb1 kb2)
  (harm-relations kb1 kb2)
  (harm-relations kb1 kb2)
  )

(define (make-allies kb1 kb2)
  (improve-relations kb1 kb2)
  (improve-relations kb1 kb2)
  (improve-relations kb1 kb2)
  (improve-relations kb1 kb2)
  )

(define (is-bad-terrain-at? loc)
  (is-bad-terrain? (kern-place-get-terrain loc)))

;; put-random-stuff -- randomly generate locations within the given rectangle
;; and, if pred? is satisfied, pass the loc to ctor.
(define (put-random-stuff place rect pred? ctor n)
  (if (> n 0)
      (let ((loc (cons place (rect-random rect))))
        (if (pred? loc)
            (begin
              (ctor loc)
              (put-random-stuff place rect pred? ctor (- n 1)))
            (put-random-stuff place rect pred? ctor n)))))

(define (drop-random-corpses kplace n)
  (put-random-stuff kplace
                    (mk-rect 0 0 
                             (kern-place-get-width kplace) 
                             (kern-place-get-height kplace))
                    (lambda (loc)
                      (eqv? (kern-place-get-terrain loc)
                            t_grass))
                    (lambda (loc)
                      (kern-obj-put-at (mk-corpse-with-loot)
                                       loc))
                    n))
                    
(define (webify kplace x y w h)
  (define (drop-web x loc)
    (let ((kter (kern-place-get-terrain loc)))
      (if (or (eqv? kter t_grass)
              (eqv? kter t_boulder))
          (kern-obj-put-at (kern-mk-obj F_web_perm 1)
                           loc))))
  (foldr-rect kplace x y w h drop-web nil))

;; Fill the rectangle with objects of the given type. If pred? is not null use
;; it to filter out unsuitable locations.
(define (rect-fill-with-npc kplace x y w h npct pred?)
  (define (drop-obj x loc)
    (if (or (null? pred?)
            (pred? loc))
        (kern-obj-put-at (kern-mk-obj ktype 1)
                         loc)))
(foldr-rect kplace x y w h drop-obj #f))

;; on-entry-to-dungeon-room -- generic place on-enty procedure for dungeon
;; rooms. When the player enters (or re-enters) a dungeon this looks for a
;; monster manager object and triggers it.
(define (on-entry-to-dungeon-room kplace kplayer)
  ;;(println "on-entry-to-dungeon-room")
  (map (lambda (kmm)
         ;;(println " signal")
         (signal-kobj kmm 'on kmm nil))
       (kplace-get-objects-of-type kplace t_monman))
  )
       
;; trigger anything with an 'on-entry' ifc
(define (on-entry-trigger-all kplace kplayer)
  (map (lambda (kobj)
         (signal-kobj kobj 'on-entry kobj))
       (kern-place-get-objects kplace))
  )
     
  
;; mk-dungeon-room -- make a 19x19 dungeon room (simplified form of
;; kern-mk-place)
(define (mk-dungeon-room tag name terrain . objects)
  (kern-mk-place tag
                 name
                 nil     ; sprite
                 (kern-mk-map nil 19 19 pal_expanded terrain)
                 #f      ; wraps
                 #t      ; underground
                 #f      ; large-scale (wilderness)
                 #f      ; tmp combat place
                 nil     ; subplaces
                 nil     ; neighbors

                 ;; objects -- automatically add a monster manager
                 (cons (put (mk-monman) 0 0)
                       objects)
                 (list 'on-entry-to-dungeon-room) ; hooks
                 nil     ; edge entrances
                 ))

(define (mk-combat-map tag . terrain)
  (kern-mk-map tag 19 19 pal_expanded terrain))

(define (mk-tower tag name terrain entrances . objects)
  (kern-mk-place tag
                 name
                 s_keep     ; sprite
                 (kern-mk-map nil 19 19 pal_expanded terrain)
                 #f      ; wraps
                 #f      ; underground
                 #f      ; large-scale (wilderness)
                 #f      ; tmp combat place
                 nil     ; subplaces
                 nil     ; neighbors

                 ;; objects -- automatically add a monster manager
                 (cons (put (mk-monman) 0 0)
                       objects)
                 (list 'on-entry-to-dungeon-room) ; hooks
                 entrances     ; edge entrances
                 ))

;; Just like mk-tower but make the sprite configurable
(define (mk-19x19-town tag name sprite terrain entrances . objects)
  (kern-mk-place tag
                 name
                 sprite
                 (kern-mk-map nil 19 19 pal_expanded terrain)
                 #f      ; wraps
                 #f      ; underground
                 #f      ; large-scale (wilderness)
                 #f      ; tmp combat place
                 nil     ; subplaces
                 nil     ; neighbors
                 ;; objects -- automatically add a monster manager
                 (cons (put (mk-monman) 0 0) objects)
                 (list 'on-entry-to-dungeon-room 'on-entry-trigger-all) ; hooks
                 entrances     ; edge entrances
                 ))


;; mk-dungeon-level -- given a 2d list of rooms, connect them up as neighbors
(define (mk-dungeon-level . rooms)
  (define (bind-dir r1 r2 dir)
    (if (and (not (null? r1))
             (not (null? r2)))
        (kern-place-set-neighbor dir r1 r2)))
  (define (bind-row top bot)
    (if (not (null? top))
        (begin
          (if (not (null? (cdr top)))
              (bind-dir (car top) (cadr top) east))
          (if (null? bot)
              (bind-row (cdr top) nil)
              (begin
                (bind-dir (car top) (car bot) south)
                (if (not (null? (cdr bot))) 
                    (bind-dir (car top) (cadr bot) southeast))
                (if (not (null? (cdr top))) 
                    (bind-dir (cadr top) (car bot) southwest))
                (bind-row (cdr top) (cdr bot)))))))
  (define (bind-rooms rooms)
    (if (not (null? rooms))
        (begin
          (bind-row (car rooms) 
                    (if (null? (cdr rooms))
                        nil
                        (cadr rooms)))
          (bind-rooms (cdr rooms)))))
  (bind-rooms rooms))


(define (println . args)
  (map display args)
  (newline))


(define (is-bad-field-at? kchar loc)
  (define (is-bad-field? val ktype)
    (or val
        (and (is-field? ktype)
             (not (is-immune-to-field? kchar ktype)))))
  (foldr is-bad-field?
         #f
         (kern-get-objects-at loc)))

(define (is-bad-loc? kchar loc)
  (or
   (is-bad-terrain-at? loc)
   (is-bad-field-at? kchar loc)
   ))

(define (is-good-loc? kchar loc)
  ;;(println "is-good-loc?")
  (and (passable? loc kchar)
       (not (occupied? loc))
       (not (is-bad-loc? kchar loc))))

(define (get-off-bad-tile? kchar)
  ;;(println "get-off-bad-tile")
  
  (define (choose-good-tile tiles)
    ;;(display "choose-good-tile")(newline)
    (if (null? tiles)
        nil
        (if (is-good-loc? kchar (car tiles))
            (car tiles)
            (choose-good-tile (cdr tiles)))))

  (define (move-to-good-tile)
    ;;(display "move-to-good-tile")(newline)
    (let* ((curloc (kern-obj-get-location kchar))
           (tiles (get-4-neighboring-tiles curloc))
           (newloc (choose-good-tile tiles)))
      (if (null? newloc)
          #f
          (begin
            ;;(display "moving")(newline)
            (kern-obj-move kchar 
                           (- (loc-x newloc) (loc-x curloc))
                           (- (loc-y newloc) (loc-y curloc)))
            #t))))

  (and
   (is-bad-loc? kchar (kern-obj-get-location kchar))
   (move-to-good-tile)))

(define (move-away-from-foes? kchar)
  ;;(println "move-away-from-foes?")
  (evade kchar (all-visible-hostiles kchar)))

;; random-loc -- choose a random location
(define (random-loc kplace x y w h)
  (mk-loc kplace 
          (+ x (modulo (random-next) w))
          (+ y (modulo (random-next) h))))

;; random-loc -- choose a random location anywhere in the given place
(define (random-loc-in-place kplace)
  (random-loc kplace
              0
              0
              (kern-place-get-width kplace)
              (kern-place-get-height kplace)))

;; random-loc-place-iter -- try up to n times to find a random location which
;; satisfies pred?
(define (random-loc-place-iter kplace pred? n)
  (if (<= n 0)
      nil
      (let ((loc (random-loc-in-place kplace)))
        (if (pred? loc)
            loc
            (random-loc-place-iter kplace pred? (- n 1))))))

(define (is-floor? loc)
  (let ((kter (kern-place-get-terrain loc)))
    (or (eqv? kter t_flagstones)
        (eqv? kter t_cobblestone))))

(define (loc-is-empty? loc)
  (null? (kern-get-objects-at loc)))

(define (mean-player-party-level)
  (let ((members (kern-party-get-members (kern-get-player))))
    (if (= 0 (length members))
        1
        (/ (foldr (lambda (sum kchar)
                    ;;(println "level:" (kern-char-get-level kchar))
                    (+ sum (kern-char-get-level kchar)))
                  0
                  members)
           (length members)))))

(define (calc-level)
  (max 1
       (+ (mean-player-party-level)
          (num-player-party-members)
          (kern-dice-roll "1d5-3"))))

(define (get-mech-at loc)
  (let ((mechs (filter kern-obj-is-mech?
                       (kern-get-objects-at loc))))
    (if (null? mechs)
        nil
        (car mechs))))

(define (handle-mech-at loc kchar)
  (let ((kmech (get-mech-at loc)))
    (if (null? kmech)
        #f
        (signal-kobj kmech 'handle kmech kchar))))

(define (get-place kobj)
  (loc-place (kern-obj-get-location kobj)))

;; xp to reach the given level
(define (power base exp)
  (if (= 0 exp)
      1
      (* base (power base (- exp 1)))))

(define (lvl-xp lvl)
  (power 2 (+ 5 lvl)))

(define (random-faction)
  (modulo (random-next) faction-num))

(define (get-target-loc caster range)
  (kern-ui-target (kern-obj-get-location caster)
                  range))

;;----------------------------------------------------------------------------
;; code for opening a moongate, warping in a monster, and re-closing it
(define (open-moongate loc)
  (let ((kgate (mk-moongate nil)))
    (kern-obj-relocate kgate loc nil)
    (moongate-animate kgate moongate-stages)
    kgate))
(define (close-moongate kgate)
  (moongate-animate kgate (reverse moongate-stages))
  (moongate-destroy kgate))
(define (warp-in kchar loc dir faction)
  (display "warp-in")(newline)
  (kern-char-set-schedule kchar nil)
  (kern-obj-inc-ref kchar)
  (kern-obj-remove kchar)
  (kern-obj-relocate kchar loc nil)
  (kern-obj-dec-ref kchar)
  (kern-map-repaint)
  (kern-sleep 250)
  (kern-obj-relocate kchar (loc-offset loc dir) nil)
  (kern-being-set-base-faction kchar faction)
  (kern-map-repaint))

;;-----------------------------------------------------------------------------
;; re-mk-composite-sprite -- combine all the sprites into one layered sprite,
;; cloning ALL BUT the first sprite. Useful for re-decorating base sprites that
;; have already been cloned.
(define (re-mk-composite-sprite sprites)
  (foldr (lambda (s1 s2) (kern-sprite-append-decoration s1 s2))
         (car sprites)
         (cdr sprites)))

;;-----------------------------------------------------------------------------
;; mk-composite-sprite -- combine all the sprites into one composite sprite,
;; cloning all the sprites in the list.
(define (mk-composite-sprite sprites)
  (re-mk-composite-sprite (cons (kern-sprite-clone (car sprites)
                                                   nil)
                                (cdr sprites))))

;   (foldr (lambda (s1 s2) (kern-sprite-append-decoration s1 s2))
;          (kern-sprite-clone (car sprites) nil)
;          (cdr sprites)))

(define (kchar-in-vehicle? kchar)
  (let ((kparty (kern-char-get-party kchar)))
    (if (null? kparty)
        #f
        (not (null? (kern-party-get-vehicle kparty))))))

;; is-in-darkness? -- #t iff light on this object's tile is less than the
;; threshold for "dark"
(define (is-in-darkness? kobj)
  (< (kern-place-get-light (kern-obj-get-location kobj))
     64))

;; Convenience wrapper for kern-obj-add-to-inventory
(define (give kpc ktype quantity)
  (kern-obj-add-to-inventory kpc ktype quantity))

;; Convenience wrapper for kern-obj-remove-from-inventory
(define (take kobj ktype quantity)
  (kern-obj-remove-from-inventory kobj ktype quantity))

;; Return #t iff object has at least that many in inventory
(define (has? kobj ktype quantity)
  (>= (num-in-inventory kobj ktype) quantity))

;; Safely if a character is in the player party. char-tag should be the
;; character's quoted scheme variable name, for example 'ch_dude.
(define (in-player-party? kchar-tag)
  (println "in-player-party? " kchar-tag)
  (and (defined? kchar-tag)
       (let ((kchar (eval kchar-tag)))
         (and (is-alive? kchar)
              (is-player-party-member? kchar)))))

(define (set-wind-north)
  (println "set-wind-north")
  (kern-set-wind north 10))

;; block-teleporting takes a place and a list of strings that looks
;; suspiciously like a terrain map, and uses the map to apply blocking
;; mechanisms to the place. Every "x#" entry in the map will cause a blocking
;; mechanism to be placed on that location. All other entries are ignored. The
;; blocking mechanisms prevent spells like blink from letting the player break
;; the fiction of a simulated multi-story place.
(define (block-teleporting kplace map)
  (define (doline y lines)
    (define (docol x tokens)
      (cond ((null? tokens) nil)
            (else
             (if (and (char=? #\x (car tokens))
                       (char=? #\# (cadr tokens)))
                 (begin
                   (kern-obj-put-at (mk-blocker) (list kplace x y))
                 ))
             (docol (+ x 1) (cdddr tokens)))))
    (cond ((null? lines) nil)
          (else
           (docol 0 (string->list (car lines)))
           (doline (+ y 1) (cdr lines)))))
    (doline 0 map))

;; Find the visible object of the given type nearest to the kchar.
(define (find-nearest kchar ktype)
  (let ((objects (filter (lambda (kobj)
                           (and (kobj-is-type? kobj ktype)
                                (can-see? kchar kobj)))
                         (kern-place-get-objects (loc-place (kern-obj-get-location kchar))))))
    (cond ((null? objects) nil)
          (else
           (nearest-obj kchar objects)))))

;; Return an integer describing the sign of x
(define (sgn x)
  (cond ((> x 0) 1)
        ((< x 0) -1)
        (else 0)))

;; Return a list of (x . y) pairs that constitute a line between two
;; points. Uses Bresenhaum's line-drawing algorithm.
(define (line x1 y1 x2 y2)
  (let* ((dx (- x2 x1))
         (dy (- y2 y1))
         (adx (abs dx))
         (ady (abs dy))
         (sdx (sgn dx))
         (sdy (sgn dy))
         (x (/ ady 2))
         (y (/ adx 2))
         (px x1)
         (py y1))
    (define (f1 i)
      ;;(println "f1 i=" i " px=" px " py=" py)
      (cond ((>= i adx)
             nil)
            (else
             (set! y (+ y ady))
             (cond ((>= y adx)
                    (set! y (- y adx))
                    (set! py (+ py sdy))))
             (set! px (+ px sdx))
             (cons (cons px py)
                   (f1 (+ 1 i))))))
    (define (f2 i)
      ;;(println "f2 i=" i " px=" px " py=" py)
      (cond ((>= i ady)
             nil)
            (else
             (set! x (+ x adx))
             (cond ((>= x ady)
                    (set! x (- x ady))
                    (set! px (+ px sdx))))
             (set! py (+ py sdy))
             (cons (cons px py)
                   (f2 (+ 1 i))))))
    (cond ((>= adx ady)
           (cons (cons x1 y1) (f1 0)))
          (else
           (cons (cons x1 y1) (f2 0))))))

;; Utility for generating dice from numbers easily
;;
(define (mkdice dice size)
	(let ((numstr (if (number? dice)
						(number->string dice)
						dice))
			(sizestr (if (number? size)
						(number->string size)
						size)))
			(string-append numstr "d" sizestr)))

;; output for effects that should only be noted if visible

(define (msg-log-visible loc . args)
	(if (kern-place-is-visible? loc)
		(apply kern-log-msg args)
		)
	)

;; Print dots across the console (similar to the u4 shrine meditation)
(define (log-dots n delay)
  (define (dots n)
    (cond ((> n 0)
           (kern-log-continue ".")
           (kern-log-flush)
           (kern-sleep delay)
           (dots (- n 1)))))
  (kern-log-begin)
  (dots n)
  (kern-log-end)
  )

(define (find-first fn? lst)
  (if (null? lst)
      nil
      (if (fn? (car lst))
          (car lst)
          (find-first fn? (cdr lst)))))

(define (append! lst val)
  (cond ((null? lst) nil)
        ((null? (cdr lst)) (set-cdr! lst val))
        (else (append! (cdr lst) val))))

(define (repeat fn n)
  (if (> n 0)
      (begin
        (fn)
        (repeat fn (- n 1)))))

(define (string-lower str)
  (list->string (map char-downcase (string->list str))))

(define (!= a b) 
  (not (= a b)))

(define (rect-x r) (car r))
(define (rect-y r) (cadr r))
(define (rect-w r) (caddr r))
(define (rect-h r) (cadddr r))

(define (rect-down r v)
  (list (rect-x r) (+ v (rect-y r)) (rect-w r) (rect-h r)))
  
(define (rect-crop-down r v)
  (list (rect-x r) (+ v (rect-y r)) (rect-w r) (- (rect-h r) v)))
 
(define (rect-offset r x y)
  (list (+ x (rect-x r)) (+ y (rect-y r)) (rect-w r) (rect-h r)))

(define (rect-crop-offset r x y)
  (list (+ x (rect-x r)) (+ y (rect-y r)) (- (rect-w r) x) (- (rect-h r) y))) 
  
(define (1- x) (- x 1))
(define (1+ x) (+ x 1))

;; Standard dc vs 1d20 + bonus, with a perfect roll granting automatic success.
(define (check-roll dc bonus)
  (let ((roll (kern-dice-roll "1d20")))
    (or (= 20 roll)
        (> (+ roll bonus) dc))))

