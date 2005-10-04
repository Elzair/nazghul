(define (mk-ambush-gob x y w h msg) (list x y w h msg))
(define (ambush-x gob) (list-ref gob 0))
(define (ambush-y gob) (list-ref gob 1))
(define (ambush-w gob) (list-ref gob 2))
(define (ambush-h gob) (list-ref gob 3))
(define (ambush-msg gob) (list-ref gob 4))

;; ----------------------------------------------------------------------------
;; mk-wilderness-monster-generator-ifc -- make an interface for a monster
;; generator in the wilderness
;; ----------------------------------------------------------------------------
(define (mk-wilderness-monster-generator-ifc threshold max party faction 
                                             vehicle)
  (define (roll-to-encounter)
    (>= (modulo (random-next) 1000) threshold))
  (define (not-too-many kobj)
    (< (length (filter (lambda (a) (eqv? (kern-obj-get-type a) party))
                       (kern-place-get-beings (loc-place 
                                               (kern-obj-get-location kobj)))))
       max))
  (define (generate gen)
    (if (and (roll-to-encounter)
             (not-too-many gen))
        (kern-obj-put-at (kern-mk-party party
                                        faction
                                        vehicle)
                         (kern-obj-get-location gen))))
  (ifc '() 
       (method 'exec generate)))

;; ----------------------------------------------------------------------------
;; mk-wilderness-ambush-generator-ifc - construct an interface with an 'exec
;; handler that will roll to ambush the player. If the roll succeeds the
;; handler will then check if the player is in the designated region and, if
;; so, create a wilderness combat with the specified npc party of the
;; given faction.
;; ----------------------------------------------------------------------------
(define (mk-wilderness-ambush-generator-ifc threshold max party faction)
  (define (roll-to-encounter)
    (>= (modulo (random-next) 1000) threshold))
  (define (player-in-rect? kgen)
    (let ((loc (kern-obj-get-location (kern-get-player)))
          (gob (gob-data (kobj-gob kgen))))
      ;(display "gob:")(display gob)(newline)
      (and (>= (loc-x loc) (ambush-x gob))
           (<  (loc-x loc) (+ (ambush-x gob) (ambush-w gob)))
           (>= (loc-y loc) (ambush-y gob))
           (<  (loc-y loc) (+ (ambush-y gob) (ambush-h gob))))))
  (define (generate kgen)
    (if (and (roll-to-encounter)
             (player-in-rect? kgen))
        (begin
          (kern-log-msg (ambush-msg (gob-data (kobj-gob kgen))))
          (kern-begin-combat (kern-obj-get-location (kern-get-player))
                             (kern-mk-party party 
                                            faction 
                                            nil)))))
  (ifc '() 
       (method 'exec generate)))

;; ----------------------------------------------------------------------------
;; mk-monster-generator-ifc -- make an interface for a monster generator in a
;; town or dungeon
;; ----------------------------------------------------------------------------
(define (mk-monster-generator-ifc threshold max mk-monster is-monster?)
  (define (roll-to-encounter)
    (>= (modulo (random-next) 1000) threshold))
  (define (not-too-many kobj)
    (< (length (filter is-monster?
                       (kern-place-get-beings (loc-place 
                                               (kern-obj-get-location kobj)))))
       max))
  (define (player-out-of-sight? gen)
    (define (can-see? members)
      (if (null? members)
          #f
          (or (kern-in-los? (kern-obj-get-location (car members))
                            (kern-obj-get-location gen))
              (can-see? (cdr members)))))
    (not (can-see? (kern-party-get-members (kern-get-player)))))
  (define (generate gen)
    (if (and (roll-to-encounter)
             (not-too-many gen)
             (player-out-of-sight? gen))
        (kern-obj-put-at (mk-monster)
                         (kern-obj-get-location gen))))
  (ifc '() 
       (method 'exec generate)))


;; ----------------------------------------------------------------------------
;; mk-wilderness-monster-generator -- make an object type for spawning random
;; encounters
;; ----------------------------------------------------------------------------
(define (mk-wilderness-monster-generator tag threshold max party faction 
                                         vehicle)
  (mk-obj-type tag                                  ;; tag
               nil                                  ;; name
               nil                                  ;; sprite
               layer-none                           ;; layer
               (mk-wilderness-monster-generator-ifc threshold  ;; ifc
                                                    max
                                                    party
                                                    faction
                                                    vehicle)))

;; ----------------------------------------------------------------------------
;; mk-wilderness-ambush-generator -- make an object type for spawning random
;; ambush encounters
;; ----------------------------------------------------------------------------
(define (mk-wilderness-ambush-generator-type tag threshold party faction)
  (mk-obj-type tag                ;; tag
               nil                ;; name
               nil                ;; sprite
               layer-none         ;; layer
               (mk-wilderness-ambush-generator-ifc threshold  ;; ifc
                                                   max
                                                   party
                                                   faction)))

;; ----------------------------------------------------------------------------
;; mk-wilderness-ambush-generator -- make an instance of a wilderness ambush
;; generator type which monitors the given rectangle
;; ----------------------------------------------------------------------------
(define (mk-wilderness-ambush-generator type x y w h msg)
  (bind (kern-obj-set-visible (kern-mk-obj type 1) #f)
        (mk-ambush-gob x y w h msg)))

;;----------------------------------------------------------------------------
;; mk-monster-generator -- make an instance of a town or dungeon monster
;; generator
;;----------------------------------------------------------------------------
(define (mk-monster-generator tag threshold max mk-monster is-monster?)
  (mk-obj-type tag                                  ;; tag
               nil                                  ;; name
               nil                                  ;; sprite
               layer-none                           ;; layer
               (mk-monster-generator-ifc threshold  ;; ifc
                                         max
                                         mk-monster
                                         is-monster?)))

;; ----------------------------------------------------------------------------
;; Monster Generators
;;
;; Some common monster generator types defined here for convenience. Keep in
;; mind that these are very generic generators, and there's nothing preventing
;; you from making "smarter" custom generators suited to a particular place.
;; ----------------------------------------------------------------------------
(mk-wilderness-monster-generator 't_orc_generator 
                                 990
                                 2
                                 t_goblin_horde 
                                 faction-monster nil)

(mk-wilderness-ambush-generator-type 't_spider_generator
                                     950
                                     t_wood_spiders
                                     faction-wood-spider nil)

(mk-wilderness-ambush-generator-type 't_queen_spider_generator
                                     999
                                     t_queen_wood_spiders
                                     faction-wood-spider nil)

(mk-wilderness-monster-generator 't_skeleton_generator
                                 990
                                 2
                                 t_skeleton_brigade
                                 faction-monster nil)

(mk-wilderness-monster-generator 't_bandit_generator
                                 990
                                 1
                                 t_bandit_gang
                                 faction-outlaw
                                 nil)

(mk-monster-generator 't_dungeon_troll_generator 990 5 mk-troll is-troll?)
(mk-monster-generator 't_dungeon_spider_generator 990 3 mk-queen-spider 
                      is-queen-spider?)
(mk-monster-generator 't_yellow_slime_generator 500 1 mk-yellow-slime
                      is-yellow-slime?)

;; Make an instance of one of the above monster generators
(define (mk-generator generator-type)
  (kern-obj-set-visible (kern-mk-obj generator-type 1) #f))

;;----------------------------------------------------------------------------
;; Newer, improveder monster generator
;;----------------------------------------------------------------------------
(define (mongen2-mk thresh max is-monster-tag mk-monster-tag mk-args
                    out-of-sight-only? targ-loc)
  (list thresh max is-monster-tag mk-monster-tag mk-args out-of-sight-only?
        targ-loc))
(define (mongen2-thresh gen) (car gen))
(define (mongen2-max gen) (cadr gen))
(define (mongen2-mk-monster gen)
  (apply (eval (cadddr gen)) (list-ref gen 4)))
(define (mongen2-out-of-sight-only? gen) (list-ref gen 5))
(define (mongen2-targ-loc gen) 
  (let ((tag-loc (list-ref gen 6)))
    (if (null? tag-loc)
        nil
        (eval-loc tag-loc))))

(define (mongen2-exec kgen)
  (let ((gen (kobj-gob-data kgen)))
    (define (roll-to-encounter)
      (>= (modulo (random-next) 1000) (mongen2-thresh gen)))
    (define (not-too-many?)
      (< (length (filter (eval (caddr gen))
                         (kern-place-get-beings (loc-place 
                                                 (kern-obj-get-location 
                                                  kgen)))))
         (mongen2-max gen)))
    (if (and (roll-to-encounter)
             (not-too-many?)
             (or (not (mongen2-out-of-sight-only? gen))
                 (player-out-of-sight? kgen)))
        (let ((targ-loc (mongen2-targ-loc gen)))
          (kern-obj-put-at (mongen2-mk-monster gen)
                           (if (null? targ-loc)
                               (kern-obj-get-location kgen)
                               targ-loc))))))

(define mongen2-ifc
  (ifc nil
       (method 'exec mongen2-exec)))

(mk-obj-type 't_mongen2 nil nil layer-none mongen2-ifc)

(define (mk-mongen2 thresh max is-monster? mk-monster mk-args)
  (bind (kern-obj-set-visible (kern-mk-obj t_mongen2 1) #f)
        (mongen2-mk thresh max is-monster? mk-monster mk-args #t nil)))

;; same, only doesn't care if player is in sight
(define (mk-edge-gen thresh max is-monster? mk-monster mk-args)
  (bind (kern-obj-set-visible (kern-mk-obj t_mongen2 1) #f)
        (mongen2-mk thresh max is-monster? mk-monster mk-args #f nil)))

;; same, only triggers when player steps on it and doesn't care if player is in
;; sight
(define step-gen-ifc
  (ifc nil
       (method 'step mongen2-exec)))

(mk-obj-type 't_step_gen nil nil layer-mechanism step-gen-ifc)

(define (mk-step-gen thresh max is-monster? mk-monster mk-args targ-loc)
  (bind (kern-obj-set-visible (kern-mk-obj t_step_gen 1) #f)
        (mongen2-mk thresh max is-monster? mk-monster mk-args #f targ-loc)))

;;----------------------------------------------------------------------------
;; Guard Generator
;;
;; Monitors a list of posts (x y) and guards. If a post is empty it creates
;; a new guard and assigns it to that post.
;;----------------------------------------------------------------------------
(define (ggen-mk freq is-guard-tag? mk-guard-tag posts)
  (list freq is-guard-tag? mk-guard-tag posts))
(define (ggen-freq ggen) (car ggen))
(define (ggen-get-is-guard-tag ggen) (cadr ggen))
(define (ggen-get-mk-guard-tag ggen) (caddr ggen))
(define (ggen-posts ggen) (cadddr ggen))

(define (ggen-exec kgen)

  ;;(display "ggen-exec")(newline)

  (let ((ggen (kobj-gob-data kgen)))

    (define (time-to-check?)
      ;;(display "time-to-check")(newline)
      (< (modulo (random-next)
                 100)
         (ggen-freq ggen)))

    (define (fill-empty-posts)
      ;;(display "fill-empty-posts")(newline)
      (let ((guards (filter (eval (ggen-get-is-guard-tag ggen))
                            (kern-place-get-beings (loc-place 
                                                    (kern-obj-get-location 
                                                     kgen))))))
        ;;(display "guard:")(display guards)(newline)

        (define (post-filled? post)
          ;;(display "post-filled?:")(display post)(newline)
          (foldr (lambda (a kguard) 
                   (or a
                       (equal? post
                               (guard-post (kobj-gob-data kguard)))))
                 #f
                 guards))

        (define (fill-post post)
          ;;(display "fill-post:")(display post)(newline)
          (let ((kguard (post-guard (apply (eval (ggen-get-mk-guard-tag ggen))
                                           nil)
                                    (car post)
                                    (cadr post)))
                (loc (kern-obj-get-location kgen)))
            (kern-obj-put-at kguard loc)))

        (map (lambda (post)
               (if (not (post-filled? post))
                   (fill-post post)))
             (ggen-posts ggen))))

    (if (and (time-to-check?)
             (player-out-of-sight? kgen))
        (fill-empty-posts))))

(define ggen-ifc
  (ifc nil
       (method 'exec ggen-exec)))

(mk-obj-type 't_ggen nil nil layer-none ggen-ifc)

(define (mk-post x y) (list x y))

(define (mk-ggen freq is-guard? mk-guard posts)
  (bind (kern-obj-set-visible (kern-mk-obj t_ggen 1) #f)
        (ggen-mk freq is-guard? mk-guard posts)))

;;----------------------------------------------------------------------------
;; Special generator which responds to the 'raise signal generated by a "Vas 
;; Uus Ylem" spell invocation on a wilderness location
;;----------------------------------------------------------------------------
(define (raise-mk proc-tag args) (list proc-tag args))
(define (raise-proc-tag raise) (car raise))
(define (raise-args raise) (cadr raise))

(define (raise-exec kraise)
  (display "raise-exec")(newline)
  (let ((raise (kobj-gob-data kraise)))
    (apply (eval (raise-proc-tag raise)) (raise-args raise))
    (kern-obj-remove kraise)))

(define raise-ifc
  (ifc nil
       (method 'raise raise-exec)))

(mk-obj-type 't_raise_listener nil nil layer-none raise-ifc)

(define (can-raise-vessel? kobj)
  (eqv? (kern-obj-get-type kobj)
        t_raise_listener))

(define (mk-raise-listener proc-tag args)
  (bind (kern-mk-obj t_raise_listener 1)
        (raise-mk proc-tag args)))
