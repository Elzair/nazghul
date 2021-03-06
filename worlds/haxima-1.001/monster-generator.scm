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
      (display "gob:")(display gob)(newline)
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

(mk-monster-generator 't_dungeon_troll_generator 990 5 mk-troll char-is-troll?)
(mk-monster-generator 't_dungeon_spider_generator 990 3 mk-queen-spider 
                      is-queen-spider?)

(define (mk-generator type)
  (kern-obj-set-visible (kern-mk-obj type 1) #f))
