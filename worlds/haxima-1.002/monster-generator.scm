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
             (player-out-of-sight? gen)
             (not (occupied? (kern-obj-get-location gen))))
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
  (display "mongen2-exec")(newline)
  (let* ((gen (kobj-gob-data kgen))
        (targ-loc (if (null? (mongen2-targ-loc gen)) 
                      (kern-obj-get-location kgen)
                      (mongen2-targ-loc gen)))
        )
    (define (roll-to-encounter)
      (display "roll-to-encounter")(newline)
      (>= (modulo (random-next) 1000) (mongen2-thresh gen)))
    (define (not-too-many?)
      (display "not-too-many?")(newline)
      (< (length (filter (eval (caddr gen))
                         (kern-place-get-beings (loc-place 
                                                 (kern-obj-get-location 
                                                  kgen)))))
         (mongen2-max gen)))
    (if (and (roll-to-encounter)
             (not-too-many?)
             (not (occupied? targ-loc))
             (or (not (mongen2-out-of-sight-only? gen))
                 (player-out-of-sight? kgen)))
        (begin
          (display "targ-loc:")(display targ-loc)(newline)
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
             (player-out-of-sight? kgen)
             (not (occupied? (kern-obj-get-location kgen))))
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

;;----------------------------------------------------------------------------
;; Random treasure drops
;;----------------------------------------------------------------------------
(define (treasure-prob tr) (car tr))
(define (treasure-type tr) (cadr tr))
(define (treasure-quan tr) (caddr tr))

(define treasure-list
  (list
   (list 32 t_gold_coins 20)
   (list 32 t_arrow 10)
   (list 32 t_bolt 5)
   (list 32 t_food 5)
   (list 8  t_heal_potion 1)
   (list 8  t_mana_potion 1)
   (list 4  t_cure_potion 1)
   (list 2  t_leather_helm 1)
   (list 16 t_dagger 1)
   (list 2  t_sword 1)
   (list 2  t_iron_helm 1)
   (list 2  t_shield 1)
   (list 2  t_armor_leather 1)
   (list 1  t_shovel 1)
   (list 4  t_torch 3)
   (list 2  t_gem 3)
   (list 4  t_picklock 3)
   ))

(define treasure-modulus
  (foldr (lambda (n entry) (+ n (car entry)))
         0
         treasure-list))

(define (treasure-lookup index)
  (define (search n list)
    (if (null? list)
        (error "treasure-lookup not found")
        (let ((next (+ n (treasure-prob (car list)))))
          (if (<= index next)
              (car list)
              (search next (cdr list))))))
  (search 0 treasure-list))
    
;; pick-random-treasure -- returns a (quantity type) list
(define (pick-random-treasure)
  (let ((trsr (treasure-lookup (modulo (random-next) treasure-modulus))))
    (list (+ 1 (modulo (random-next) (treasure-quan trsr)))
          (treasure-type trsr))))


(define (mk-random-treasure)
  (let ((pair (pick-random-treasure)))
    (kern-mk-obj (car pair)
                 (+ 1 (modulo (random-next) (cadr pair))))))
    

;; creates a list of objects
(define (mk-treasure-heap n)
  (if (> n 0)
      (cons (mk-random-treasure)
            (mk-treasure-heap (- n 1)))))

;; mk-treasure-list -- returns a list of n (quantity type) lists
(define (mk-treasure-list n)
  (if (> n 0)
      (cons (pick-random-treasure)
            (mk-treasure-list (- n 1)))))

;;----------------------------------------------------------------------------
;; spawn-pt -- generates a monster when triggered externally. The level of the
;; monsters is calculated on-the-fly based on the player party level. The
;; faction and npc type mix are determined by the "factory", which is passed to
;; the spawn-pt constructor.
(define (spawn-pt-mk npct-tag)
  (list 'spawn-pt npct-tag))
(define (spawn-pt-npct-tag sppt) (cadr sppt))

(define (spawn-pt-exec ksppt)
  (println "spawn-pt-exec")
  (let* ((sppt (gob ksppt))
         )
    (println " " sppt)

    (define (mean-player-party-level)
      (let ((members (kern-party-get-members (kern-get-player))))
        (/ (foldr (lambda (sum kchar)
                    (+ sum (kern-char-get-level kchar)))
                  0
                  members)
           (length members))))

    (define (calc-level)
      (max 0
           (+ (mean-player-party-level)
              (kern-dice-roll "1d5-3"))))

    (let ((npc (spawn-npc (spawn-pt-npct-tag sppt) 
                          (calc-level))))
      (kern-obj-put-at npc
                       (kern-obj-get-location ksppt))
      (println " done!")
      npc)
    ))

(define spawn-pt-ifc
  (ifc nil
       (method 'on spawn-pt-exec)))

(mk-obj-type 't_spawn_pt nil nil layer-none spawn-pt-ifc)

(define (spawn-pt npct-tag)
  (bind (kern-obj-set-visible (kern-mk-obj t_spawn_pt 1) #f)
        (spawn-pt-mk npct-tag)))

;;----------------------------------------------------------------------------
;; guard-pt -- a spawn pt which creates an npc with a guard post
(define (guard-pt-exec kgen)
  (println "guard-pt-exec")
  (let ((kchar (spawn-pt-exec kgen)))
    (npcg-set-post! (gob kchar)
                    (cdr (kern-obj-get-location kgen)))
    kchar))

(define guard-pt-ifc
  (ifc nil
       (method 'on guard-pt-exec)))

(mk-obj-type 't_guard_pt nil nil layer-none guard-pt-ifc)

(define (guard-pt npct-tag)
  (bind (kern-obj-set-visible (kern-mk-obj t_guard_pt 1) #f)
        (spawn-pt-mk npct-tag)))

;;----------------------------------------------------------------------------
;; custom-pt -- a generic 'on trigger which is run by the respawn manager
(define custom-pt-ifc
  (ifc nil
       (method 'on on-trig-exec)))

(mk-obj-type 't_custom_pt nil nil layer-none custom-pt-ifc)

(define (custom-pt proc-tag . args)
  (bind (kern-obj-set-visible (kern-mk-obj t_custom_pt 1) #f)
        (trig-mk proc-tag args)))

;;----------------------------------------------------------------------------
;; time-to-respawn? -- checks if an hour and a minute has passed
(define (time-to-respawn? oldtime)
  (let ((curtime (kern-get-time)))
    (println "time-to-respawn? " curtime " a day past " oldtime "?")
    (or (> (time-year curtime) (time-year oldtime))
        (> (time-month curtime) (time-month oldtime))
        (> (time-week curtime) (time-week oldtime))
        (>= (- (time-day curtime) (time-day oldtime)) 2)
        (and (> (time-day curtime) (time-day oldtime))
             (>= (time-hour curtime) (time-hour oldtime))))))

;;----------------------------------------------------------------------------
;; monman -- monster manager object

(define (monman-mk time)
  (list 'monman time))
(define (monman-time mm) (cadr mm))
(define (monman-set-time! mm val) (set-car! (cdr mm) val))

(define (monman-exec kmm)

  (println "monman-exec")

  (let ((mm (gob kmm))
        (kplace (loc-place (kern-obj-get-location kmm))))

    (println " mm=" mm)
    
    (define (cleanup-old-spawn)
      (println " cleanup-old-spawn")
      (map kern-obj-remove
           (filter kbeing-was-spawned?
                   (kern-place-get-beings kplace))))

    (define (trigger-spawn-pt sppt)
      (println " trigger-spawn-pt")
      (signal-kobj sppt 'on sppt nil))

    (define (respawn)
      (println " respawn")
      (monman-set-time! mm (kern-get-time))
      (map trigger-spawn-pt
           (kplace-get-objects-of-type kplace t_spawn_pt))
      (map trigger-spawn-pt
           (kplace-get-objects-of-type kplace t_guard_pt))
      (map trigger-spawn-pt
           (kplace-get-objects-of-type kplace t_custom_pt))
      )

    (if (time-to-respawn? (monman-time mm))
        (and (cleanup-old-spawn)
             (respawn)))

  ))

(define monman-ifc
  (ifc nil
       (method 'on monman-exec)))

(mk-obj-type 't_monman nil nil layer-none monman-ifc)

(define (mk-monman)
  (bind (kern-obj-set-visible (kern-mk-obj t_monman 1) #f)
        (monman-mk (map - game-start-time
                        (time-mk 0 0 0 1 1 1)))))
