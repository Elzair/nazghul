;; !!! OBSOLETE !!!
;; Note: if you try to create this interface on-the-fly, as in the commented
;; out part below, then you will eventually get a runtime crash. The reason is
;; that the procedure created by the lambda in mk-gen-ifc will not be referred
;; to by any environment variable, so the gc will deallocate it.
(define goblin-gen-ifc 
  (ifc '()
       (method 'exec (lambda (kobj)
                       (if (>= (modulo (random-next) 100) 99)
                           (kern-obj-put-at (kern-mk-party t_goblin_horde
                                                           faction-monster
                                                           '())
                                            (kern-obj-get-location kobj)))))))


;; A monster generator
(mk-obj-type 't_goblin_generator "goblin generator" nil layer-none 
             goblin-gen-ifc)


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
;; mk-wilderness-ambush-ifc -- make an interface for a monster ambush generator
;; in the wilderness
;; ----------------------------------------------------------------------------
(define (mk-wilderness-ambush-generator-ifc threshold max party faction) 
  (define (generate gen)
    (if (>= (modulo (random-next) 1000) threshold)
        (begin
          (kern-log-msg "*** AMBUSH ***")
          (kern-begin-combat (kern-obj-get-location gen)
                             (kern-mk-party party faction nil)))))
  (ifc '() 
       (method 'exec generate)))

;; ----------------------------------------------------------------------------
;; mk-wilderness-monster-generator -- make an object type for spawning random
;; encounters
;; ----------------------------------------------------------------------------
(define (mk-wilderness-monster-generator tag threshold max party faction 
                                         vehicle)
  (mk-obj-type tag                                  ;; tag
               "monster generator"                  ;; name
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
(define (mk-wilderness-ambush-generator tag threshold max party faction)
  (mk-obj-type tag                ;; tag
               "ambush generator" ;; name
               nil                ;; sprite
               layer-none         ;; layer
               (mk-wilderness-ambush-generator-ifc threshold  ;; ifc
                                                   max
                                                   party
                                                   faction)))

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

(mk-wilderness-ambush-generator 't_spider_generator
                                 990
                                 2
                                 t_wood_spiders
                                 faction-wood-spider nil)

(mk-wilderness-ambush-generator 't_queen_spider_generator
                                 999
                                 1
                                 t_queen_wood_spiders
                                 faction-wood-spider nil)

(mk-wilderness-monster-generator 't_skeleton_generator
                                 990
                                 2
                                 t_skeleton_brigade
                                 faction-monster nil)

;; ----------------------------------------------------------------------------
;; A monster type is a convenient collection of all the attributes needed to
;; create an instance of a stock monster.
;; ----------------------------------------------------------------------------
(define (mk-monster-type species occupation sprite name faction ai)
  (list species occupation sprite name faction ai))

;; ----------------------------------------------------------------------------
;; Given one of our monster types, create an instance. Note the trick here with
;; apply: a monster-type is a list of exactly the args needed for the
;; kern-mk-stock-char api call.
;; ----------------------------------------------------------------------------
(define (mk-monster type)
  (apply kern-mk-stock-char type))
